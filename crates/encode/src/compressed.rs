use ribit_core::instruction::{self, Instruction};
use ribit_core::{Width, opcode, register};

use crate::{EncodeError, encode_register, signed_truncate, signed_truncate_32};

#[derive(Copy, Clone)]
#[repr(u8)]
enum Quadrant {
    C0 = 0,
    C1 = 1,
    C2 = 2, // C3 is 32 bit instructions
}

#[inline]
const fn ci(quad: Quadrant, func3: u8, imm6: u16, rd_rs1: Option<register::RiscV>) -> u16 {
    let quad = quad as u16;
    let func3 = (func3 as u16) << 13;

    let rd_rs1 = (encode_register(rd_rs1) as u16) << 7;

    func3 | (imm6 & 0b0001_0000_0111_1100) | rd_rs1 | quad
}

// "cb" instructions that are actually i-type
fn cbi(
    quad: Quadrant,
    func3: u8,
    func2: Quadrant,
    imm6: u16,
    rd_rs1_prime: Option<register::RiscV>,
) -> Result<u16, EncodeError> {
    let quad = quad as u16;
    let func3 = u16::from(func3) << 13;
    let func2 = (func2 as u16) << 10;

    let imm6 = imm6 & 0b11_1111;
    let imm = ((imm6 & 0b01_1111) << 2) | ((imm6 & 0b10_0000) << 7);

    let rd_rs1 = u16::from(encode_short_reg(rd_rs1_prime)?);
    let rd_rs1 = rd_rs1 << 7;

    Ok(func3 | func2 | imm | rd_rs1 | quad)
}

#[inline]
const fn cr(
    quad: Quadrant,
    func4: u8,
    rd_rs1: Option<register::RiscV>,
    rs2: Option<register::RiscV>,
) -> u16 {
    let quad = quad as u16;
    let func4 = (func4 as u16) << 12;

    let rd_rs1 = (encode_register(rd_rs1) as u16) << 7;
    let rs2 = (encode_register(rs2) as u16) << 2;

    func4 | rd_rs1 | rs2 | quad
}

#[inline]
fn ca(
    quad: Quadrant,
    func6: u8,
    func2: u8,
    rd_rs1_prime: Option<register::RiscV>,
    rs2_prime: Option<register::RiscV>,
) -> Result<u16, EncodeError> {
    let quad = quad as u16;
    let func6 = u16::from(func6) << 10;
    let func2 = u16::from(func2) << 5;

    let rd_rs1 = u16::from(encode_short_reg(rd_rs1_prime)?);
    let rd_rs1 = rd_rs1 << 7;

    let rs2 = u16::from(encode_short_reg(rs2_prime)?);
    let rs2 = rs2 << 2;

    Ok(func6 | rd_rs1 | func2 | rs2 | quad)
}

fn encode_short_reg(reg: Option<register::RiscV>) -> Result<u8, EncodeError> {
    // register must be 8..16 (because that's where the bias is setup)
    let reg = reg.map_or(0, register::RiscV::get);

    if !(8..16).contains(&reg) {
        return Err(EncodeError::Incompressable);
    }

    let reg = reg & 0b111;

    Ok(reg)
}

pub fn instruction(instruction: &Instruction) -> Result<u16, EncodeError> {
    match instruction {
        Instruction::R(instruction::R { rs1, rs2, rd, opcode }) => match opcode {
            opcode::R::ADD => {
                if *rd != *rs1 && rs1.is_some() {
                    return Err(EncodeError::Incompressable);
                }

                let func4 = match rs1 {
                    // C.ADD
                    Some(_) => 0b1001,
                    // C.MV
                    None => 0b1000,
                };

                Ok(cr(Quadrant::C2, func4, *rd, *rs2))
            }
            opcode::R::SUB => {
                if *rd != *rs1 {
                    return Err(EncodeError::Incompressable);
                }

                ca(Quadrant::C1, 0b10_0011, 0b00, *rd, *rs2)
            }

            opcode::R::XOR => {
                if *rd != *rs1 {
                    return Err(EncodeError::Incompressable);
                }

                ca(Quadrant::C1, 0b10_0011, 0b01, *rd, *rs2)
            }
            opcode::R::OR => {
                if *rd != *rs1 {
                    return Err(EncodeError::Incompressable);
                }

                ca(Quadrant::C1, 0b10_0011, 0b10, *rd, *rs2)
            }
            opcode::R::AND => {
                if *rd != *rs1 {
                    return Err(EncodeError::Incompressable);
                }

                ca(Quadrant::C1, 0b10_0011, 0b11, *rd, *rs2)
            }
            opcode::R::SLL
            | opcode::R::SCond(_)
            | opcode::R::SRL
            | opcode::R::SRA
            | opcode::R::MUL
            | opcode::R::MULH
            | opcode::R::MULHSU
            | opcode::R::MULHU
            | opcode::R::DIV
            | opcode::R::DIVU
            | opcode::R::REM
            | opcode::R::REMU => Err(EncodeError::Incompressable),
        },

        Instruction::S(instruction::S { imm, rs1, rs2, width }) => {
            const OPCODE: u16 = 0b1100_0000_0000_0000;

            if *width != Width::DWord {
                return Err(EncodeError::Incompressable);
            }

            if *rs1 == Some(register::RiscV::X2) {
                const OPCODE: u16 = 0b1100_0000_0000_0010;
                let rs2 = (encode_register(*rs2) as u16) << 2;

                //  bbaa_aa00 -> xxxa_aaab_bxxx_xxxx
                let imm = ((imm & 0b0011_1100) << 7) | ((imm & 0b1100_0000) << 1);

                return Ok(imm | rs2 | OPCODE);
            }

            let rs1 = u16::from(encode_short_reg(*rs1)?) << 7;
            let rs2 = u16::from(encode_short_reg(*rs2)?) << 2;

            //  0caa_ab00 -> xxxa_aaxx_xbcx_xxxx
            let imm =
                ((imm & 0b0011_1000) << 7) | ((imm & 0b0100_0000) >> 1) | ((imm & 0b0100) << 4);

            Ok(imm | rs1 | rs2 | OPCODE)
        }
        Instruction::B(instruction::B { rs1, rs2, imm, cmp_mode }) => {
            if imm & 1 != 0 {
                return Err(EncodeError::InvalidInstruction);
            }

            if rs2.is_some() {
                return Err(EncodeError::Incompressable);
            }

            let func3: u16 = match cmp_mode {
                opcode::Cmp::Eq => 0b110,
                opcode::Cmp::Ne => 0b111,
                _ => return Err(EncodeError::Incompressable),
            };

            let func3 = func3 << 13;
            let rs1 = u16::from(encode_short_reg(*rs1)?) << 7;

            let imm = signed_truncate::<9>(*imm)?;

            // 0b000a_cceb_bdd0 -> 000a_bbxx_xccd_de00
            let imm = ((imm & 0b0001_0000_0000) << 4)
                | ((imm & 0b0000_0001_1000) << 7)
                | ((imm & 0b0000_1100_0000) >> 1)
                | ((imm & 0b0000_0000_0110) << 2)
                | ((imm & 0b0000_0010_0000) >> 3);

            Ok(func3 | rs1 | imm | 0b01)
        }
        Instruction::U(instruction::U { imm, rd, opcode }) => {
            match opcode {
                opcode::U::LUI => {}
                opcode::U::AUIPC => return Err(EncodeError::Incompressable),
            }

            if matches!(rd, Some(register::RiscV::X2)) {
                return Err(EncodeError::Incompressable);
            }

            if *imm == 0 {
                return Err(EncodeError::Incompressable);
            }

            if imm & 0xfff != 0 {
                return Err(EncodeError::InvalidInstruction);
            }

            let imm = (imm >> 12) as u16;
            let imm6 = ((imm & 0b01_1111) << 2) | ((imm & 0b10_0000) << 7);

            Ok(ci(Quadrant::C1, 0b011, imm6, *rd))
        }
        Instruction::J(instruction::J { imm, rd, opcode }) => {
            let opcode: u16 = match opcode {
                opcode::J::JAL if rd.is_none() => 0b1010_0000_0000_0001,
                opcode::J::JAL if matches!(*rd, Some(register::RiscV::X1)) => 0b0010_0000_0000_0001,
                opcode::J::JAL => return Err(EncodeError::Incompressable),
            };

            let imm = signed_truncate_32::<12>(*imm)?;

            // 0000_adcc_fehb_ggg0 -> xxxa_bccd_efgg_ghxx
            let imm = ((imm & 0b1000_0000_0000) << 1)
                | ((imm & 0b0000_0001_0000) << 7)
                | ((imm & 0b0011_0000_0000) << 1)
                | ((imm & 0b0100_0000_0000) >> 2)
                | ((imm & 0b0000_0100_0000) << 1)
                | ((imm & 0b0000_1000_0000) >> 1)
                | ((imm & 0b0000_0000_1110) << 2)
                | ((imm & 0b0000_0010_0000) >> 3);

            let imm = imm as u16;

            Ok(opcode | imm)
        }

        Instruction::Sys(instruction::Sys { opcode }) => match opcode {
            opcode::RSys::ECALL => Err(EncodeError::Incompressable),
            opcode::RSys::EBREAK => Ok(0b1001_0000_0000_0010),
        },

        // terror
        Instruction::I(instruction::I { imm, rs1, rd, opcode }) => {
            match opcode {
                opcode::I::ADDI => {
                    // func3=0,quad=0
                    const ADDI4SPN: u16 = 0b0000_0000_0000_0000;
                    // addi is all kinds of special, it's the cannoical nop (twice over), and has other special unstructions besides.

                    // first thing's first, let's turn cannonical nop into "not a problem"
                    if *imm == 0 && rs1.is_none() && rd.is_none() {
                        return Ok(const { ci(Quadrant::C1, 0b000, 0, None) });
                    }

                    // now there's still a few possible encodings:
                    // C.ADDI - has full RS1/RD with (sext)6:0 nz-immediate.
                    // C.ADDI16SP - RS1/RD=x2 with (sext)10:4 nz-immediate (ie, scaled by 16).
                    // C.ADDI4SPN - RS1=x2, RD', (zext)10:2 nz-immediate (scaled by 4)
                    // C.LI - RS1=x0, RD, (sext)6:0 imm.
                    // I think my chosen cannonicalization is:
                    // C.ADDI16SP, C.LI, C.ADDI, C.ADDI4SPN

                    if rs1 == rd || rs1.is_none() {
                        const ADDI: u8 = 0b000;
                        const LI: u8 = 0b010;
                        const ADDI16SP: u8 = 0b011;

                        let imm6_a = || {
                            let imm = super::signed_truncate::<6>(*imm)?;
                            let imm6 = ((imm & 0b01_1111) << 2) | ((imm & 0b10_0000) << 7);

                            Ok(imm6)
                        };

                        let imm6_b = || {
                            let imm = super::signed_truncate::<10>(*imm)?;

                            // 0000_00ad_dceb_0000 -> 000a_0000_0bcd_de00
                            let imm = ((imm & 0b0010_0000_0000) << 3)
                                | ((imm & 0b0000_0001_0000) << 2)
                                | ((imm & 0b0000_0100_0000) >> 1)
                                | ((imm & 0b0001_1000_0000) >> 4)
                                | ((imm & 0b0000_0010_0000) >> 3);

                            Ok(imm)
                        };

                        let (func3, imm6) = match rs1 {
                            Some(register::RiscV::X2) if imm.trailing_zeros() >= 4 && *imm != 0 => {
                                (ADDI16SP, imm6_b()?)
                            }
                            Some(_) => (ADDI, imm6_a()?),
                            None => (LI, imm6_a()?),
                        };

                        return Ok(ci(Quadrant::C1, func3, imm6, *rd));
                    }

                    // C.ADDI4SPN case

                    // RS1 is implicit, but it does have to be X2
                    if *rs1 != Some(register::RiscV::X2) {
                        return Err(EncodeError::Incompressable);
                    }

                    let rd = encode_short_reg(*rd)?;

                    if *imm & !0b0000_0011_1111_1100 != 0 {
                        return Err(EncodeError::Incompressable);
                    }

                    // 0000_00bb_bbaa_dc00 -> aabb_bbcd
                    let imm = ((imm & 0b00_0011_0000) << 2)
                        | ((imm & 0b11_1100_0000) >> 4)
                        | ((imm & 0b00_0000_0100) >> 1)
                        | ((imm & 0b00_0000_1000) >> 3);

                    let imm = imm << 5;

                    Ok(imm | (u16::from(rd) << 2) | ADDI4SPN)
                }
                opcode::I::ANDI => {
                    let imm6 = signed_truncate::<6>(*imm)?;
                    cbi(Quadrant::C1, 0b100, Quadrant::C2, imm6, *rd)
                }
                opcode::I::SLLI => {
                    let imm = imm & 0b1_1111;

                    if rs1 != rd {
                        return Err(EncodeError::Incompressable);
                    }

                    Ok(ci(Quadrant::C2, 0b000, imm << 2, *rd))
                }
                opcode::I::SRLI => {
                    if rs1 != rd {
                        return Err(EncodeError::Incompressable);
                    }

                    cbi(Quadrant::C1, 0b100, Quadrant::C0, imm & 0b1_1111, *rd)
                }
                opcode::I::SRAI => {
                    if rs1 != rd {
                        return Err(EncodeError::Incompressable);
                    }

                    cbi(Quadrant::C1, 0b100, Quadrant::C1, imm & 0b1_1111, *rd)
                }

                opcode::I::SICond(_) | opcode::I::XORI | opcode::I::ORI => {
                    Err(EncodeError::Incompressable)
                }
            }
        }
        Instruction::IJump(instruction::IJump { imm, rs1, rd, opcode }) => {
            let opcode::IJump::JALR = opcode;

            if *imm != 0 {
                return Err(EncodeError::Incompressable);
            }

            if rs1.is_none() {
                return Err(EncodeError::InvalidInstruction);
            }

            let func4 = match rd {
                None => 0b1000,
                Some(register::RiscV::X1) => 0b1001,
                Some(_) => return Err(EncodeError::Incompressable),
            };

            Ok(cr(Quadrant::C2, func4, *rs1, None))
        }
        Instruction::IMem(instruction::IMem { imm, rs1, rd, opcode }) => {
            const OPCODE: u16 = 0b0100_0000_0000_0000;
            match opcode {
                opcode::IMem::LD(Width::DWord) => {}

                opcode::IMem::FENCE | opcode::IMem::LD(_) | opcode::IMem::LDU(_) => {
                    return Err(EncodeError::Incompressable);
                }
            }

            // two options here, C.LWSP or C.LW
            // C.LWSP has full range for rd, but only ever has x2 for rs, C.LW has rs' and rd', C.LWSP also has twice the range.

            // C.LWSP
            if *rs1 == Some(register::RiscV::X2) {
                // reserved.
                let Some(rd) = rd else {
                    return Err(EncodeError::Incompressable);
                };

                // ccab_bb00 -> 0bxxxa_0000_0bbb_ccxx
                let imm = ((imm & 0b0010_0000) << 7)
                    | ((imm & 0b0001_1100) << 2)
                    | ((imm & 0b1100_0000) >> 4);

                return Ok(ci(Quadrant::C2, 0b010, imm, Some(*rd)));
            }

            let rd = u16::from(encode_short_reg(*rd)?) << 2;
            let rs1 = u16::from(encode_short_reg(*rs1)?) << 7;

            //  0caa_ab00 -> xxxa_aaxx_xbcx_xxxx
            let imm =
                ((imm & 0b0011_1000) << 7) | ((imm & 0b0100_0000) >> 1) | ((imm & 0b0100) << 4);

            Ok(imm | rs1 | rd | OPCODE)
        }
    }
}

#[cfg(test)]
mod tests {
    use ribit_core::instruction::Instruction;
    use ribit_core::{instruction, opcode};

    #[test]
    fn roundtrip_all() {
        for raw in 0_u16..=u16::MAX {
            let Ok(instruction) = ribit_decode::compressed::decode_instruction(raw) else {
                continue;
            };

            let res = super::instruction(&instruction);

            assert!(res.is_ok(), "encode failed: {instruction:?}");

            let res = res.unwrap();

            // ADDI isn't guaranteed round tripable, instead try decoding a second time and instead checking the values.
            if matches!(instruction, Instruction::I(instruction::I { opcode: opcode::I::ADDI, .. }))
            {
                let second = ribit_decode::compressed::decode_instruction(res).unwrap();

                match (instruction, second) {
                    (Instruction::I(a), Instruction::I(b)) => {
                        assert_eq!(a.opcode, b.opcode);
                        assert_eq!(a.imm, b.imm);
                        assert_eq!(a.rd, b.rd);
                        assert_eq!(a.rs1, b.rs1);
                    }
                    (instruction, _) => {
                        panic!("roundtrip failed ({raw:016b} != {res:016b}: {instruction:?}")
                    }
                }

                continue;
            }

            // ADD also isn't guaranteed round trippable (C.MV and C.ADD with RD/RS1=X0)
            if let Instruction::R(instruction::R {
                rs1: None,
                rd: None,
                opcode: opcode::R::ADD,
                rs2,
            }) = instruction
            {
                let second = ribit_decode::compressed::decode_instruction(res).unwrap();

                match second {
                    Instruction::R(instruction::R {
                        rs1: None,
                        rd: None,
                        opcode: opcode::R::ADD,
                        rs2: new_rs2,
                    }) if rs2 == new_rs2 => continue,
                    _ => {
                        panic!("roundtrip failed ({raw:016b} != {res:016b}: {instruction:?}")
                    }
                }
            }

            assert!(raw == res, "roundtrip failed ({raw:016b} != {res:016b}): {instruction:?}");
        }
    }
}
