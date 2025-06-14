use super::{sign_extend, sign_extend_32};
use crate::instruction::{self, Instruction};
use crate::register::RiscV as RiscVRegister;
use crate::{CompressedDecodeError, Extension, Width, opcode};

const fn decode_register(instruction: u16) -> RiscVRegister {
    // safety: rnum masks in 0b1000 which prevents it from ever being 0
    // safety: rnum masks out any bits that might cause it to be more than 31 (0b1_1111)
    #[allow(unsafe_code)]
    unsafe {
        let rnum = ((instruction | 0b1000) & 0b1111) as u8;
        RiscVRegister::new_unchecked(rnum)
    }
}

fn decode_full_register(instruction: u16) -> Option<RiscVRegister> {
    super::decode_register(u32::from(instruction))
}

#[allow(clippy::too_many_lines)]
pub fn decode_instruction(instruction: u16) -> Result<Instruction, CompressedDecodeError> {
    let opcode = (instruction & 0b11) as u8;
    let funct3 = ((instruction >> 13) & 0b111) as u8;

    #[allow(clippy::match_same_arms)]
    let instruction = match (opcode, funct3) {
        (0b00, 0b000) => {
            // todo: make this a function
            if instruction == 0 {
                // defined as unusable.
                return Err(CompressedDecodeError::InvalidInstruction(instruction));
            }

            let imm = (instruction >> 5) & 0xff;
            // aabb_bbcd -> 0000_00bb_bbaa_dc00
            let imm = ((imm & 0b1100_0000) >> 2)
                | ((imm & 0b0011_1100) << 4)
                | ((imm & 0b10) << 1)
                | ((imm & 0b01) << 3);

            // reserved
            if imm == 0 {
                return Err(CompressedDecodeError::InvalidInstruction(instruction));
            }

            let rd = decode_register(instruction >> 2);
            Instruction::I(instruction::I::new(
                imm,
                Some(RiscVRegister::X2),
                Some(rd),
                opcode::I::ADDI,
            ))
        }

        // C.FLD requires D extension
        (0b00, 0b001) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::D, instruction));
        }
        // C.FSD requires D extension
        (0b00, 0b101) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::D, instruction));
        }

        (0b00, 0b010 | 0b110) => {
            let rs1 = Some(decode_register(instruction >> 7));
            let r = Some(decode_register(instruction >> 2));

            // xxxa_aaxx_xbcx_xxxx -> 0caa_ab00
            let imm = ((instruction >> 7) & 0b0011_1000)
                | ((instruction << 1) & 0b0100_0000)
                | ((instruction >> 4) & 0b0100);

            if funct3 == 0b010 {
                Instruction::IMem(instruction::IMem::new(
                    imm,
                    rs1,
                    r,
                    opcode::IMem::LD(Width::DWord),
                ))
            } else {
                Instruction::S(instruction::S::new(imm, rs1, r, Width::DWord))
            }
        }

        (0b00, 0b011) => return Err(CompressedDecodeError::InvalidInstruction(instruction)),

        // C.FSW requires the F extension
        (0b00, 0b111) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::F, instruction));
        }

        (0b00, 0b100) => return Err(CompressedDecodeError::InvalidInstruction(instruction)), // reserved
        (0b01, 0b000 | 0b010) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let imm = sign_extend(imm, 6);

            let r = decode_full_register(instruction >> 7);

            if funct3 == 0b000 {
                // addi/noop
                Instruction::I(instruction::I::new(imm, r, r, opcode::I::ADDI))
            } else {
                // C.LI
                Instruction::I(instruction::I::new(imm, None, r, opcode::I::ADDI))
            }
        }

        (0b01, 0b001 | 0b101) => {
            // xxxa_bccd_efgg_ghxx -> 0000_adcc_fehb_ggg0
            let imm = u32::from(
                ((instruction >> 1) & 0b1000_0000_0000)
                    | ((instruction >> 7) & 0b0000_0001_0000)
                    | ((instruction >> 1) & 0b0011_0000_0000)
                    | ((instruction << 2) & 0b0100_0000_0000)
                    | ((instruction >> 1) & 0b0000_0100_0000)
                    | ((instruction << 1) & 0b0000_1000_0000)
                    | ((instruction >> 2) & 0b0000_0000_1110)
                    | ((instruction << 3) & 0b0000_0010_0000),
            );

            let imm = sign_extend_32(imm, 12);

            let link_reg = (funct3 == 0b001).then_some(RiscVRegister::X1);

            Instruction::J(instruction::J::new(imm, link_reg, opcode::J::JAL))
        }

        (0b01, 0b011) => {
            let imm = instruction & 0b0001_0000_0111_1100;

            if imm == 0 {
                // reserved.
                return Err(CompressedDecodeError::InvalidInstruction(instruction));
            }

            let r = decode_full_register(instruction >> 7);

            match r {
                Some(r) if r.get() == 2 => {
                    // 000a_0000_0bcd_de00 -> 0000_00ad_dceb_0000
                    let imm = ((imm >> 3) & 0b0010_0000_0000)
                        | ((imm >> 2) & 0b0000_0001_0000)
                        | ((imm << 1) & 0b0000_0100_0000)
                        | ((imm << 4) & 0b0001_1000_0000)
                        | ((imm << 3) & 0b0000_0010_0000);

                    let imm = sign_extend(imm, 10);

                    Instruction::I(instruction::I::new(
                        imm,
                        Some(RiscVRegister::X2),
                        Some(RiscVRegister::X2),
                        opcode::I::ADDI,
                    ))
                }

                _ => {
                    let imm = u32::from(imm);
                    // 000a_0000_0bbb_bb00 -> 00ab_bbbb_0000_0000_0000
                    let imm = ((imm << 5) & 0b0010_0000_0000_0000_0000)
                        | ((imm << 10) & 0b0001_1111_0000_0000_0000);

                    let imm = sign_extend_32(imm, 18);

                    Instruction::U(instruction::U::new(imm, r, opcode::U::LUI))
                }
            }
        }

        (0b01, 0b100) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let r = Some(decode_register(instruction >> 7));
            let rs2 = Some(decode_register(instruction >> 2));

            let imm5 = imm >> 5;

            match (instruction >> 10) & 0b11 {
                0b00 if imm5 == 0 => Instruction::I(instruction::I::new(
                    (instruction >> 2) & 0x1f,
                    r,
                    r,
                    opcode::I::SRLI,
                )),
                0b01 if imm5 == 0 => Instruction::I(instruction::I::new(
                    (instruction >> 2) & 0x1f,
                    r,
                    r,
                    opcode::I::SRAI,
                )),
                0b10 => {
                    Instruction::I(instruction::I::new(sign_extend(imm, 6), r, r, opcode::I::ANDI))
                }
                0b11 if imm5 == 0 => match (instruction >> 5) & 0b11 {
                    0b00 => Instruction::R(instruction::R::new(r, rs2, r, opcode::R::SUB)),
                    0b01 => Instruction::R(instruction::R::new(r, rs2, r, opcode::R::XOR)),
                    0b10 => Instruction::R(instruction::R::new(r, rs2, r, opcode::R::OR)),
                    0b11 => Instruction::R(instruction::R::new(r, rs2, r, opcode::R::AND)),
                    _ => unreachable!(),
                },
                _ => return Err(CompressedDecodeError::InvalidInstruction(instruction)),
            }
        }

        (0b01, 0b110 | 0b111) => {
            let rs1 = decode_register(instruction >> 7);

            // 000a_bbxx_xccd_de00 -> 0b000a_cceb_bdd0
            let imm = ((instruction >> 4) & 0b0001_0000_0000)
                | ((instruction >> 7) & 0b0000_0001_1000)
                | ((instruction << 1) & 0b0000_1100_0000)
                | ((instruction >> 2) & 0b0000_0000_0110)
                | ((instruction << 3) & 0b0000_0010_0000);

            let imm = sign_extend(imm, 9);

            let cmp_mode = if funct3 == 0b110 { opcode::Cmp::Eq } else { opcode::Cmp::Ne };

            Instruction::B(instruction::B::new(imm, Some(rs1), None, cmp_mode))
        }

        (0b10, 0b000) => {
            if ((instruction >> 7) & 0b0010_0000) != 0 {
                return Err(CompressedDecodeError::InvalidInstruction(instruction));
            }

            let shamt = (instruction >> 2) & 0x1f;

            let r = decode_full_register(instruction >> 7);

            Instruction::I(instruction::I::new(shamt, r, r, opcode::I::SLLI))
        }

        // C.FLDSP requires D extension
        (0b10, 0b001) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::D, instruction));
        }

        (0b10, 0b010 | 0b011) => {
            // 0bxxxa_0000_0bbb_ccxx -> ccab_bb00
            let imm = ((instruction >> 7) & 0b0010_0000)
                | ((instruction >> 2) & 0b0001_1100)
                | ((instruction << 4) & 0b1100_0000);

            let r = decode_full_register(instruction >> 7)
                .ok_or(CompressedDecodeError::InvalidInstruction(instruction))?;

            if funct3 == 0b011 {
                // C.FLWSP requires F extension
                return Err(CompressedDecodeError::UnimplementedExtension(
                    Extension::D,
                    instruction,
                ));
            }

            Instruction::IMem(instruction::IMem::new(
                imm,
                Some(RiscVRegister::X2),
                Some(r),
                opcode::IMem::LD(Width::DWord),
            ))
        }

        (0b10, 0b100) => {
            let r = decode_full_register(instruction >> 7);
            let rs2 = decode_full_register(instruction >> 2);
            let imm5 = ((instruction >> 12) & 1) != 0;

            match (imm5, rs2, r) {
                (false, None, None) => {
                    return Err(CompressedDecodeError::InvalidInstruction(instruction));
                }
                (false, None, rs1) => {
                    Instruction::IJump(instruction::IJump::new(0, rs1, None, opcode::IJump::JALR))
                }
                (false, rs2, rd) => {
                    Instruction::R(instruction::R::new(None, rs2, rd, opcode::R::ADD))
                }
                (true, None, None) => Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
                (true, None, rs1) => Instruction::IJump(instruction::IJump::new(
                    0,
                    rs1,
                    Some(RiscVRegister::X1),
                    opcode::IJump::JALR,
                )),
                (true, rs2, r) => Instruction::R(instruction::R::new(r, rs2, r, opcode::R::ADD)),
            }
        }

        // C.FSDSP requires D extension
        (0b10, 0b101) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::D, instruction));
        }

        (0b10, 0b110) => {
            let rs2 = decode_full_register(instruction >> 2);

            // xxxa_aaab_bxxx_xxxx -> bbaa_aa00
            let imm = ((instruction >> 7) & 0b0011_1100) | ((instruction >> 1) & 0b1100_0000);

            Instruction::S(instruction::S::new(imm, Some(RiscVRegister::X2), rs2, Width::DWord))
        }

        // C.FSWSP requires RV32F extension.
        (0b10, 0b111) => {
            return Err(CompressedDecodeError::UnimplementedExtension(Extension::F, instruction));
        }

        // this range is used for full length instructions
        (0b11, _) => return Err(CompressedDecodeError::InvalidInstruction(instruction)),

        // unknown opcode.
        _ => return Err(CompressedDecodeError::InvalidInstruction(instruction)),
    };

    Ok(instruction)
}
