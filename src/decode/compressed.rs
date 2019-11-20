use crate::{instruction::*, opcode, DecodeError};

use super::{sign_extend, sign_extend_32};

fn decode_register(instruction: u16) -> RiscVRegister {
    // safety: rnum ors in 0b1_1000 which prevents it from ever being 0
    // safety: rnum ands out any bits that might cause it to be more than 31 (0b1_1111)
    unsafe {
        let rnum = ((instruction | 0b1_1000) & 0b1_1111) as u8;
        RiscVRegister::new_unchecked(rnum)
    }
}

fn decode_full_register(instruction: u16) -> Option<RiscVRegister> {
    super::decode_register(instruction as u32)
}

#[allow(clippy::too_many_lines)]
pub fn decode_instruction(instruction: u16) -> Result<Instruction, DecodeError> {
    let opcode = (instruction & 0b11) as u8;
    let funct3 = ((instruction >> 13) & 0b111) as u8;

    #[allow(clippy::match_same_arms)]
    let instruction = match (opcode, funct3) {
        (0b00, 0b000) => {
            // todo: make this a function
            if instruction == 0 {
                // defined as unusable.
                return Err(DecodeError);
            }

            let imm = (instruction >> 5) & 0xff;
            // aabb_bbcd -> 0000_00bb_bbaa_dc00
            let imm = ((imm & 0b1100_0000) >> 2)
                | ((imm & 0b0011_1100) << 4)
                | ((imm & 0b10) << 1)
                | ((imm & 0b01) << 3);

            // reserved
            if imm == 0 {
                return Err(DecodeError);
            }

            let rd = decode_register(instruction >> 2);
            Instruction::I(ITypeInstruction::new(
                imm,
                Some(RiscVRegister::X2),
                Some(rd),
                opcode::I::ADDI,
            ))
        }

        // C.FLD requires D extension
        (0b00, 0b001) => return Err(DecodeError),
        // C.FSD requires D extension
        (0b00, 0b101) => return Err(DecodeError),

        (0b00, 0b010) | (0b00, 0b110) => {
            let rs1 = Some(decode_register(instruction >> 7));
            let r = Some(decode_register(instruction >> 2));

            // xxxa_aaxx_xbcx_xxxx -> 0caa_ab00
            let imm = ((instruction >> 7) & 0b0011_1000)
                | ((instruction << 1) & 0b0100_0000)
                | ((instruction >> 4) & 0b0100);

            if funct3 == 0b010 {
                Instruction::I(ITypeInstruction::new(imm, rs1, r, opcode::I::LW))
            } else {
                Instruction::S(STypeInstruction::new(imm, rs1, r, opcode::S::SW))
            }
        }

        (0b00, 0b011) => return Err(DecodeError),

        // C.FSW requires the F extension
        (0b00, 0b111) => return Err(DecodeError),

        (0b00, 0b100) => return Err(DecodeError), // reserved
        (0b01, 0b000) | (0b01, 0b010) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let imm = sign_extend(imm, 6);

            let r = decode_full_register(instruction >> 7);

            // FIXME: need to sign extend imm
            if funct3 == 0b000 {
                // addi/noop
                Instruction::I(ITypeInstruction::new(imm, r, r, opcode::I::ADDI))
            } else {
                // C.LI
                Instruction::I(ITypeInstruction::new(imm, None, r, opcode::I::ADDI))
            }
        }

        (0b01, 0b001) | (0b01, 0b101) => {
            // xxxa_bccd_efgg_ghxx -> 0000_adcc_fehb_ggg0
            let imm = (((instruction >> 1) & 0b1000_0000_0000)
                | ((instruction >> 7) & 0b0000_0001_0000)
                | ((instruction >> 1) & 0b0011_0000_0000)
                | ((instruction << 2) & 0b0100_0000_0000)
                | ((instruction >> 1) & 0b0000_0100_0000)
                | ((instruction << 1) & 0b0000_1000_0000)
                | ((instruction >> 2) & 0b0000_0000_1110)
                | ((instruction << 2) & 0b0000_0010_0000)) as u32;

            let imm = sign_extend_32(imm, 12);

            let link_reg = (funct3 == 0b101).then(RiscVRegister::X1);

            Instruction::J(JTypeInstruction::new(imm, link_reg, opcode::J::JAL))
        }

        (0b01, 0b011) => {
            let imm = instruction & 0b0001_0000_0111_1100;

            if imm == 0 {
                // reserved.
                return Err(DecodeError);
            }

            let r = decode_full_register(instruction >> 7);

            match r {
                Some(r) if r.get() == 2 => {
                    // 000a_0000_0bcd_de00 -> 0000_00ad_dceb_0000
                    let imm = ((imm >> 4) & 0b0001_0000_0000)
                        | ((imm >> 2) & 0b0000_0001_0000)
                        | ((imm << 1) & 0b0000_0100_0000)
                        | ((imm << 4) & 0b0000_0001_1000)
                        | ((imm << 3) & 0b0000_0010_0000);

                    let imm = sign_extend(imm, 10);

                    Instruction::I(ITypeInstruction::new(
                        imm,
                        Some(RiscVRegister::X2),
                        Some(RiscVRegister::X2),
                        opcode::I::ADDI,
                    ))
                }

                _ => {
                    let imm = imm as u32;
                    // 000a_0000_0bbb_bb00 -> 00ab_bbbb_0000_0000_0000
                    let imm = ((imm << 5) & 0b0010_0000_0000_0000_0000)
                        | ((imm << 10) & 0b0001_1111_0000_0000_0000);

                    let imm = sign_extend_32(imm, 18);

                    Instruction::U(UTypeInstruction::new(imm, r, opcode::U::LUI))
                }
            }
        }

        (0b01, 0b100) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let r = Some(decode_register(instruction >> 7));
            let rs2 = Some(decode_register(instruction >> 2));

            let imm5 = imm >> 5;

            match (instruction >> 10) & 0b11 {
                0b00 if imm5 == 0 => {
                    Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::SRLI))
                }
                0b01 if imm5 == 0 => {
                    Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::SRAI))
                }
                0b10 => Instruction::I(ITypeInstruction::new(imm, r, r, opcode::I::ANDI)),
                0b11 if imm5 == 0 => match (instruction >> 5) & 0b11 {
                    0b00 => Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::SUB)),
                    0b01 => Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::XOR)),
                    0b10 => Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::OR)),
                    0b11 => Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::AND)),
                    _ => unreachable!(),
                },
                _ => return Err(DecodeError),
            }
        }

        (0b01, 0b110) | (0b01, 0b111) => {
            let rs1 = decode_register(instruction >> 7);
            // 000a_bbxx_xccd_de00 -> 0b000a_cceb_bdd0
            let imm = ((instruction >> 4) & 0b0001_0000_0000)
                | ((instruction >> 7) & 0b0000_0001_1000)
                | ((instruction << 1) & 0b0000_1100_0000)
                | ((instruction >> 1) & 0b0000_0000_1100)
                | ((instruction << 3) & 0b0000_0010_0000);

            let imm = sign_extend(imm, 9);

            let opcode = if funct3 == 0b110 {
                opcode::B::BEQ
            } else {
                opcode::B::BNE
            };

            Instruction::B(BTypeInstruction::new(imm, Some(rs1), None, opcode))
        }

        (0b10, 0b000) => {
            // shamt is encoded as rs2
            let rs2 = decode_full_register(instruction >> 2);

            if ((instruction >> 7) & 0b0010_0000) != 0 {
                return Err(DecodeError);
            }

            let r = decode_full_register(instruction >> 7);

            Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::SLLI))
        }

        // C.FLDSP requires D extension
        (0b10, 0b001) => return Err(DecodeError),

        (0b10, 0b010) | (0b10, 0b011) => {
            // 0bxxxa_0000_0bbb_ccxx -> ccab_bb00
            let imm = ((instruction >> 7) & 0b0010_0000)
                | ((instruction >> 2) & 0b0001_1100)
                | ((instruction << 4) & 0b1100_0000);

            let r = decode_full_register(instruction >> 7).ok_or(DecodeError)?;

            if funct3 == 0b011 {
                // C.FLWSP requires F extension
                return Err(DecodeError);
            }

            Instruction::I(ITypeInstruction::new(imm, Some(r), Some(r), opcode::I::LW))
        }

        (0b10, 0b100) => {
            let r = decode_full_register(instruction >> 7);
            let rs2 = decode_full_register(instruction >> 2);
            let imm5 = ((instruction >> 12) & 1) != 0;

            match (imm5, rs2, r) {
                (false, None, None) => return Err(DecodeError),
                (false, None, rs1) => {
                    Instruction::I(ITypeInstruction::new(0, rs1, None, opcode::I::JALR))
                }
                (false, rs2, rd) => {
                    Instruction::R(RTypeInstruction::new(None, rs2, rd, opcode::R::ADD))
                }
                (true, None, None) => {
                    Instruction::R(RTypeInstruction::new(None, None, None, opcode::R::EBREAK))
                }
                (true, None, rs1) => Instruction::I(ITypeInstruction::new(
                    0,
                    rs1,
                    Some(RiscVRegister::X1),
                    opcode::I::JALR,
                )),
                (true, rs2, r) => Instruction::R(RTypeInstruction::new(r, rs2, r, opcode::R::ADD)),
            }
        }

        // C.FSDSP requires D extension
        (0b10, 0b101) => return Err(DecodeError),

        (0b10, 0b110) => {
            let rs2 = decode_full_register(instruction >> 2);

            // xxxa_aaab_bxxx_xxxx -> bbaa_aa00
            let imm = ((instruction >> 7) & 0b0011_1100) | ((instruction >> 1) & 0b1100_0000);

            Instruction::S(STypeInstruction::new(
                imm,
                Some(RiscVRegister::X2),
                rs2,
                opcode::S::SW,
            ))
        }

        // C.FSWSP requires RV32F extension.
        (0b10, 0b111) => return Err(DecodeError),

        // this range is used for full length instructions
        (0b11, _) => return Err(DecodeError),

        // unknown opcode.
        _ => return Err(DecodeError),
    };

    Ok(instruction)
}
