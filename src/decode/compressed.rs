use crate::{instruction::*, DecodeError};

fn decode_register(instruction: u16) -> u8 {
    ((instruction & 0b111) | 0b1_1000) as u8
}

pub fn decode_instruction(instruction: u16) -> Result<Instruction, DecodeError> {
    let opcode = (instruction & 0b11) as u8;
    let funct3 = ((instruction >> 13) & 0b111) as u8;
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
            Instruction::IType(ITypeInstruction::new(imm, 2, rd, ITypeOpcode::ADDI))
        }

        // C.FLD requires D extension
        (0b00, 0b001) => return Err(DecodeError),
        // C.FSD requires D extension
        (0b00, 0b101) => return Err(DecodeError),

        (0b00, 0b010) | (0b00, 0b110) => {
            let rs1 = decode_register(instruction >> 7);
            let rd = decode_register(instruction >> 2);

            // xxxa_aaxx_xbcx_xxxx -> 0caa_ab00
            let imm = ((instruction >> 7) & 0b0011_1000)
                | ((instruction << 1) & 0b0100_0000)
                | ((instruction >> 4) & 0b0100);
            if funct3 == 0b010 {
                Instruction::IType(ITypeInstruction::new(imm, rs1, rd, ITypeOpcode::LW))
            } else {
                let rs = (rd << 4) | rs1;
                Instruction::SType(STypeInstruction::new(imm, rs, STypeOpcode::SW))
            }
        }

        (0b00, 0b011) => return Err(DecodeError),

        // C.FSW requires the F extension
        (0b00, 0b111) => return Err(DecodeError),

        (0b00, 0b100) => return Err(DecodeError), // reserved
        (0b01, 0b000) | (0b01, 0b010) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let r = ((instruction >> 7) & 0b0001_1111) as u8;

            // FIXME: need to sign extend imm
            if funct3 == 0b000 {
                // addi/noop
                Instruction::IType(ITypeInstruction::new(imm, r, r, ITypeOpcode::ADDI))
            } else {
                // C.LI
                Instruction::IType(ITypeInstruction::new(imm, 0, r, ITypeOpcode::ADDI))
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

            // FIXME: Sign extend imm.
            let link_reg = (funct3 == 0b101) as u8;

            Instruction::JType(JTypeInstruction::new(imm, link_reg, JTypeOpcode::JAL))
        }

        (0b01, 0b011) => {
            let imm = instruction & 0b0001_0000_0111_1100;

            if imm == 0 {
                // reserved.
                return Err(DecodeError);
            }

            let r = ((instruction >> 7) & 0b0001_1111) as u8;

            match r {
                2 => {
                    // 000a_0000_0bcd_de00 -> 0000_00ad_dceb_0000
                    let imm = ((imm >> 4) & 0b0001_0000_0000)
                        | ((imm >> 2) & 0b0000_0001_0000)
                        | ((imm << 1) & 0b0000_0100_0000)
                        | ((imm << 4) & 0b0000_0001_1000)
                        | ((imm << 3) & 0b0000_0010_0000);
                    // FIXME: Sign extend imm
                    Instruction::IType(ITypeInstruction::new(imm, 2, 2, ITypeOpcode::ADDI))
                }

                _ => {
                    let imm = imm as u32;
                    // 000a_0000_0bbb_bb00 -> 00ab_bbbb_0000_0000_0000
                    let imm = ((imm << 05) & 0b0010_0000_0000_0000_0000)
                        | ((imm << 10) & 0b0001_1111_0000_0000_0000);

                    // FIXME: C.LUI loads the non-zero 6-bit immediate field into bits 17–12 of the destination register, clears the
                    // bottom 12 bits, and sign-extends bit 17 into all higher bits of the destination.
                    Instruction::UType(UTypeInstruction::new(imm, r & 0xf, UTypeOpcode::LUI))
                }
            }
        }

        (0b01, 0b100) => {
            let imm = ((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111);
            let r = decode_register(instruction >> 7);
            let rs2 = decode_register(instruction >> 2);

            let imm5 = imm >> 5;

            let rs = (rs2 << 4) | r;

            match (instruction >> 10) & 0b11 {
                0b00 if imm5 == 0 => {
                    Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::SRLI))
                }
                0b01 if imm5 == 0 => {
                    Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::SRAI))
                }
                0b10 => Instruction::IType(ITypeInstruction::new(imm, r, r, ITypeOpcode::ANDI)),
                0b11 if imm5 == 0 => match (instruction >> 5) & 0b11 {
                    0b00 => Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::SUB)),
                    0b01 => Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::XOR)),
                    0b10 => Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::OR)),
                    0b11 => Instruction::RType(RTypeInstruction::new(rs, r, RTypeOpcode::AND)),
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

            // FIXME: sign extend imm

            let opcode = if funct3 == 0b110 {
                BTypeOpcode::BEQ
            } else {
                BTypeOpcode::BNE
            };

            Instruction::BType(BTypeInstruction::new(imm, rs1 & 0xf, opcode))
        }

        (0b10, 0b000) => {
            let imm =
                (((instruction >> 7) & 0b0010_0000) | ((instruction >> 2) & 0b0001_1111)) as u8;

            if imm & 0b0001_0000 != 0 {
                return Err(DecodeError);
            }

            let r = ((instruction >> 7) & 0b0001_1111) as u8;

            Instruction::RType(RTypeInstruction::new(
                (imm << 4) | r & 0xf,
                r & 0xf,
                RTypeOpcode::SLLI,
            ))
        }

        // C.FLDSP requires D extension
        (0b10, 0b001) => return Err(DecodeError),

        (0b10, 0b010) | (0b10, 0b011) => {
            // 0bxxxa_0000_0bbb_ccxx -> ccab_bb00
            let imm = ((instruction >> 7) & 0b0010_0000)
                | ((instruction >> 2) & 0b0001_1100)
                | ((instruction << 4) & 0b1100_0000);

            let r = ((instruction >> 7) & 0b0001_1111) as u8;

            if funct3 == 0b011 {
                // C.FLWSP requires F extension
                return Err(DecodeError);
            }

            if r == 0 {
                return Err(DecodeError);
            }

            Instruction::IType(ITypeInstruction::new(
                imm,
                r & 0xf,
                r & 0xf,
                ITypeOpcode::LW,
            ))
        }

        (0b10, 0b100) => {
            let r = ((instruction >> 7) & 0b0001_1111) as u8;
            let rs2 = ((instruction >> 2) & 0b0001_1111) as u8;
            let imm5 = ((instruction >> 12) & 1) != 0;

            match (imm5, rs2, r) {
                (false, 0, 0) => return Err(DecodeError),
                (false, 0, rs1) => {
                    Instruction::IType(ITypeInstruction::new(0, rs1 & 0xf, 0, ITypeOpcode::JALR))
                }
                (false, rs2, rd) => {
                    Instruction::RType(RTypeInstruction::new(rs2 << 4, rd & 0xf, RTypeOpcode::ADD))
                }
                (true, 0, 0) => {
                    Instruction::RType(RTypeInstruction::new(0, 0, RTypeOpcode::EBREAK))
                }
                (true, 0, rs1) => {
                    Instruction::IType(ITypeInstruction::new(0, rs1 & 0xf, 1, ITypeOpcode::JALR))
                }
                (true, rs2, r) => Instruction::RType(RTypeInstruction::new(
                    (rs2 << 4) | (r & 0xf),
                    r & 0xf,
                    RTypeOpcode::ADD,
                )),
            }
        }

        // C.FSDSP requires D extension
        (0b10, 0b101) => return Err(DecodeError),

        (0b10, 0b110) => {
            let rs2 = (instruction >> 2) & 0b0001_1111;
            let rs2 = (rs2 << 4) as u8;

            // xxxa_aaab_bxxx_xxxx -> bbaa_aa00
            let imm = ((instruction >> 7) & 0b0011_1100) | ((instruction >> 1) & 0b1100_0000);

            Instruction::SType(STypeInstruction::new(imm, rs2, STypeOpcode::SW))
        }

        // C.FSWSP requires RV32F extension.
        (0b10, 0b111) => return Err(DecodeError),

        // unknown opcode.
        _ => return Err(DecodeError),
    };

    Ok(instruction)
}
