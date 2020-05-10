use crate::{
    instruction::{self, Instruction},
    opcode, register, Width,
};
use opcode::Cmp;
use std::fmt;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(u8)]
enum InstructionWidth {
    Compressed = 2,
    Normal = 4,
}

struct WrapRegister(Option<register::RiscV>);

impl fmt::Display for WrapRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            None => f.write_str("x0"),
            Some(xnum) => write!(f, "x{}", xnum.get()),
        }
    }
}

pub struct FmtInstruction<'a> {
    instruction: &'a Instruction,
    width: InstructionWidth,
}

impl<'a> FmtInstruction<'a> {
    pub fn from_info(info: &'a instruction::Info) -> Self {
        let width = match info.len {
            2 => InstructionWidth::Compressed,
            4 => InstructionWidth::Normal,
            width => panic!("Invalid instruction width: {}", width),
        };

        Self {
            width,
            instruction: &info.instruction,
        }
    }
}

impl<'a> fmt::Display for FmtInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // todo: instructions actually get different names and parameters when they're compressed sometimes.
        match self.width {
            InstructionWidth::Compressed => f.write_str("C.")?,
            InstructionWidth::Normal => {}
        }

        match self.instruction {
            Instruction::R(instruction::R {
                rs1,
                rs2,
                rd,
                opcode,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);
                let rd = WrapRegister(*rd);

                write!(f, "{} {}, {}, {}", opcode, rd, rs1, rs2)
            }

            Instruction::I(instruction::I {
                rd,
                rs1,
                imm,
                opcode,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}, {}", opcode, rd, rs1, imm)
            }

            Instruction::IJump(instruction::IJump {
                rd,
                rs1,
                imm,
                opcode,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}, {}", opcode, rd, rs1, imm)
            }

            Instruction::IMem(instruction::IMem {
                rd,
                rs1,
                imm,
                opcode,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}, {}", opcode, rd, rs1, imm)
            }

            Instruction::S(instruction::S {
                imm,
                rs1,
                rs2,
                width,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                match width {
                    Width::Byte => write!(f, "SB {}, {}, {}", rs1, rs2, imm),
                    Width::Word => write!(f, "SH {}, {}, {}", rs1, rs2, imm),
                    Width::DWord => write!(f, "SW {}, {}, {}", rs1, rs2, imm),
                }
            }
            Instruction::B(instruction::B {
                rs1,
                rs2,
                imm,
                cmp_mode,
            }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                match *cmp_mode {
                    Cmp::Eq => write!(f, "BEQ {}, {}, {}", rs1, rs2, imm),
                    Cmp::Ne => write!(f, "BNE {}, {}, {}", rs1, rs2, imm),
                    Cmp::Lt => write!(f, "BLT {}, {}, {}", rs1, rs2, imm),
                    Cmp::Ltu => write!(f, "BLTU {}, {}, {}", rs1, rs2, imm),
                    Cmp::Ge => write!(f, "BGE {}, {}, {}", rs1, rs2, imm),
                    Cmp::Geu => write!(f, "BGEU {}, {}, {}", rs1, rs2, imm),
                }
            }

            Instruction::U(instruction::U { imm, rd, opcode }) => {
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}", opcode, rd, imm)
            }

            Instruction::J(instruction::J { imm, rd, opcode }) => {
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}", opcode, rd, imm)
            }

            Instruction::Sys(instruction::Sys { opcode }) => opcode.fmt(f),
        }
    }
}
