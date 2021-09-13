use std::fmt;

use opcode::Cmp;

use crate::instruction::{self, Instruction};
use crate::{opcode, register, Width};

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
    #[must_use]
    pub fn from_info(info: &'a instruction::Info) -> Self {
        let width = match info.len {
            2 => InstructionWidth::Compressed,
            4 => InstructionWidth::Normal,
            width => panic!("Invalid instruction width: {}", width),
        };

        Self { width, instruction: &info.instruction }
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
            Instruction::R(instruction::R { rs1, rs2, rd, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);
                let rd = WrapRegister(*rd);

                write!(f, "{} {}, {}, {}", opcode, rd, rs1, rs2)
            }

            Instruction::I(instruction::I { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {}, {:04x}", opcode, rd, rs1, imm)
            }

            Instruction::IJump(instruction::IJump { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {:04x}({})", opcode, rd, imm, rs1)
            }

            Instruction::IMem(instruction::IMem { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {:04x}({})", opcode, rd, imm, rs1)
            }

            Instruction::S(instruction::S { imm, rs1, rs2, width }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                match width {
                    Width::Byte => write!(f, "SB {}, {}({:04x})", rs2, rs1, imm),
                    Width::Word => write!(f, "SH {}, {}({:04x})", rs2, rs1, imm),
                    Width::DWord => write!(f, "SW {}, {}({:04x})", rs2, rs1, imm),
                }
            }
            Instruction::B(instruction::B { rs1, rs2, imm, cmp_mode }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                match *cmp_mode {
                    Cmp::Eq => write!(f, "BEQ {}, {:04x}({})", rs1, imm, rs2),
                    Cmp::Ne => write!(f, "BNE {}, {:04x}({})", rs1, imm, rs2),
                    Cmp::Lt => write!(f, "BLT {}, {:04x}({})", rs1, imm, rs2),
                    Cmp::Ltu => write!(f, "BLTU {}, {:04x}({})", rs1, imm, rs2),
                    Cmp::Ge => write!(f, "BGE {}, {:04x}({})", rs1, imm, rs2),
                    Cmp::Geu => write!(f, "BGEU {}, {:04x}({})", rs1, imm, rs2),
                }
            }

            Instruction::U(instruction::U { imm, rd, opcode }) => {
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {:05x}", opcode, rd, imm >> 12)
            }

            Instruction::J(instruction::J { imm, rd, opcode }) => {
                let rd = WrapRegister(*rd);
                write!(f, "{} {}, {:08x}", opcode, rd, imm)
            }

            Instruction::Sys(instruction::Sys { opcode }) => opcode.fmt(f),
        }
    }
}
