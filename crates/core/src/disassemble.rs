use core::fmt;

use opcode::Cmp;

use crate::instruction::{self, Instruction};
use crate::{Width, opcode, register};

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
            width => panic!("Invalid instruction width: {width}"),
        };

        Self { width, instruction: &info.instruction }
    }
}

impl fmt::Display for FmtInstruction<'_> {
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

                write!(f, "{opcode} {rd}, {rs1}, {rs2}")
            }

            Instruction::I(instruction::I { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{opcode} {rd}, {rs1}, {:03x}", imm & 0xfff)
            }

            Instruction::IJump(instruction::IJump { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{opcode} {rd}, {:03x}({rs1})", imm & 0xfff)
            }

            Instruction::IMem(instruction::IMem { rd, rs1, imm, opcode }) => {
                let rs1 = WrapRegister(*rs1);
                let rd = WrapRegister(*rd);
                write!(f, "{opcode} {rd}, {:03x}({rs1})", imm & 0xfff)
            }

            Instruction::S(instruction::S { imm, rs1, rs2, width }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                let op = match width {
                    Width::Byte => "SB",
                    Width::Word => "SH",
                    Width::DWord => "SW",
                };

                write!(f, "{op} {rs2}, {:03x}({rs1})", imm & 0xfff)
            }
            Instruction::B(instruction::B { rs1, rs2, imm, cmp_mode }) => {
                let rs1 = WrapRegister(*rs1);
                let rs2 = WrapRegister(*rs2);

                let op = match *cmp_mode {
                    Cmp::Eq => "BEQ",
                    Cmp::Ne => "BNE",
                    Cmp::Lt => "BLT",
                    Cmp::Ge => "BLTU",
                    Cmp::Ltu => "BGE",
                    Cmp::Geu => "BGEU",
                };

                write!(f, "{op} {rs1}, {imm:04x}({rs2})")
            }

            Instruction::U(instruction::U { imm, rd, opcode }) => {
                let rd = WrapRegister(*rd);
                write!(f, "{opcode} {rd}, {:05x}", imm >> 12)
            }

            &Instruction::J(instruction::J { imm, rd, opcode }) => {
                let rd = WrapRegister(rd);
                write!(f, "{opcode} {rd}, {:05x}", (imm.cast_signed() >> 1) & 0xfffff)
            }

            Instruction::Sys(instruction::Sys { opcode }) => opcode.fmt(f),
        }
    }
}
