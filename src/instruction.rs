use crate::decode::{decode_rd, decode_rs, sign_extend};

use crate::opcode;
use crate::register::RiscV as RiscVRegister;

// todo: decide on visibility of fields

pub struct Info {
    pub instruction: Instruction,
    pub start_address: u32,
    pub len: u32,
}

impl Info {
    #[must_use]
    pub fn end_address(&self) -> u32 {
        self.start_address.wrapping_add(self.len)
    }

    #[must_use]
    pub fn new(instruction: Instruction, start_address: u32, len: u32) -> Self {
        Self {
            instruction,
            start_address,
            len,
        }
    }
}
pub enum Instruction {
    R(R),
    I(I),
    S(S),
    B(B),
    U(U),
    J(J),
    Sys(Sys),
}

#[derive(Debug)]
pub struct Sys {
    pub(crate) opcode: opcode::RSys,
}

impl Sys {
    pub(crate) fn new(opcode: opcode::RSys) -> Self {
        Self { opcode }
    }
}

#[derive(Debug)]
pub struct R {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::R,
}

impl R {
    pub(crate) fn new(
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::R,
    ) -> Self {
        Self {
            rs1,
            rs2,
            rd,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::R) -> Self {
        let (rs1, rs2) = decode_rs(instruction);
        let rd = decode_rd(instruction);
        Self {
            rs1,
            rs2,
            rd,
            opcode,
        }
    }
}

pub struct I {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::I,
}

impl I {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::I,
    ) -> Self {
        Self {
            imm,
            rs1,
            rd,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::I) -> Self {
        let imm = ((instruction >> 20) & 0x0fff) as u16;
        let rs1 = decode_rs(instruction).0;
        let rd = decode_rd(instruction);
        Self {
            imm,
            rs1,
            rd,
            opcode,
        }
    }
}

pub struct S {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) width: opcode::Width,
}

impl S {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        width: opcode::Width,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            width,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, width: opcode::Width) -> Self {
        let (rs1, rs2) = decode_rs(instruction);

        let imm = (((instruction >> 19) & 0b0000_1111_1110_0000)
            | ((instruction >> 7) & 0b0000_0000_0001_1111)) as u16;

        let imm = sign_extend(imm, 12);

        Self {
            rs1,
            rs2,
            imm,
            width,
        }
    }
}

pub struct B {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) imm: u16,
    pub(crate) cmp_mode: opcode::Cmp,
}

impl B {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        cmp_mode: opcode::Cmp,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            cmp_mode,
        }
    }
}

pub struct U {
    imm: u32,
    rd: Option<RiscVRegister>,
    opcode: opcode::U,
}

impl U {
    pub(crate) fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::U) -> Self {
        Self { imm, rd, opcode }
    }
}

pub struct J {
    pub(crate) imm: u32,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::J,
}

impl J {
    pub(crate) fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::J) -> Self {
        Self { imm, rd, opcode }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::J) -> Self {
        let rd = decode_rd(instruction);
        // abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx -> 000a_dddd_dddd_cbbb_bbbb_bbb0
        let imm = ((instruction >> 11) & 0b0001_0000_0000_0000_0000_0000)
            | ((instruction >> 19) & 0b0000_0000_0000_0111_1111_1110)
            | ((instruction >> 9) & 0b0000_0000_0000_1000_0000_0000)
            | (instruction & 0b0000_1111_1111_0000_0000_0000);

        Self { imm, rd, opcode }
    }
}
