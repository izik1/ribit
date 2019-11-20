use crate::decode::{decode_rd, decode_rs, sign_extend};

use crate::opcode;
use crate::register::RiscVRegister;
pub enum Instruction {
    R(RTypeInstruction),
    I(ITypeInstruction),
    S(STypeInstruction),
    B(BTypeInstruction),
    U(UTypeInstruction),
    J(JTypeInstruction),
}

#[derive(Debug)]
pub struct RTypeInstruction {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::R,
}

impl RTypeInstruction {
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

pub struct ITypeInstruction {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::I,
}

impl ITypeInstruction {
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

pub struct STypeInstruction {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) opcode: opcode::S,
}

impl STypeInstruction {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        opcode: opcode::S,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::S) -> Self {
        let (rs1, rs2) = decode_rs(instruction);

        let imm = (((instruction >> 19) & 0b0000_1111_1110_0000)
            | ((instruction >> 7) & 0b0000_0000_0001_1111)) as u16;

        let imm = sign_extend(imm, 12);

        Self {
            rs1,
            rs2,
            imm,
            opcode,
        }
    }
}

pub struct BTypeInstruction {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) imm: u16,
    pub(crate) opcode: opcode::B,
}

impl BTypeInstruction {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        opcode: opcode::B,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            opcode,
        }
    }
}

pub struct UTypeInstruction {
    imm: u32,
    rd: Option<RiscVRegister>,
    opcode: opcode::U,
}

impl UTypeInstruction {
    pub(crate) fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::U) -> Self {
        Self { imm, rd, opcode }
    }
}

pub struct JTypeInstruction {
    pub(crate) imm: u32,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::J,
}

impl JTypeInstruction {
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
