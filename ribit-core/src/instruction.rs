use crate::register::RiscV as RiscVRegister;
use crate::{opcode, Width};

// todo: decide on visibility of fields

pub struct Info {
    pub instruction: Instruction,
    pub len: u32,
}

impl Info {
    #[must_use]
    pub fn new(instruction: Instruction, len: u32) -> Self {
        Self { instruction, len }
    }
}

#[derive(Debug)]
pub enum Instruction {
    R(R),
    I(I),
    IJump(IJump),
    IMem(IMem),
    S(S),
    B(B),
    U(U),
    J(J),
    Sys(Sys),
}

impl Instruction {
    #[must_use]
    pub fn is_terminator(&self) -> bool {
        match self {
            Self::J(_) | Self::B(_) | Self::Sys(_) | Self::IJump(_) => true,

            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Sys {
    pub opcode: opcode::RSys,
}

impl Sys {
    pub fn new(opcode: opcode::RSys) -> Self {
        Self { opcode }
    }
}

#[derive(Debug)]
pub struct R {
    pub rs1: Option<RiscVRegister>,
    pub rs2: Option<RiscVRegister>,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::R,
}

impl R {
    pub fn new(
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::R,
    ) -> Self {
        Self { rs1, rs2, rd, opcode }
    }
}

#[derive(Debug)]
pub struct I {
    pub imm: u16,
    pub rs1: Option<RiscVRegister>,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::I,
}

impl I {
    pub fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::I,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

#[derive(Debug)]
pub struct IJump {
    pub imm: u16,
    pub rs1: Option<RiscVRegister>,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::IJump,
}

impl IJump {
    pub fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::IJump,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

#[derive(Debug)]
pub struct IMem {
    pub imm: u16,
    pub rs1: Option<RiscVRegister>,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::IMem,
}

impl IMem {
    pub fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::IMem,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

#[derive(Debug)]
pub struct S {
    pub imm: u16,
    pub rs1: Option<RiscVRegister>,
    pub rs2: Option<RiscVRegister>,
    pub width: Width,
}

impl S {
    pub fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        width: Width,
    ) -> Self {
        Self { imm, rs1, rs2, width }
    }
}

#[derive(Debug)]
pub struct B {
    pub rs1: Option<RiscVRegister>,
    pub rs2: Option<RiscVRegister>,
    pub imm: u16,
    pub cmp_mode: opcode::Cmp,
}

impl B {
    pub fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        cmp_mode: opcode::Cmp,
    ) -> Self {
        Self { imm, rs1, rs2, cmp_mode }
    }
}

#[derive(Debug)]
pub struct U {
    pub imm: u32,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::U,
}

impl U {
    pub fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::U) -> Self {
        Self { imm, rd, opcode }
    }
}

#[derive(Debug)]
pub struct J {
    pub imm: u32,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::J,
}

impl J {
    pub fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::J) -> Self {
        Self { imm, rd, opcode }
    }
}
