use core::fmt;
use core::ops::ControlFlow;

use crate::register::RiscV as RiscVRegister;
use crate::{Never, Width, opcode};

// todo: decide on visibility of fields
#[derive(Debug)]
pub struct Info<K: InstructionKind = Any> {
    pub instruction: Instruction<K>,
    pub len: u32,
}

impl Info {
    pub const fn into_controlflow(self) -> ControlFlow<Info<Terminal>, Info<NonTerminal>> {
        match self.instruction.into_controlflow() {
            ControlFlow::Continue(instruction) => {
                ControlFlow::Continue(Info { instruction, len: self.len })
            }
            ControlFlow::Break(it) => ControlFlow::Break(Info { instruction: it, len: self.len }),
        }
    }
}

impl<K: InstructionKind> Info<K> {
    #[must_use]
    pub const fn new(instruction: Instruction<K>, len: u32) -> Self {
        Self { instruction, len }
    }
}

impl<T: Into<Instruction>> From<T> for Info {
    fn from(value: T) -> Self {
        Self { instruction: value.into(), len: 4 }
    }
}

pub trait InstructionKind {
    type R: fmt::Debug;
    type I: fmt::Debug;
    type IJump: fmt::Debug;
    type IMem: fmt::Debug;
    type S: fmt::Debug;
    type B: fmt::Debug;
    type U: fmt::Debug;
    type J: fmt::Debug;
    type Sys: fmt::Debug;
}

#[derive(Debug)]
pub struct Any;

impl InstructionKind for Any {
    type R = R;
    type I = I;
    type IJump = IJump;
    type IMem = IMem;
    type S = S;
    type B = B;
    type U = U;
    type J = J;
    type Sys = Sys;
}

#[derive(Debug)]
pub struct Terminal;

impl InstructionKind for Terminal {
    type R = Never;
    type I = Never;
    type IJump = IJump;
    type IMem = Never;
    type S = Never;
    type B = B;
    type U = Never;
    type J = J;
    type Sys = Sys;
}

#[derive(Debug)]
pub struct NonTerminal;

impl InstructionKind for NonTerminal {
    type R = R;
    type I = I;
    type IJump = Never;
    type IMem = IMem;
    type S = S;
    type B = Never;
    type U = U;
    type J = Never;
    type Sys = Never;
}

#[derive(Debug)]
pub enum Instruction<K: InstructionKind = Any> {
    R(K::R),
    I(K::I),
    IJump(K::IJump),
    IMem(K::IMem),
    S(K::S),
    B(K::B),
    U(K::U),
    J(K::J),
    Sys(K::Sys),
}

impl Instruction {
    pub const fn into_controlflow(
        self,
    ) -> ControlFlow<Instruction<Terminal>, Instruction<NonTerminal>> {
        use ControlFlow::{Break, Continue};
        match self {
            Self::R(it) => Continue(Instruction::R(it)),
            Self::I(it) => Continue(Instruction::I(it)),
            Self::IMem(it) => Continue(Instruction::IMem(it)),
            Self::S(it) => Continue(Instruction::S(it)),
            Self::U(it) => Continue(Instruction::U(it)),
            Self::IJump(it) => Break(Instruction::IJump(it)),
            Self::B(it) => Break(Instruction::B(it)),
            Self::J(it) => Break(Instruction::J(it)),
            Self::Sys(it) => Break(Instruction::Sys(it)),
        }
    }
}

impl<K: InstructionKind> Instruction<K> {
    #[must_use]
    pub const fn is_terminator(&self) -> bool {
        matches!(self, Self::J(_) | Self::B(_) | Self::Sys(_) | Self::IJump(_))
    }
}

#[derive(Debug)]
pub struct Sys {
    pub opcode: opcode::RSys,
}

impl Sys {
    #[must_use]
    #[inline(always)]
    pub const fn new(opcode: opcode::RSys) -> Self {
        Self { opcode }
    }
}

impl From<Sys> for Instruction {
    #[inline(always)]
    fn from(value: Sys) -> Self {
        Self::Sys(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::R,
    ) -> Self {
        Self { rs1, rs2, rd, opcode }
    }
}

impl From<R> for Instruction {
    #[inline(always)]
    fn from(value: R) -> Self {
        Self::R(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::I,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

impl From<I> for Instruction {
    #[inline(always)]
    fn from(value: I) -> Self {
        Self::I(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::IJump,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

impl From<IJump> for Instruction {
    #[inline(always)]
    fn from(value: IJump) -> Self {
        Self::IJump(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::IMem,
    ) -> Self {
        Self { imm, rs1, rd, opcode }
    }
}

impl From<IMem> for Instruction {
    #[inline(always)]
    fn from(value: IMem) -> Self {
        Self::IMem(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        width: Width,
    ) -> Self {
        Self { imm, rs1, rs2, width }
    }
}

impl From<S> for Instruction {
    #[inline(always)]
    fn from(value: S) -> Self {
        Self::S(value)
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
    #[must_use]
    #[inline(always)]
    pub const fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        cmp_mode: opcode::Cmp,
    ) -> Self {
        Self { rs1, rs2, imm, cmp_mode }
    }
}

impl From<B> for Instruction {
    #[inline(always)]
    fn from(value: B) -> Self {
        Self::B(value)
    }
}

#[derive(Debug)]
pub struct U {
    pub imm: u32,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::U,
}

impl U {
    #[must_use]
    #[inline(always)]
    pub const fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::U) -> Self {
        Self { imm, rd, opcode }
    }
}

impl From<U> for Instruction {
    #[inline(always)]
    fn from(value: U) -> Self {
        Self::U(value)
    }
}

#[derive(Debug)]
pub struct J {
    pub imm: u32,
    pub rd: Option<RiscVRegister>,
    pub opcode: opcode::J,
}

impl J {
    #[must_use]
    #[inline(always)]
    pub const fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::J) -> Self {
        Self { imm, rd, opcode }
    }
}

impl From<J> for Instruction {
    #[inline(always)]
    fn from(value: J) -> Self {
        Self::J(value)
    }
}
