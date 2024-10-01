use ribit_core::Width;
use ribit_ssa::{CmpKind, ShiftKind};

use super::{Memory, Register};

pub enum UnaryOpKind {
    Neg,
    Not,
    Inc,
    Dec,
}

pub struct UnaryOp {
    pub operand: UnaryOperand,
    pub kind: UnaryOpKind,
}

impl UnaryOp {
    pub fn not(operand: UnaryOperand) -> Self {
        Self { operand, kind: UnaryOpKind::Not }
    }

    pub fn neg(operand: UnaryOperand) -> Self {
        Self { operand, kind: UnaryOpKind::Neg }
    }

    pub fn inc(operand: UnaryOperand) -> Self {
        Self { operand, kind: UnaryOpKind::Inc }
    }

    pub fn dec(operand: UnaryOperand) -> Self {
        Self { operand, kind: UnaryOpKind::Dec }
    }
}

pub enum BinaryOpKind {
    Sub,
    And,
    Add,
    Or,
    Xor,
}

pub struct BinaryOp {
    pub operand: MaybeTiedBinaryOperand,
    pub kind: BinaryOpKind,
}

pub enum Instruction {
    Unary(UnaryOp),

    Mov { op: UntiedBinaryOperand, width: Width },
    MovZx32 { op: UntiedBinaryOperand, src_width: Width },
    MovSx32 { op: UntiedBinaryOperand, src_width: Width },

    // HACK: these are just for returns, properly support 64-bit instructions everywhere please!
    Mov64 { op: UntiedBinaryOp64 },
    Or64 { op: TiedBinaryOp64 },

    ShiftImm { unary: UnaryOperand, kind: ShiftKind, src2: u8 },
    // src2 is always cl.
    ShiftCl { unary: UnaryOperand, kind: ShiftKind },

    // fixme: handle FLAGS right.
    Cmp { op: UntiedBinaryOperand },
    // fixme: handle FLAGS right.
    Test { op: UntiedBinaryOperand },

    // fixme: handle FLAGs right.
    SetCC { dest: Register, condition: CmpKind },
    // fixme: handle FLAGs right.
    // note: things will absolutely explode if you try to `CMovCC` with an immediate (there's no instruction for that)
    CMovCC { op: UntiedBinaryOperand, condition: CmpKind },

    Binary(BinaryOp),
}

impl From<UnaryOp> for Instruction {
    fn from(value: UnaryOp) -> Self {
        Self::Unary(value)
    }
}

impl From<BinaryOp> for Instruction {
    fn from(value: BinaryOp) -> Self {
        Self::Binary(value)
    }
}

pub enum MaybeTiedBinaryOperand {
    RegReg { dest: Register, src1: Register, src2: Register },
    RegMem { dest: Register, src1: Register, src2: Memory },
    RegImm { dest: Register, src1: Register, src2: u32 },
    MemReg { lhs: Memory, rhs: Register },
    MemImm { lhs: Memory, rhs: u32 },
}

pub enum TiedBinaryOp64 {
    RegReg { dest: Register, src1: Register, src2: Register },
    RegImm { dest: Register, src1: Register, src2: u64 },
}

pub enum UntiedBinaryOperand {
    RegReg(Register, Register),
    RegMem(Register, Memory),
    RegImm(Register, u32),
    MemReg(Memory, Register),
    MemImm(Memory, u32),
}

pub enum UntiedBinaryOp64 {
    RegReg(Register, Register),
    RegImm(Register, u64),
}

pub enum UnaryOperand {
    // dest is tied to src.
    Reg { dest: Register, src: Register },
    Mem(Memory),
}

pub enum Terminator {
    Ret,
}
