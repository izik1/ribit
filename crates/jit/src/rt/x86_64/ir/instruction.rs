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
    pub args: UnaryArgs,
    pub kind: UnaryOpKind,
}

impl UnaryOp {
    pub fn not(args: UnaryArgs) -> Self {
        Self { args, kind: UnaryOpKind::Not }
    }

    pub fn neg(args: UnaryArgs) -> Self {
        Self { args, kind: UnaryOpKind::Neg }
    }

    pub fn inc(args: UnaryArgs) -> Self {
        Self { args, kind: UnaryOpKind::Inc }
    }

    pub fn dec(args: UnaryArgs) -> Self {
        Self { args, kind: UnaryOpKind::Dec }
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
    pub args: MaybeTiedBinaryArgs,
    pub kind: BinaryOpKind,
}

pub enum Instruction {
    Unary(UnaryOp),

    Mov { args: UntiedBinaryArgs, width: Width },
    MovZx32 { args: UntiedBinaryArgs, src_width: Width },
    MovSx32 { args: UntiedBinaryArgs, src_width: Width },

    // HACK: these are just for returns, properly support 64-bit instructions everywhere please!
    Mov64 { args: UntiedBinaryArgs64 },
    Or64 { args: TiedBinaryArgs64 },

    ShiftImm { args: UnaryArgs, kind: ShiftKind, src2: u8 },
    // src2 is always cl.
    ShiftCl { args: UnaryArgs, kind: ShiftKind },

    // fixme: handle FLAGS right.
    Cmp { args: UntiedBinaryArgs },
    // fixme: handle FLAGS right.
    Test { args: UntiedBinaryArgs },

    // fixme: handle FLAGs right.
    SetCC { dest: Register, condition: CmpKind },
    // fixme: handle FLAGs right.
    // note: things will absolutely explode if you try to `CMovCC` with an immediate (there's no instruction for that)
    CMovCC { args: UntiedBinaryArgs, condition: CmpKind },

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

pub enum MaybeTiedBinaryArgs {
    RegReg { dest: Register, src1: Register, src2: Register },
    RegMem { dest: Register, src1: Register, src2: Memory },
    RegImm { dest: Register, src1: Register, src2: u32 },
    MemReg { lhs: Memory, rhs: Register },
    MemImm { lhs: Memory, rhs: u32 },
}

pub enum TiedBinaryArgs64 {
    RegReg { dest: Register, src1: Register, src2: Register },
    RegImm { dest: Register, src1: Register, src2: u64 },
}

pub enum UntiedBinaryArgs {
    RegReg(Register, Register),
    RegMem(Register, Memory),
    RegImm(Register, u32),
    MemReg(Memory, Register),
    MemImm(Memory, u32),
}

pub enum UntiedBinaryArgs64 {
    RegReg(Register, Register),
    RegImm(Register, u64),
}

pub enum UnaryArgs {
    // dest is tied to src.
    Reg { dest: Register, src: Register },
    Mem(Memory),
}

pub enum Terminator {
    Ret,
}
