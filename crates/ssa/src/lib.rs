#![forbid(unsafe_code)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]

use std::fmt;

use instruction::CmpArgs;
use reference::Ref;
use ribit_core::{opcode, ReturnCode};
use ty::ConstTy;

pub mod analysis;
mod block;
pub mod eval;
pub mod icmp;
mod id;
pub mod instruction;
pub mod lower;
pub mod opt;
pub mod reference;
mod source;
pub mod ty;

#[cfg(test)]
mod tests;

pub use block::{Block, BlockDisplay};
pub use id::{Id, IdAllocator};
pub use instruction::Instruction;
pub use source::{AnySource, Source, SourcePair};
pub use ty::{Bitness, Constant, Type};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, PartialOrd, Ord)]
pub struct StackIndex(pub u8);

impl fmt::Display for StackIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "!s{}", self.0)
    }
}

impl StackIndex {
    #[must_use]
    pub fn offset(self, redzone: bool) -> i32 {
        if redzone { (i32::from(self.0) + 1) * -4 } else { i32::from(self.0) * 4 }
    }
}

// bitpacked: [(signed/unsigned):2, (lt/gt):1, (eq/no):0]
#[repr(u8)]
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Inequality {
    Ult = 0b000,
    Ule = 0b001,
    Ugt = 0b010,
    Uge = 0b011,
    Slt = 0b100,
    Sle = 0b101,
    Sgt = 0b110,
    Sge = 0b111,
}

impl Inequality {
    const SIGNED_UNSIGNED_BIT: u8 = 2;
    const LESS_GREATER_BIT: u8 = 1;
    const EQUAL_NOT_EQUAL_BIT: u8 = 0;

    #[inline(always)]
    pub const fn is_signed(self) -> bool {
        (self as u8 & (1 << Inequality::SIGNED_UNSIGNED_BIT)) != 0
    }

    #[inline(always)]
    pub const fn include_eq(self) -> bool {
        (self as u8 & (1 << Inequality::EQUAL_NOT_EQUAL_BIT)) != 0
    }

    #[inline(always)]
    pub const fn is_less(self) -> bool {
        (self as u8 & (1 << Inequality::LESS_GREATER_BIT)) != 0
    }

    #[inline(always)]
    pub const fn is_greater(self) -> bool {
        (self as u8 & (1 << Inequality::LESS_GREATER_BIT)) != 0
    }

    /// Turns `a {ineq} b` to `b {ineq} a` without changing the result.
    /// equalness is preserved, signedness is preserved, and `<` swaps with `>`
    ///
    /// Reverse might've been a better name, but that's taken by `cmp::Ordering` and means something completely different.
    pub fn swap(self) -> Self {
        let mask: u8 = const { 1 << Self::LESS_GREATER_BIT };

        // this optimizes better than just swapping variants around because the compiler can prove the `unreachable!` truly is unreachable.
        // so we go enum -> u8 -> u8 -> enum
        // and since the bits match the enum variants we really go enum -> enum.
        match (self as u8) ^ mask {
            0b000 => Self::Ult,
            0b001 => Self::Ule,
            0b010 => Self::Ugt,
            0b011 => Self::Uge,
            0b100 => Self::Slt,
            0b101 => Self::Sle,
            0b110 => Self::Sgt,
            0b111 => Self::Sge,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum CmpKind {
    Eq,
    Ne,
    Inequality(Inequality),
}

impl CmpKind {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
            Self::Inequality(Inequality::Ult) => "ult",
            Self::Inequality(Inequality::Ule) => "ule",
            Self::Inequality(Inequality::Ugt) => "ugt",
            Self::Inequality(Inequality::Uge) => "uge",
            Self::Inequality(Inequality::Slt) => "slt",
            Self::Inequality(Inequality::Sle) => "sle",
            Self::Inequality(Inequality::Sgt) => "sgt",
            Self::Inequality(Inequality::Sge) => "sge",
        }
    }

    pub fn swap(self) -> Self {
        match self {
            // equality is symmetric.
            Self::Eq => Self::Eq,
            Self::Ne => Self::Ne,
            Self::Inequality(inequality) => Self::Inequality(inequality.swap()),
        }
    }
}

impl fmt::Display for CmpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for CmpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CmpKind").field(&self.as_str()).finish()
    }
}

impl From<opcode::Cmp> for CmpKind {
    fn from(cmp: opcode::Cmp) -> Self {
        match cmp {
            opcode::Cmp::Eq => Self::Eq,
            opcode::Cmp::Ne => Self::Ne,
            opcode::Cmp::Lt => Self::Inequality(Inequality::Slt),
            opcode::Cmp::Ltu => Self::Inequality(Inequality::Ult),
            opcode::Cmp::Ge => Self::Inequality(Inequality::Sge),
            opcode::Cmp::Geu => Self::Inequality(Inequality::Uge),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum CommutativeBinOp {
    And,
    Add,
    Or,
    Xor,
}

impl CommutativeBinOp {
    // all current commutative binops *are* associative (iff we're talking about integers.)
    // But theoretically we could implement some commutative binop that isn't associative.
    #[must_use]
    pub const fn is_associative(self) -> bool {
        match self {
            CommutativeBinOp::And
            | CommutativeBinOp::Add
            | CommutativeBinOp::Or
            | CommutativeBinOp::Xor => true,
        }
    }
}

impl fmt::Display for CommutativeBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::And => f.write_str("and"),
            Self::Add => f.write_str("add"),
            Self::Or => f.write_str("or"),
            Self::Xor => f.write_str("xor"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ShiftOp {
    Sll,
    Srl,
    Sra,
}

impl fmt::Display for ShiftOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Sll => f.write_str("sll"),
            Self::Srl => f.write_str("srl"),
            Self::Sra => f.write_str("sra"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Arg {
    Register = 0,
    Memory = 1,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Terminator {
    Ret { addr: Source<ty::I32>, code: ReturnCode },
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ret { addr, code } => write!(f, "ret {}, {addr}", *code as u32),
        }
    }
}

pub fn update_reference(src: &mut AnySource, old: Id, new: Id) {
    match src {
        AnySource::Ref(r) if r.id == old => r.id = new,
        _ => {}
    }
}

pub fn update_typed_reference<T: ConstTy>(src: &mut Source<T>, old: Id, new: Id) {
    match src {
        Source::Ref(r) if *r == old => *r = new,
        _ => {}
    }
}

// hack: do something better than `start_from`
pub fn update_references(graph: &mut Block, start_from: usize, old: Id, new: Id) {
    for instr in &mut graph.instructions[start_from..] {
        match instr {
            Instruction::Fence | Instruction::Arg { .. } | Instruction::ReadStack { .. } => {}

            Instruction::ShiftOp { dest: _, src, .. } => match src {
                SourcePair::RefRef(lhs, rhs) => {
                    if lhs.id == old {
                        lhs.id = new;
                    }

                    if rhs.id == old {
                        rhs.id = new;
                    }
                }
                SourcePair::RefConst(it, _) | SourcePair::ConstRef(_, it) => {
                    if it.id == old {
                        it.id = new;
                    }
                }
            },

            Instruction::CommutativeBinOp { dest: _, src1: reference, src2: source, .. }
            | Instruction::Sub { dest: _, src1: source, src2: reference }
            | Instruction::Cmp {
                dest: _,
                args: CmpArgs { src1: reference, src2: source, kind: _ },
            } => {
                if reference.id == old {
                    reference.id = new;
                }

                update_reference(source, old, new);
            }

            Instruction::ReadReg { dest: _, base, .. } => {
                update_typed_reference(base, old, new);
            }

            Instruction::WriteStack { dest: _, src } => {
                if src.id == old {
                    src.id = new;
                }
            }

            Instruction::WriteReg { src, base, .. } | Instruction::ReadMem { src, base, .. } => {
                update_typed_reference(base, old, new);
                update_reference(src, old, new);
            }

            Instruction::WriteMem { addr, src, base, .. } => {
                update_typed_reference(base, old, new);

                update_typed_reference(addr, old, new);
                update_reference(src, old, new);
            }

            Instruction::Select(it) => {
                if it.cond.id == old {
                    it.cond.id = new;
                }

                update_reference(&mut it.if_true, old, new);
                update_reference(&mut it.if_false, old, new);
            }

            Instruction::ExtInt(it) => {
                if it.src.id == old {
                    it.src.id = new;
                }
            }
        }

        match &mut graph.terminator {
            Terminator::Ret { addr, .. } => {
                update_typed_reference(addr, old, new);
            }
        }
    }
}
