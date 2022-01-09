#![forbid(unsafe_code)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]

use std::fmt;

use reference::Ref;
use ribit_core::{opcode, ReturnCode};
use ty::ConstTy;

pub mod analysis;
mod block;
pub mod eval;
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum CmpKind {
    Eq,
    Ne,
    Sge,
    Sl,
    Uge,
    Ul,
}

impl From<opcode::Cmp> for CmpKind {
    fn from(cmp: opcode::Cmp) -> Self {
        match cmp {
            opcode::Cmp::Eq => Self::Eq,
            opcode::Cmp::Ne => Self::Ne,
            opcode::Cmp::Lt => Self::Sl,
            opcode::Cmp::Ltu => Self::Ul,
            opcode::Cmp::Ge => Self::Sge,
            opcode::Cmp::Geu => Self::Uge,
        }
    }
}

impl fmt::Display for CmpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Eq => write!(f, "EQ"),
            Self::Ne => write!(f, "NE"),
            Self::Sge => write!(f, "SGE"),
            Self::Sl => write!(f, "SL"),
            Self::Uge => write!(f, "UGE"),
            Self::Ul => write!(f, "UL"),
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
pub enum BinOp {
    Sll,
    Srl,
    Sra,
    Sub,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Sll => f.write_str("sll"),
            Self::Srl => f.write_str("srl"),
            Self::Sra => f.write_str("sra"),
            Self::Sub => f.write_str("sub"),
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
            Self::Ret { addr, code } => write!(f, "ret {}, {}", *code as u32, addr),
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

            Instruction::BinOp { dest: _, src, .. } | Instruction::Cmp { dest: _, src, .. } => {
                match src {
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
                }
            }

            Instruction::CommutativeBinOp { dest: _, src1, src2, .. } => {
                if src1.id == old {
                    src1.id = new;
                }

                update_reference(src2, old, new);
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
