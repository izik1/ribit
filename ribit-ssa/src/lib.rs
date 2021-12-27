#![forbid(unsafe_code)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]

use std::convert::TryFrom;
use std::fmt;

use reference::{Reference, TypedRef};
use ribit_core::opcode;

pub mod analysis;
mod block;
pub mod eval;
mod id;
mod instruction;
pub mod lower;
pub mod opt;
pub mod reference;
mod ty;

pub use block::{Block, BlockDisplay};
pub use id::{Id, IdAllocator};
pub use instruction::Instruction;
use ty::ConstTy;
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
        if redzone {
            (i32::from(self.0) + 1) * -4
        } else {
            i32::from(self.0) * 4
        }
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

/// Like source, but for specific types.
///
/// Such as `Int` or `bool`.
#[derive(PartialEq, Eq)]
pub enum TypedSource<T: ConstTy> {
    /// A constant of type `T`
    Const(T::Const),
    /// A reference to a value of type `T`
    Ref(Id),
}

impl<T: ConstTy> Copy for TypedSource<T> where T::Const: Copy {}

impl<T: ConstTy> Clone for TypedSource<T>
where
    T::Const: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Const(arg0) => Self::Const(arg0.clone()),
            Self::Ref(arg0) => Self::Ref(*arg0),
        }
    }
}

impl<T: ConstTy> TypedSource<T> {
    #[must_use]
    pub fn constant(self) -> Option<T::Const> {
        match self {
            Self::Const(v) => Some(v),
            Self::Ref(_) => None,
        }
    }

    #[must_use]
    pub fn reference(self) -> Option<TypedRef<T>> {
        match self {
            Self::Ref(it) => Some(TypedRef::new(it)),
            Self::Const(_) => None,
        }
    }
}

impl<T: ConstTy> TypedSource<T>
where
    T::Const: Into<Constant>,
{
    pub fn upcast(self) -> AnySource {
        match self {
            Self::Const(konst) => AnySource::Const(konst.into()),
            Self::Ref(id) => AnySource::Ref(Reference { ty: T::TY, id }),
        }
    }
}

impl<T: ConstTy> fmt::Display for TypedSource<T>
where
    T::Const: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(v) => v.fmt(f),
            Self::Ref(r) => r.fmt(f),
        }
    }
}

impl<T: ConstTy> fmt::Debug for TypedSource<T>
where
    T::Const: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(arg0) => f.debug_tuple("Const").field(arg0).finish(),
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum AnySource {
    Const(Constant),
    Ref(Reference),
}

impl AnySource {
    #[must_use]
    pub fn constant(self) -> Option<Constant> {
        match self {
            Self::Const(v) => Some(v),
            Self::Ref(_) => None,
        }
    }

    #[must_use]
    pub fn reference(self) -> Option<Reference> {
        match self {
            Self::Ref(r) => Some(r),
            Self::Const(_) => None,
        }
    }

    #[must_use]
    pub fn ty(self) -> Type {
        match self {
            AnySource::Const(konst) => konst.ty(),
            AnySource::Ref(r) => r.ty,
        }
    }
}

impl fmt::Display for AnySource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(v) => v.fmt(f),
            Self::Ref(r) => r.fmt(f),
        }
    }
}

/// A pair of constants/references that specifically *can't* be `const,const`
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SourcePair {
    RefRef(Reference, Reference),
    RefConst(Reference, Constant),
    ConstRef(Constant, Reference),
}

impl SourcePair {
    #[must_use]
    pub fn lhs(self) -> AnySource {
        match self {
            Self::RefRef(lhs, _) | Self::RefConst(lhs, _) => AnySource::Ref(lhs),
            Self::ConstRef(lhs, _) => AnySource::Const(lhs),
        }
    }

    #[must_use]
    pub fn rhs(self) -> AnySource {
        match self {
            Self::RefRef(_, rhs) | Self::ConstRef(_, rhs) => AnySource::Ref(rhs),
            Self::RefConst(_, rhs) => AnySource::Const(rhs),
        }
    }
}

impl TryFrom<(AnySource, AnySource)> for SourcePair {
    type Error = (Constant, Constant);

    fn try_from(value: (AnySource, AnySource)) -> Result<Self, Self::Error> {
        match value {
            (AnySource::Const(lhs), AnySource::Const(rhs)) => Err((lhs, rhs)),
            (AnySource::Const(lhs), AnySource::Ref(rhs)) => Ok(Self::ConstRef(lhs, rhs)),
            (AnySource::Ref(lhs), AnySource::Const(rhs)) => Ok(Self::RefConst(lhs, rhs)),
            (AnySource::Ref(lhs), AnySource::Ref(rhs)) => Ok(Self::RefRef(lhs, rhs)),
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
    Ret { addr: AnySource, code: AnySource },
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ret { addr, code } => write!(f, "ret {}, {}", code, addr),
        }
    }
}

pub fn update_reference(src: &mut AnySource, old: Id, new: Id) {
    match src {
        AnySource::Ref(r) if r.id == old => r.id = new,
        _ => {}
    }
}

pub fn update_typed_reference<T: ConstTy>(src: &mut TypedSource<T>, old: Id, new: Id) {
    match src {
        TypedSource::Ref(r) if *r == old => *r = new,
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
                update_reference(base, old, new);
            }

            Instruction::WriteStack { dest: _, src } => {
                if src.id == old {
                    src.id = new;
                }
            }

            Instruction::WriteReg { src, base, .. } | Instruction::ReadMem { src, base, .. } => {
                update_reference(base, old, new);
                update_reference(src, old, new);
            }

            Instruction::WriteMem { addr, src, base, .. } => {
                update_reference(base, old, new);
                update_reference(addr, old, new);
                update_reference(src, old, new);
            }

            Instruction::Select { cond, if_true, if_false, dest: _ } => {
                if cond.id == old {
                    cond.id = new;
                }

                update_reference(if_true, old, new);
                update_reference(if_false, old, new);
            }

            Instruction::ExtInt { src, .. } => {
                if src.id == old {
                    src.id = new;
                }
            }
        }

        match &mut graph.terminator {
            Terminator::Ret { addr, code } => {
                update_reference(addr, old, new);
                update_reference(code, old, new);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use ribit_core::opcode::Cmp;
    use ribit_core::{opcode, register};

    use crate::{lower, Block};

    pub const MEM_SIZE: u32 = 0x1000000;

    pub fn max_fn() -> Block {
        use ribit_core::instruction;

        let mut ctx = lower::Context::new(1024, MEM_SIZE);

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X11),
                opcode::R::ADD,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                31,
                Some(register::RiscV::X11),
                Some(register::RiscV::X12),
                opcode::I::SRLI,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X11),
                Some(register::RiscV::X12),
                Some(register::RiscV::X11),
                opcode::R::AND,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X10),
                opcode::R::ADD,
            )),
            4,
        );

        lower::terminal(
            ctx,
            instruction::Instruction::IJump(instruction::IJump::new(
                0,
                Some(register::RiscV::X1),
                None,
                opcode::IJump::JALR,
            )),
            2,
        )
    }

    pub fn min_fn() -> Block {
        use ribit_core::instruction;

        let mut ctx = lower::Context::new(1024, MEM_SIZE);
        // fn min(x: u32, y: u32) -> u32 {
        //     let tmp0 = (x < y) as u32;
        //     let tmp1 = (0 - tmp0 as i32) as u32;
        //     let tmp2 = x ^ y;
        //     let tmp3 = tmp2 & tmp1;
        //     y ^ tmp3
        // }
        // X10 -> x
        // X11 -> y

        // `let tmp0 = (x < y) as u32`
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X12),
                opcode::R::SCond(Cmp::Ltu),
            )),
            4,
        );

        // `let tmp1 = (0 - tmp0 as i32) as u32`
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                None,
                Some(register::RiscV::X12),
                Some(register::RiscV::X12),
                opcode::R::SUB,
            )),
            4,
        );

        // `let tmp2 = x ^ y;`
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X10),
                opcode::R::XOR,
            )),
            4,
        );

        // `let tmp3 = tmp2 & tmp1;`
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X12),
                Some(register::RiscV::X10),
                opcode::R::AND,
            )),
            4,
        );

        // `y ^ tmp3`
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X10),
                opcode::R::XOR,
            )),
            4,
        );

        lower::terminal(
            ctx,
            instruction::Instruction::IJump(instruction::IJump::new(
                0,
                Some(register::RiscV::X1),
                None,
                opcode::IJump::JALR,
            )),
            2,
        )
    }
}
