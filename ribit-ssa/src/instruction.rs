use std::fmt;

use ribit_core::{register, Width};

use crate::reference::Reference;
use crate::{
    ty, AnySource, Arg, BinOp, CmpKind, CommutativeBinOp, Id, Source, SourcePair, StackIndex, Type,
};

mod ext_int;
mod select;

pub use ext_int::ExtInt;
pub use select::Select;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    Arg { dest: Id, src: Arg },
    WriteStack { dest: StackIndex, src: Reference },
    ReadStack { dest: Id, src: StackIndex },
    ReadReg { dest: Id, base: Source<ty::I32>, src: register::RiscV },
    WriteReg { dest: register::RiscV, base: Source<ty::I32>, src: AnySource },
    ReadMem { dest: Id, src: AnySource, base: Source<ty::I32>, width: Width, sign_extend: bool },
    WriteMem { addr: Source<ty::I32>, src: AnySource, base: Source<ty::I32>, width: Width },
    BinOp { dest: Id, src: SourcePair, op: BinOp },
    Cmp { dest: Id, src: SourcePair, kind: CmpKind },
    CommutativeBinOp { dest: Id, src1: Reference, src2: AnySource, op: CommutativeBinOp },
    // todo: box this
    Select(Select),
    ExtInt(ExtInt),
    Fence,
}

impl Instruction {
    pub fn visit_arg_ids<F: FnMut(Id)>(&self, mut visit: F) {
        match self {
            Instruction::Arg { .. } | Instruction::ReadStack { .. } | Instruction::Fence => {}
            Instruction::ReadReg { base, .. } => {
                if let Source::Ref(it) = base {
                    visit(*it)
                }
            }
            Instruction::WriteReg { base, src, .. } | Instruction::ReadMem { base, src, .. } => {
                if let Source::Ref(it) = base {
                    visit(*it)
                }

                if let AnySource::Ref(src) = src {
                    visit(src.id)
                }
            }

            Instruction::WriteMem { addr, src, base, .. } => {
                if let Source::Ref(it) = addr {
                    visit(*it)
                }

                if let AnySource::Ref(it) = src {
                    visit(it.id)
                }

                if let Source::Ref(it) = base {
                    visit(*it)
                }
            }

            Instruction::BinOp { src, .. } | Instruction::Cmp { src, .. } => match src {
                SourcePair::RefRef(l, r) => {
                    visit(l.id);
                    visit(r.id);
                }
                SourcePair::RefConst(it, _) | SourcePair::ConstRef(_, it) => visit(it.id),
            },

            Instruction::CommutativeBinOp { src1, src2, .. } => {
                visit(src1.id);

                if let AnySource::Ref(it) = src2 {
                    visit(it.id)
                }
            }

            Instruction::Select(it) => {
                visit(it.cond.id);
                if let AnySource::Ref(it) = it.if_true {
                    visit(it.id)
                }

                if let AnySource::Ref(it) = it.if_false {
                    visit(it.id)
                }
            }

            Instruction::WriteStack { src, .. } => visit(src.id),
            Instruction::ExtInt(it) => visit(it.src.id),
        }
    }

    #[must_use]
    pub fn ty(&self) -> Type {
        match self {
            Instruction::Arg { dest: _, src: _ } => Type::I32,
            Instruction::WriteStack { dest: _, src: _ } => Type::Unit,
            Instruction::ReadStack { dest: _, src: _ } => Type::I32,
            Instruction::ReadReg { dest: _, base: _, src: _ } => Type::I32,
            Instruction::WriteReg { dest: _, base: _, src: _ } => Type::Unit,
            Instruction::ReadMem { dest: _, src: _, base: _, width: _, sign_extend: _ } => {
                Type::I32
            }
            Instruction::WriteMem { addr: _, src: _, base: _, width: _ } => Type::Unit,
            Instruction::BinOp { dest: _, src, op: _ } => {
                let ty = src.lhs().ty();

                assert_eq!(ty, src.rhs().ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Instruction::CommutativeBinOp { dest: _, src1, src2, op: _ } => {
                let ty = src1.ty;

                assert_eq!(ty, src2.ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Instruction::Cmp { dest: _, src, kind: _ } => {
                let ty = src.lhs().ty();

                assert_eq!(ty, src.rhs().ty());

                Type::Boolean
            }

            Instruction::Select(it) => it.ty(),

            Instruction::Fence => Type::Unit,
            Instruction::ExtInt(it) => it.ty(),
        }
    }

    #[must_use]
    pub fn id(&self) -> Option<Id> {
        match self {
            Self::ReadReg { dest, .. }
            | Self::ReadMem { dest, .. }
            | Self::ReadStack { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::Arg { dest, .. }
            | Self::CommutativeBinOp { dest, .. }
            | Self::BinOp { dest, .. } => Some(*dest),

            Self::Select(it) => Some(it.id()),
            Self::ExtInt(it) => Some(it.id()),

            Self::WriteStack { .. }
            | Self::WriteReg { .. }
            | Self::WriteMem { .. }
            | Self::Fence => None,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReadReg { dest, base, src } => write!(f, "{} = x({}){}", dest, base, src.get()),

            Self::ReadMem { dest, src, base, width, sign_extend } => match sign_extend {
                true => write!(f, "{} = signed {} m({}){}", dest, width, base, src),
                false => write!(f, "{} = {} m({}){}", dest, width, base, src),
            },

            Self::ReadStack { dest, src } => write!(f, "{} = {}", dest, src),

            Self::WriteReg { base, dest, src } => write!(f, "x({}){} = {}", base, dest.get(), src),

            Self::WriteMem { base, addr, src, width } => {
                write!(f, "m({}){} = {} {}", base, addr, width, src)
            }

            Self::WriteStack { dest, src } => write!(f, "{} = {}", dest, src),

            Self::Arg { dest, src } => write!(f, "{} = args[{}]", dest, *src as u8),

            Self::CommutativeBinOp { dest, src1, src2, op } => write!(
                f,
                "{dest} = {op} {src1}, {src2}",
                dest = dest,
                op = op,
                src1 = src1,
                src2 = src2
            ),

            Self::BinOp { dest, src, op } => write!(
                f,
                "{dest} = {op} {src1}, {src2}",
                dest = dest,
                op = op,
                src1 = src.lhs(),
                src2 = src.rhs()
            ),
            Self::Cmp { dest, src, kind } => {
                write!(f, "{} = cmp {} {}, {}", dest, kind, src.lhs(), src.rhs())
            }

            Self::Select(it) => it.fmt(f),
            Self::Fence => write!(f, "fence"),
            Self::ExtInt(it) => it.fmt(f),
        }
    }
}
