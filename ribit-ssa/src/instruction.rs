use std::fmt;

use ribit_core::{register, Width};

use crate::reference::Reference;
use crate::{
    ty, AnySource, Arg, CmpKind, CommutativeBinOp, Id, ShiftOp, Source, SourcePair, StackIndex,
    Type,
};

mod ext_int;
mod select;

pub use ext_int::ExtInt;
pub use select::Select;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    Arg {
        dest: Id,
        src: Arg,
    },
    WriteStack {
        dest: StackIndex,
        src: Reference,
    },
    ReadStack {
        dest: Id,
        src: StackIndex,
    },
    ReadReg {
        dest: Id,
        base: Source<ty::I32>,
        src: register::RiscV,
    },
    WriteReg {
        dest: register::RiscV,
        base: Source<ty::I32>,
        src: AnySource,
    },
    ReadMem {
        dest: Id,
        src: AnySource,
        base: Source<ty::I32>,
        width: Width,
        sign_extend: bool,
    },
    WriteMem {
        addr: Source<ty::I32>,
        src: AnySource,
        base: Source<ty::I32>,
        width: Width,
    },
    ShiftOp {
        dest: Id,
        src: SourcePair,
        op: ShiftOp,
    },

    /// Subtraction is special, and not in a good way.
    /// Specifically, `x - imm` is the same as `x + (-imm)`.
    Sub {
        dest: Id,
        src1: AnySource,
        src2: Reference,
    },
    Cmp {
        dest: Id,
        src: SourcePair,
        kind: CmpKind,
    },
    CommutativeBinOp {
        dest: Id,
        src1: Reference,
        src2: AnySource,
        op: CommutativeBinOp,
    },
    // todo: box this
    Select(Select),
    ExtInt(ExtInt),
    Fence,
}

impl Instruction {
    pub fn visit_arg_ids<F: FnMut(Id)>(&self, mut visit: F) {
        match self {
            Self::Arg { .. } | Self::ReadStack { .. } | Self::Fence => {}
            Self::ReadReg { base, .. } => {
                if let Source::Ref(it) = base {
                    visit(*it)
                }
            }
            Self::WriteReg { base, src, .. } | Self::ReadMem { base, src, .. } => {
                if let Source::Ref(it) = base {
                    visit(*it)
                }

                if let AnySource::Ref(src) = src {
                    visit(src.id)
                }
            }

            Self::WriteMem { addr, src, base, .. } => {
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

            Self::ShiftOp { src, .. } | Self::Cmp { src, .. } => match src {
                SourcePair::RefRef(l, r) => {
                    visit(l.id);
                    visit(r.id);
                }
                SourcePair::RefConst(it, _) | SourcePair::ConstRef(_, it) => visit(it.id),
            },

            Self::CommutativeBinOp { src1: reference, src2: source, .. }
            | Self::Sub { src1: source, src2: reference, .. } => {
                visit(reference.id);

                if let AnySource::Ref(it) = source {
                    visit(it.id)
                }
            }

            Self::Select(it) => {
                visit(it.cond.id);
                if let AnySource::Ref(it) = it.if_true {
                    visit(it.id)
                }

                if let AnySource::Ref(it) = it.if_false {
                    visit(it.id)
                }
            }

            Self::WriteStack { src, .. } => visit(src.id),
            Self::ExtInt(it) => visit(it.src.id),
        }
    }

    #[must_use]
    pub fn ty(&self) -> Type {
        match self {
            Self::Arg { dest: _, src: _ } => Type::I32,
            Self::WriteStack { dest: _, src: _ } => Type::Unit,
            Self::ReadStack { dest: _, src: _ } => Type::I32,
            Self::ReadReg { dest: _, base: _, src: _ } => Type::I32,
            Self::WriteReg { dest: _, base: _, src: _ } => Type::Unit,
            Self::ReadMem { dest: _, src: _, base: _, width: _, sign_extend: _ } => Type::I32,
            Self::WriteMem { addr: _, src: _, base: _, width: _ } => Type::Unit,
            Self::ShiftOp { dest: _, src, op: _ } => {
                let ty = src.lhs().ty();

                assert_eq!(ty, src.rhs().ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Self::CommutativeBinOp { dest: _, src1: reference, src2: source, op: _ }
            | Self::Sub { dest: _, src1: source, src2: reference } => {
                let ty = reference.ty;

                assert_eq!(ty, source.ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Self::Cmp { dest: _, src, kind: _ } => {
                let ty = src.lhs().ty();

                assert_eq!(ty, src.rhs().ty());

                Type::Boolean
            }

            Self::Select(it) => it.ty(),

            Self::Fence => Type::Unit,
            Self::ExtInt(it) => it.ty(),
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
            | Self::Sub { dest, .. }
            | Self::ShiftOp { dest, .. } => Some(*dest),

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

            Self::ShiftOp { dest, src, op } => write!(
                f,
                "{dest} = {op} {src1}, {src2}",
                dest = dest,
                op = op,
                src1 = src.lhs(),
                src2 = src.rhs()
            ),

            Self::Sub { dest, src1, src2 } => write!(f, "{} = sub {}, {}", dest, src1, src2),

            Self::Cmp { dest, src, kind } => {
                write!(f, "{} = cmp {} {}, {}", dest, kind, src.lhs(), src.rhs())
            }

            Self::Select(it) => it.fmt(f),
            Self::Fence => f.write_str("fence"),
            Self::ExtInt(it) => it.fmt(f),
        }
    }
}
