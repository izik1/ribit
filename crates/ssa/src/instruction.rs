use std::cmp::Ordering;
use std::fmt;

use ribit_core::{Width, register};

use crate::reference::Reference;
use crate::{
    AnySource, Arg, CmpKind, CommutativeBinOp, Constant, Id, ShiftOp, Source, SourcePair,
    StackIndex, Type, eval, ty,
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
        args: CmpArgs,
    },
    CommutativeBinOp {
        dest: Id,
        args: CommutativeBinArgs,
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
                    visit(*it);
                }
            }
            Self::WriteReg { base, src, .. } | Self::ReadMem { base, src, .. } => {
                if let Source::Ref(it) = base {
                    visit(*it);
                }

                if let AnySource::Ref(src) = src {
                    visit(src.id);
                }
            }

            Self::WriteMem { addr, src, base, .. } => {
                if let Source::Ref(it) = addr {
                    visit(*it);
                }

                if let AnySource::Ref(it) = src {
                    visit(it.id);
                }

                if let Source::Ref(it) = base {
                    visit(*it);
                }
            }

            Self::ShiftOp { src, .. } => match src {
                SourcePair::RefRef(l, r) => {
                    visit(l.id);
                    visit(r.id);
                }
                SourcePair::RefConst(it, _) | SourcePair::ConstRef(_, it) => visit(it.id),
            },

            Self::CommutativeBinOp {
                dest: _,
                args: BinaryArgs { src1: reference, src2: source, op: _ },
            }
            | Self::Sub { src1: source, src2: reference, .. }
            | Self::Cmp { dest: _, args: CmpArgs { src1: reference, src2: source, op: _ } } => {
                visit(reference.id);

                if let AnySource::Ref(it) = source {
                    visit(it.id);
                }
            }

            Self::Select(it) => {
                visit(it.cond.id);
                if let AnySource::Ref(it) = it.if_true {
                    visit(it.id);
                }

                if let AnySource::Ref(it) = it.if_false {
                    visit(it.id);
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

                ty::assert_types_eq!(ty, src.rhs().ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Self::CommutativeBinOp {
                dest: _,
                args: BinaryArgs { src1: reference, src2: source, op: _ },
            }
            | Self::Sub { dest: _, src1: source, src2: reference } => {
                let ty = reference.ty;

                ty::assert_types_eq!(ty, source.ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }

            Self::Cmp { dest: _, args: CmpArgs { src1, src2, op: _ } } => {
                ty::assert_types_eq!(src1.ty, src2.ty());

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

            Self::Select(it) => Some(it.dest),
            Self::ExtInt(it) => Some(it.dest),

            Self::WriteStack { .. }
            | Self::WriteReg { .. }
            | Self::WriteMem { .. }
            | Self::Fence => None,
        }
    }

    #[must_use]
    pub fn reference(&self) -> Option<Reference> {
        self.id().map(|id| Reference { ty: self.ty(), id })
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReadReg { dest, base, src } => write!(f, "{dest} = x({base}){}", src.get()),

            Self::ReadMem { dest, src, base, width, sign_extend } => match sign_extend {
                true => write!(f, "{dest} = signed {width} m({base}){src}"),
                false => write!(f, "{dest} = {width} m({base}){src}"),
            },

            Self::ReadStack { dest, src } => write!(f, "{dest} = {src}"),

            Self::WriteReg { base, dest, src } => write!(f, "x({base}){} = {src}", dest.get()),

            Self::WriteMem { base, addr, src, width } => {
                write!(f, "m({base}){addr} = {width} {src}")
            }

            Self::WriteStack { dest, src } => write!(f, "{dest} = {src}"),

            Self::Arg { dest, src } => write!(f, "{dest} = args[{}]", *src as u8),

            Self::CommutativeBinOp { dest, args: BinaryArgs { src1, src2, op } } => {
                write!(f, "{dest} = {op} {src1}, {src2}")
            }

            Self::ShiftOp { dest, src, op } => {
                write!(f, "{dest} = {op} {src1}, {src2}", src1 = src.lhs(), src2 = src.rhs())
            }

            Self::Sub { dest, src1, src2 } => write!(f, "{dest} = sub {src1}, {src2}"),

            Self::Cmp { dest, args: CmpArgs { src1, src2, op } } => {
                write!(f, "{dest} = cmp {op} {src1}, {src2}")
            }

            Self::Select(it) => it.fmt(f),
            Self::Fence => f.write_str("fence"),
            Self::ExtInt(it) => it.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct BinaryArgs<T> {
    pub src1: Reference,
    pub src2: AnySource,
    pub op: T,
}

pub type CommutativeBinArgs = BinaryArgs<CommutativeBinOp>;

impl CommutativeBinArgs {
    pub fn new(op: CommutativeBinOp, src1: AnySource, src2: AnySource) -> Result<Self, AnySource> {
        /// Reassociate by doing... nothing.
        #[inline(always)]
        fn reassociate_id(_: CommutativeBinOp, r: Reference, c: Constant) -> (Reference, Constant) {
            (r, c)
        }

        Self::new_assoc(op, src1, src2, reassociate_id)
    }

    pub fn new_assoc<F>(
        op: CommutativeBinOp,
        src1: AnySource,
        src2: AnySource,
        reassociate: F,
    ) -> Result<Self, AnySource>
    where
        F: FnOnce(CommutativeBinOp, Reference, Constant) -> (Reference, Constant),
    {
        let (src1, src2) = match (src1, src2) {
            (AnySource::Const(src1), AnySource::Const(src2)) => {
                return Err(AnySource::Const(eval::commutative_binop(src1, src2, op)));
            }
            (AnySource::Const(c), AnySource::Ref(r)) | (AnySource::Ref(r), AnySource::Const(c)) => {
                let (r, c) = reassociate(op, r, c);
                (r, AnySource::Const(c))
            }
            (AnySource::Ref(src1), AnySource::Ref(src2)) if src1.id <= src2.id => {
                (src1, AnySource::Ref(src2))
            }
            (src1 @ AnySource::Ref(_), AnySource::Ref(src2)) => (src2, src1),
        };

        let args = BinaryArgs { src1, src2, op };

        let res = eval::commutative_absorb(args)
            .map(AnySource::Const)
            .or_else(|| eval::commutative_identity(args).map(AnySource::Ref));

        if let Some(it) = res {
            return Err(it);
        }

        Ok(Self { src1, src2, op })
    }
}

pub type CmpArgs = BinaryArgs<CmpKind>;

impl CmpArgs {
    pub fn new(src1: AnySource, src2: AnySource, op: CmpKind) -> Result<Self, bool> {
        let (src1, src2, op) = match (src1, src2) {
            (AnySource::Const(src1), AnySource::Const(src2)) => {
                return Err(eval::icmp(src1, src2, op));
            }

            (src1 @ AnySource::Const(_), AnySource::Ref(src2)) => (src2, src1, op.swap()),

            (AnySource::Ref(src1), AnySource::Ref(src2)) => match src1.id.cmp(&src2.id) {
                Ordering::Less => (src1, AnySource::Ref(src2), op),
                Ordering::Equal => return Err(op.include_eq()),
                Ordering::Greater => (src2, AnySource::Ref(src1), op.swap()),
            },

            (AnySource::Ref(src1), src2 @ AnySource::Const(_)) => (src1, src2, op),
        };

        if let AnySource::Const(src2) = src2
            && let Some(res) = eval::icmp_constant(src2, op)
        {
            return Err(res);
        }

        Ok(Self { src1, src2, op })
    }
}
