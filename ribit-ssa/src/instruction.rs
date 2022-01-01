use std::fmt;

use ribit_core::{register, Width};

use crate::reference::Reference;
use crate::ty::{Bitness, BoolTy};
use crate::{
    AnySource, Arg, BinOp, CmpKind, CommutativeBinOp, Id, SourcePair, StackIndex, Type, TypedRef,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Select {
    pub dest: Id,
    pub cond: TypedRef<BoolTy>,
    pub if_true: AnySource,
    pub if_false: AnySource,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = select {}, {}, {}", self.dest, self.cond, self.if_true, self.if_false)
    }
}

impl Select {
    pub fn ty(&self) -> Type {
        let ty = self.if_true.ty();

        assert_eq!(ty, self.if_false.ty());

        ty
    }

    pub fn id(&self) -> Id {
        self.dest
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtInt {
    pub dest: Id,
    pub width: Width,
    pub src: Reference,
    pub signed: bool,
}

impl ExtInt {
    pub fn ty(&self) -> Type {
        assert!(matches!(self.src.ty, Type::Int(_) | Type::Boolean));
        Type::Int(Bitness::from(self.width))
    }

    pub fn id(&self) -> Id {
        self.dest
    }
}

impl fmt::Display for ExtInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instr = match self.signed {
            true => "sext",
            false => "zext",
        };
        write!(f, "{} = {} {} {}", self.dest, instr, self.width, self.src)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    Arg { dest: Id, src: Arg },
    WriteStack { dest: StackIndex, src: Reference },
    ReadStack { dest: Id, src: StackIndex },
    ReadReg { dest: Id, base: AnySource, src: register::RiscV },
    WriteReg { dest: register::RiscV, base: AnySource, src: AnySource },
    ReadMem { dest: Id, src: AnySource, base: AnySource, width: Width, sign_extend: bool },
    WriteMem { addr: AnySource, src: AnySource, base: AnySource, width: Width },
    BinOp { dest: Id, src: SourcePair, op: BinOp },
    Cmp { dest: Id, src: SourcePair, kind: CmpKind },
    CommutativeBinOp { dest: Id, src1: Reference, src2: AnySource, op: CommutativeBinOp },
    // todo: box this
    Select(Select),
    ExtInt(ExtInt),
    Fence,
}

impl Instruction {
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
