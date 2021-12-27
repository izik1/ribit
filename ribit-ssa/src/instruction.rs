use std::fmt;

use ribit_core::{register, Width};

use crate::reference::Reference;
use crate::ty::{Bitness, BoolTy};
use crate::{AnySource, Arg, BinOp, CmpKind, Id, StackIndex, Type, TypedSource};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    Arg { dest: Id, src: Arg },
    // A const should never be put on the stack, as you can just reload it.
    WriteStack { dest: StackIndex, src: Reference },
    ReadStack { dest: Id, src: StackIndex },
    ReadReg { dest: Id, base: AnySource, src: register::RiscV },
    WriteReg { dest: register::RiscV, base: AnySource, src: AnySource },
    ReadMem { dest: Id, src: AnySource, base: AnySource, width: Width, sign_extend: bool },
    WriteMem { addr: AnySource, src: AnySource, base: AnySource, width: Width },
    BinOp { dest: Id, src1: AnySource, src2: AnySource, op: BinOp },
    Cmp { dest: Id, src1: AnySource, src2: AnySource, kind: CmpKind },
    // todo: box this
    Select { dest: Id, cond: TypedSource<BoolTy>, if_true: AnySource, if_false: AnySource },
    ExtInt { dest: Id, width: Width, src: Reference, signed: bool },
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
            Instruction::BinOp { dest: _, src1, src2, op: _ } => {
                let ty = src1.ty();

                assert_eq!(ty, src2.ty());

                // type technically depends on op, but... for now:
                assert!(matches!(ty, Type::Int(_)));

                ty
            }
            Instruction::Cmp { dest: _, src1, src2, kind: _ } => {
                let ty = src1.ty();

                assert_eq!(ty, src2.ty());

                Type::Boolean
            }
            Instruction::Select { dest: _, cond: _, if_true, if_false } => {
                let ty = if_true.ty();

                assert_eq!(ty, if_false.ty());

                ty
            }

            Instruction::Fence => Type::Unit,
            Instruction::ExtInt { dest: _, width, src: source, signed: _ } => {
                assert!(matches!(source.ty, Type::Int(_) | Type::Boolean));
                Type::Int(Bitness::from(*width))
            }
        }
    }

    #[must_use]
    pub fn id(&self) -> Option<Id> {
        match self {
            Self::Select { dest, .. }
            | Self::ReadReg { dest, .. }
            | Self::ReadMem { dest, .. }
            | Self::ReadStack { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::Arg { dest, .. }
            | Self::BinOp { dest, .. }
            | Self::ExtInt { dest, .. } => Some(*dest),

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
            Self::BinOp { dest, src1, src2, op } => write!(
                f,
                "{dest} = {op} {src1}, {src2}",
                dest = dest,
                op = op,
                src1 = src1,
                src2 = src2
            ),
            Self::Cmp { dest, src1, src2, kind } => {
                write!(f, "{} = cmp {} {}, {}", dest, kind, src1, src2)
            }

            Self::Select { dest, cond, if_true, if_false } => {
                write!(f, "{} = select {}, {}, {}", dest, cond, if_true, if_false)
            }
            Self::Fence => write!(f, "fence"),
            Self::ExtInt { dest, width, src: source, signed } => {
                let instr = match *signed {
                    true => "sext",
                    false => "zext",
                };
                write!(f, "{} = {} {} {}", dest, instr, width, source)
            }
        }
    }
}
