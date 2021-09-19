#![forbid(unsafe_code)]

use std::fmt;

use reference::Reference;
use ribit_core::{opcode, register, Width};

pub mod analysis;
pub mod eval;
mod id;
pub mod lower;
pub mod opt;
pub mod reference;
mod ty;

pub use id::{Id, IdAllocator};
pub use ty::Type;

pub struct BlockDisplay<'a>(&'a [Instruction], &'a Terminator);

impl<'a> std::fmt::Display for BlockDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            write!(f, "{}", first)?;
            for item in rest {
                write!(f, "\n{}", item)?;
            }

            write!(f, "\n{}", self.1)?;
        } else {
            write!(f, "{}", self.1)?;
        }

        Ok(())
    }
}

pub struct Block {
    pub allocator: IdAllocator,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl Block {
    pub fn arg_ref(&self, arg: Arg) -> Option<Reference> {
        self.instructions.iter().find_map(|it| match it {
            Instruction::Arg { dest, src } if *src == arg => {
                Some(Reference { ty: Type::Int32, id: *dest })
            }
            _ => None,
        })
    }

    pub fn reference(&self, id: Id) -> Option<Reference> {
        self.instructions
            .iter()
            .find_map(|it| it.id().filter(|it| *it == id).map(|id| Reference { ty: it.ty(), id }))
    }

    pub fn display_instructions(&self) -> BlockDisplay<'_> {
        BlockDisplay(&self.instructions, &self.terminator)
    }
}

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
            (self.0 as i32 + 1) * -4
        } else {
            self.0 as i32 * 4
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
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

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Source {
    Val(u32),
    Ref(Reference),
}

impl Source {
    #[must_use]
    pub fn val(self) -> Option<u32> {
        match self {
            Self::Val(v) => Some(v),
            Self::Ref(_) => None,
        }
    }

    #[must_use]
    pub fn reference(self) -> Option<Reference> {
        match self {
            Self::Ref(r) => Some(r),
            Self::Val(_) => None,
        }
    }

    #[must_use]
    pub fn ty(self) -> Type {
        match self {
            Source::Val(_) => Type::Int32,
            Source::Ref(r) => r.ty,
        }
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Val(v) => write!(f, "{:08x}", v),
            Self::Ref(r) => r.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum BinOp {
    And,
    Add,
    Or,
    Sll,
    Srl,
    Sra,
    Sub,
    Xor,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::And => f.write_str("and"),
            Self::Add => f.write_str("add"),
            Self::Or => f.write_str("or"),
            Self::Sll => f.write_str("sll"),
            Self::Srl => f.write_str("srl"),
            Self::Sra => f.write_str("sra"),
            Self::Sub => f.write_str("sub"),
            Self::Xor => f.write_str("xor"),
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
    Ret { addr: Source, code: Source },
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ret { addr, code } => write!(f, "ret {}, {}", code, addr),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    Arg { dest: Id, src: Arg },
    // A const should never be put on the stack.
    WriteStack { dest: StackIndex, src: Reference },
    ReadStack { dest: Id, src: StackIndex },
    ReadReg { dest: Id, base: Source, src: register::RiscV },
    WriteReg { dest: register::RiscV, base: Source, src: Source },
    ReadMem { dest: Id, src: Source, base: Source, width: Width, sign_extend: bool },
    WriteMem { addr: Source, src: Source, base: Source, width: Width },
    BinOp { dest: Id, src1: Source, src2: Source, op: BinOp },
    LoadConst { dest: Id, src: u32 },
    Cmp { dest: Id, src1: Source, src2: Source, kind: CmpKind },
    // todo: box this
    Select { dest: Id, cond: Source, if_true: Source, if_false: Source },
    Fence,
}

impl Instruction {
    pub fn ty(&self) -> Type {
        match self {
            Instruction::Arg { dest: _, src: _ } => Type::Int32,
            Instruction::WriteStack { dest: _, src: _ } => Type::Unit,
            Instruction::ReadStack { dest: _, src: _ } => Type::Int32,
            Instruction::ReadReg { dest: _, base: _, src: _ } => Type::Int32,
            Instruction::WriteReg { dest: _, base: _, src: _ } => Type::Unit,
            Instruction::ReadMem { dest: _, src: _, base: _, width: _, sign_extend: _ } => {
                Type::Int32
            }
            Instruction::WriteMem { addr: _, src: _, base: _, width: _ } => Type::Unit,
            Instruction::BinOp { dest: _, src1, src2, op: _ } => {
                let ty = src1.ty();

                assert_eq!(ty, src2.ty());

                // type technically depends on op, but... for now:
                assert_eq!(ty, Type::Int32);

                ty
            }
            Instruction::LoadConst { dest: _, src: _ } => Type::Int32,
            Instruction::Cmp { dest: _, src1, src2, kind: _ } => {
                let ty = src1.ty();

                assert_eq!(ty, src2.ty());

                ty
            }
            Instruction::Select { dest: _, cond: _, if_true, if_false } => {
                let ty = if_true.ty();

                assert_eq!(ty, if_false.ty());

                ty
            }

            Instruction::Fence => Type::Unit,
        }
    }

    #[must_use]
    pub fn id(&self) -> Option<Id> {
        match self {
            Self::Select { dest, .. }
            | Self::ReadReg { dest, .. }
            | Self::ReadMem { dest, .. }
            | Self::ReadStack { dest, .. }
            | Self::LoadConst { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::Arg { dest, .. }
            | Self::BinOp { dest, .. } => Some(*dest),

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

            Self::LoadConst { dest, src } => write!(f, "{} = {:08x}", dest, src),
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
        }
    }
}

pub fn update_reference(src: &mut Source, old: Id, new: Id) {
    match src {
        Source::Ref(r) if r.id == old => r.id = new,
        _ => {}
    }
}

// hack: do something better than `start_from`
pub fn update_references(graph: &mut Block, start_from: usize, old: Id, new: Id) {
    for instr in &mut graph.instructions[start_from..] {
        match instr {
            Instruction::Fence
            | Instruction::Arg { .. }
            | Instruction::LoadConst { .. }
            | Instruction::ReadStack { .. } => {}

            Instruction::BinOp { dest: _, src1, src2, .. }
            | Instruction::Cmp { dest: _, src1, src2, .. } => {
                update_reference(src1, old, new);
                update_reference(src2, old, new);
            }

            Instruction::ReadReg { dest: _, base, .. } => {
                update_reference(base, old, new);
            }

            Instruction::WriteStack { dest: _, src } => {
                if src.id == old {
                    src.id = new
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
                update_reference(cond, old, new);
                update_reference(if_true, old, new);
                update_reference(if_false, old, new);
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
    use ribit_core::{opcode, register};

    use crate::{lower, Arg, Block, Id, Instruction, Source, Terminator};

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

    #[test]
    fn empty() {
        const START_PC: u32 = 0xfefe_fefe;

        let ctx = lower::Context::new(START_PC, MEM_SIZE);

        let block = ctx.ret();

        assert_eq!(block.instructions.len(), 2);

        assert_eq!(block.instructions[0], Instruction::Arg { src: Arg::Register, dest: Id(0) });

        assert_eq!(block.instructions[1], Instruction::Arg { src: Arg::Memory, dest: Id(1) });

        assert_eq!(
            block.terminator,
            Terminator::Ret { addr: Source::Val(START_PC), code: Source::Val(0) }
        );
    }

    #[test]
    fn reads_register() {
        let mut ctx = lower::Context::new(0, MEM_SIZE);

        ctx.read_register(register::RiscV::X1);
        ctx.read_register(register::RiscV::X2);
        ctx.read_register(register::RiscV::X1);
        let block = ctx.ret();

        assert_eq!(block.instructions.len(), 4);

        assert_eq!(block.instructions[0], Instruction::Arg { dest: Id(0), src: Arg::Register });

        assert_eq!(block.instructions[1], Instruction::Arg { dest: Id(1), src: Arg::Memory });

        let reg_arg = block.arg_ref(Arg::Register).unwrap();

        assert_eq!(
            block.instructions[2],
            Instruction::ReadReg {
                dest: Id(2),
                src: register::RiscV::X1,
                base: Source::Ref(reg_arg)
            }
        );

        assert_eq!(
            block.instructions[3],
            Instruction::ReadReg {
                dest: Id(3),
                src: register::RiscV::X2,
                base: Source::Ref(reg_arg)
            }
        );

        assert_eq!(
            block.terminator,
            Terminator::Ret { addr: Source::Val(0), code: Source::Val(0) }
        );
    }

    #[test]
    fn writes_register() {
        let mut ctx = lower::Context::new(0, MEM_SIZE);

        ctx.write_register(register::RiscV::X2, Source::Val(0));
        let block = ctx.ret();

        assert_eq!(block.instructions.len(), 3);

        assert_eq!(block.instructions[0], Instruction::Arg { src: Arg::Register, dest: Id(0) });

        assert_eq!(block.instructions[1], Instruction::Arg { src: Arg::Memory, dest: Id(1) });

        let reg_arg = block.arg_ref(Arg::Register).unwrap();

        assert_eq!(
            block.instructions[2],
            Instruction::WriteReg {
                dest: register::RiscV::X2,
                src: Source::Val(0),
                base: Source::Ref(reg_arg),
            }
        );
    }
}
