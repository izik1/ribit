use std::fmt;

use crate::{opcode, register, Width};

pub mod analysis;
pub mod eval;
pub mod lower;
pub mod opt;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Id(u16);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
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
    Id(Id),
}

impl Source {
    #[must_use]
    pub fn val(self) -> Option<u32> {
        match self {
            Self::Val(v) => Some(v),
            Self::Id(_) => None,
        }
    }

    #[must_use]
    pub fn id(self) -> Option<Id> {
        match self {
            Self::Id(id) => Some(id),
            Self::Val(_) => None,
        }
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Val(v) => v.fmt(f),
            Self::Id(id) => id.fmt(f),
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
pub enum Instruction {
    Arg {
        dest: Id,
        src: Arg,
    },
    ReadReg {
        dest: Id,
        base: Source,
        src: register::RiscV,
    },
    WriteReg {
        dest: register::RiscV,
        base: Source,
        src: Source,
    },
    ReadMem {
        dest: Id,
        src: Source,
        base: Source,
        width: Width,
        sign_extend: bool,
    },
    WriteMem {
        addr: Source,
        src: Source,
        base: Source,
        width: Width,
    },
    BinOp {
        dest: Id,
        src1: Source,
        src2: Source,
        op: BinOp,
    },
    LoadConst {
        dest: Id,
        src: u32,
    },
    Cmp {
        dest: Id,
        src1: Source,
        src2: Source,
        kind: CmpKind,
    },
    // todo: box this
    Select {
        dest: Id,
        cond: Source,
        if_true: Source,
        if_false: Source,
    },
    Ret {
        addr: Source,
        code: Source,
    },
    Fence,
}

impl Instruction {
    #[must_use]
    pub fn id(&self) -> Option<Id> {
        match self {
            Self::Select { dest, .. }
            | Self::ReadReg { dest, .. }
            | Self::ReadMem { dest, .. }
            | Self::LoadConst { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::Arg { dest, .. }
            | Self::BinOp { dest, .. } => Some(*dest),
            Self::WriteReg { .. } | Self::WriteMem { .. } | Self::Ret { .. } | Self::Fence => None,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReadReg { dest, base, src } => write!(f, "{} = x({}){}", dest, base, src.get()),
            Self::ReadMem {
                dest,
                src,
                base,
                width,
                sign_extend,
            } => match sign_extend {
                true => write!(f, "{} = signed {} m({}){}", dest, width, base, src),
                false => write!(f, "{} = {} m({}){}", dest, width, base, src),
            },

            Self::WriteReg { base, dest, src } => write!(f, "x({}){} = {}", base, dest.get(), src),
            Self::WriteMem { base, addr, src, width } => write!(f, "m({}){} = {} {}", base, addr, width, src),
            Self::LoadConst { dest, src } => write!(f, "{} = {}", dest, src),
            Self::Arg { dest, src } => write!(f, "{} = args[{}]", dest, *src as u8),
            Self::BinOp {
                dest,
                src1,
                src2,
                op,
            } => write!(
                f,
                "{dest} = {op} {src1}, {src2}",
                dest = dest,
                op = op,
                src1 = src1,
                src2 = src2
            ),
            Self::Cmp {
                dest,
                src1,
                src2,
                kind,
            } => write!(f, "{} = cmp {} {}, {}", dest, kind, src1, src2),

            Self::Select {
                dest,
                cond,
                if_true,
                if_false,
            } => write!(f, "{} = select {}, {}, {}", dest, cond, if_true, if_false),
            Self::Fence => write!(f, "fence"),
            Self::Ret { addr, code } => write!(f, "ret {}, {}", code, addr),
        }
    }
}

#[cfg(test)]
fn max_fn() -> Vec<Instruction> {
    use crate::instruction;

    let mut ctx = lower::Context::new(1024);

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

#[cfg(test)]
mod test {
    use super::{lower, Id, Instruction, Source};
    use crate::register;
    use crate::ssa::Arg;

    #[test]
    fn empty() {
        const START_PC: u32 = 0xfefe_fefe;

        let ctx = lower::Context::new(START_PC);

        let instrs = ctx.ret();

        assert_eq!(instrs.len(), 1);

        assert_eq!(
            instrs[0],
            Instruction::Ret {
                addr: Source::Val(START_PC),
                code: Source::Val(0),
            }
        );
    }

    #[test]
    fn reads_register() {
        let mut ctx = lower::Context::new(0);

        ctx.read_register(register::RiscV::X1);
        ctx.read_register(register::RiscV::X2);
        ctx.read_register(register::RiscV::X1);
        let instrs = ctx.ret();

        assert_eq!(instrs.len(), 5);

        assert_eq!(
            instrs[0],
            Instruction::Arg {
                dest: Id(0),
                src: Arg::Register,
            }
        );

        assert_eq!(
            instrs[1],
            Instruction::Arg {
                dest: Id(1),
                src: Arg::Memory,
            }
        );

        assert_eq!(
            instrs[2],
            Instruction::ReadReg {
                dest: Id(2),
                src: register::RiscV::X1,
                base: Source::Id(Id(0)),
            }
        );

        assert_eq!(
            instrs[3],
            Instruction::ReadReg {
                dest: Id(3),
                src: register::RiscV::X2,
                base: Source::Id(Id(0)),
            }
        );

        assert_eq!(
            instrs[4],
            Instruction::Ret {
                addr: Source::Val(0),
                code: Source::Val(0),
            }
        );
    }

    #[test]
    fn writes_register() {
        let mut ctx = lower::Context::new(0);

        ctx.write_register(register::RiscV::X2, Source::Val(0));
        let instrs = ctx.ret();

        assert_eq!(instrs.len(), 2);
        assert_eq!(
            instrs[0],
            Instruction::WriteReg {
                dest: register::RiscV::X2,
                src: Source::Val(0),
                base: Source::Id(Id(0)),
            }
        );
    }
}
