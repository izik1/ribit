use std::fmt;

use crate::{opcode, register, Width};

pub mod lower;
pub mod opt;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct InstructionId(usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct Id(u16);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
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

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Source {
    Val(u32),
    Id(Id),
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

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    ReadReg {
        dest: Id,
        src: register::RiscV,
    },
    WriteReg {
        dest: register::RiscV,
        src: Source,
    },
    ReadMem {
        dest: Id,
        src: Source,
        width: Width,
        sign_extend: bool,
    },
    WriteMem {
        addr: Source,
        src: Source,
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
    pub fn id(&self) -> Option<Id> {
        match self {
            Self::Select { dest, .. }
            | Self::ReadReg { dest, .. }
            | Self::ReadMem { dest, .. }
            | Self::LoadConst { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::BinOp { dest, .. } => Some(*dest),
            Self::WriteReg { .. } | Self::WriteMem { .. } | Self::Ret { .. } | Self::Fence => None,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReadReg { dest, src } => write!(f, "{} = x{}", dest, src.get()),
            Self::ReadMem {
                dest,
                src,
                width,
                sign_extend,
            } => match sign_extend {
                true => write!(f, "{} = signed {} mem[{}]", dest, width, src),
                false => write!(f, "{} = {} mem[{}]", dest, width, src),
            },

            Self::WriteReg { dest, src } => write!(f, "x{} = {}", dest.get(), src),
            Self::WriteMem { addr, src, width } => write!(f, "mem[{}] = {} {}", addr, width, src),
            Self::LoadConst { dest, src } => write!(f, "{} = {}", dest, src),
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
mod test {
    use super::{lower, Id, Instruction, Source};
    use crate::register;

    #[test]
    fn empty() {
        const START_PC: u32 = 0xfefe_fefe;

        let ctx = lower::Context::new(START_PC);

        let instrs = ctx.ret();

        assert_eq!(instrs.len(), 2);

        assert_eq!(
            instrs[0],
            Instruction::LoadConst {
                dest: Id(0),
                src: START_PC,
            }
        );

        assert_eq!(
            instrs[1],
            Instruction::Ret {
                addr: Source::Id(Id(0)),
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
            instrs[1],
            Instruction::ReadReg {
                dest: Id(1),
                src: register::RiscV::X1
            }
        );
        assert_eq!(
            instrs[2],
            Instruction::ReadReg {
                dest: Id(2),
                src: register::RiscV::X2
            }
        );
        assert_eq!(
            instrs[3],
            Instruction::ReadReg {
                dest: Id(3),
                src: register::RiscV::X1
            }
        );
    }

    #[test]
    fn writes_register() {
        let mut ctx = lower::Context::new(0);

        ctx.write_register(register::RiscV::X2, Source::Val(0));
        let instrs = ctx.ret();

        assert_eq!(instrs.len(), 3);
        assert_eq!(
            instrs[1],
            Instruction::WriteReg {
                dest: register::RiscV::X2,
                src: Source::Val(0)
            }
        );
    }
}

#[cfg(test)]
fn cmp_instrs(expected: &[&str], actual: &[Instruction]) {
    for (idx, instr) in actual.iter().enumerate() {
        assert_eq!(expected[idx], format!("{}", instr));
        println!("{}", instr);
    }
}
