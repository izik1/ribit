use std::fmt;

use crate::{opcode, register};

pub mod lower;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct InstructionId(usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    And {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    LoadConst {
        dest: Id,
        src: u32,
    },
    Add {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Or {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Sll {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Srl {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Sra {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Sub {
        dest: Id,
        src1: Source,
        src2: Source,
    },
    Xor {
        dest: Id,
        src1: Source,
        src2: Source,
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
            | Self::LoadConst { dest, .. }
            | Self::Cmp { dest, .. }
            | Self::Sub { dest, .. }
            | Self::Or { dest, .. }
            | Self::Sll { dest, .. }
            | Self::Srl { dest, .. }
            | Self::Sra { dest, .. }
            | Self::Xor { dest, .. }
            | Self::And { dest, .. }
            | Self::Add { dest, .. } => Some(*dest),
            Self::WriteReg { .. } | Self::Ret { .. } | Self::Fence => None,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ReadReg { dest, src } => write!(f, "{} = x{}", dest, src.get()),
            Self::WriteReg { dest, src } => write!(f, "x{} = {}", dest.get(), src),
            Self::LoadConst { dest, src } => write!(f, "{} = {}", dest, src),
            Self::Or { dest, src1, src2 } => write!(f, "{} = or {}, {}", dest, src1, src2),
            Self::Sll { dest, src1, src2 } => write!(f, "{} = sll {}, {}", dest, src1, src2),
            Self::Srl { dest, src1, src2 } => write!(f, "{} = srl {}, {}", dest, src1, src2),
            Self::Sra { dest, src1, src2 } => write!(f, "{} = sra {}, {}", dest, src1, src2),
            Self::Xor { dest, src1, src2 } => write!(f, "{} = xor {}, {}", dest, src1, src2),
            Self::And { dest, src1, src2 } => write!(f, "{} = and {}, {}", dest, src1, src2),
            Self::Add { dest, src1, src2 } => write!(f, "{} = add {}, {}", dest, src1, src2),
            Self::Sub { dest, src1, src2 } => write!(f, "{} = sub {}, {}", dest, src1, src2),
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
    use std::mem;

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
