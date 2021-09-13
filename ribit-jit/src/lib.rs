#[macro_use]
extern crate static_assertions;

mod rt;
mod sbi;
pub mod x86_64;

pub use rt::{Block, Runtime, Target};

use std::collections::HashMap;

use rasen::params::Register;
use ribit_ssa as ssa;

/// Runtime for X86-64 hosts.
pub type AMD64Runtime = rt::Runtime<x86_64::rt::X86_64>;

pub const XLEN: usize = 32;

pub const MEMORY_SIZE: u32 = 1024 * 1024 * 16;

// ensure that memory size is a power of two.
const_assert_eq!(MEMORY_SIZE.count_ones(), 1);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Source {
    Val(u32),
    Register(Register),
}

impl Source {
    #[must_use]
    pub fn from_ssa_src(src: ssa::Source, map: &HashMap<ssa::Id, Register>) -> Option<Self> {
        match src {
            ssa::Source::Val(v) => Some(Self::Val(v)),
            ssa::Source::Id(id) => map.get(&id).copied().map(Self::Register),
        }
    }

    #[must_use]
    pub fn val(self) -> Option<u32> {
        match self {
            Self::Val(v) => Some(v),
            _ => None,
        }
    }

    #[must_use]
    pub fn reg(self) -> Option<Register> {
        match self {
            Self::Register(reg) => Some(reg),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn is_reg(self) -> bool {
        self.reg().is_some()
    }

    #[inline]
    #[must_use]
    pub fn is_val(self) -> bool {
        self.val().is_some()
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::fmt;

    use rasen::params::Register;
    use ribit_core::{instruction, opcode, register};

    pub struct ShowAllocs<'a> {
        pub allocs: &'a HashMap<ribit_ssa::Id, Register>,
        pub clobbers: &'a HashMap<usize, Vec<Register>>,
    }

    impl fmt::Display for ShowAllocs<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut allocs: Vec<_> = self.allocs.iter().collect();
            allocs.sort_by_key(|(id, _)| *id);

            let mut clobbers: Vec<_> = self.clobbers.iter().collect();
            clobbers.sort_by_key(|(idx, _)| *idx);

            let needs_seperator = !allocs.is_empty() && !clobbers.is_empty();

            for (id, reg) in allocs {
                id.fmt(f)?;
                f.write_str(" => ")?;
                writeln!(f, "{}", FmtRegister(*reg))?;
            }

            if needs_seperator {
                writeln!(f, "---------")?;
            }

            for (idx, regs) in clobbers {
                write!(f, "{} => [", idx)?;

                let reg_count = regs.len();
                for (offset, reg) in regs.iter().enumerate() {
                    FmtRegister(*reg).fmt(f)?;
                    if offset < reg_count - 1 {
                        f.write_str(", ")?;
                    } else {
                        writeln!(f, "]")?;
                    }
                }
            }

            Ok(())
        }
    }

    pub struct FmtRegister(pub Register);

    impl fmt::Display for FmtRegister {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.0 {
                Register::Zax => write!(f, "zax"),
                Register::Zcx => write!(f, "zcx"),
                Register::Zdx => write!(f, "zdx"),
                Register::Zbx => write!(f, "zbx"),
                Register::Zsp => write!(f, "zsp"),
                Register::Zbp => write!(f, "zbp"),
                Register::Zsi => write!(f, "zsi"),
                Register::Zdi => write!(f, "zdi"),
                Register::R8 => write!(f, "r8"),
                Register::R9 => write!(f, "r9"),
                Register::R10 => write!(f, "r10"),
                Register::R11 => write!(f, "r11"),
                Register::R12 => write!(f, "r12"),
                Register::R13 => write!(f, "r13"),
                Register::R14 => write!(f, "r14"),
                Register::R15 => write!(f, "r15"),
            }
        }
    }

    // fixme: dedup with `ribit_ssa`'s version
    pub fn max_fn() -> (Vec<ribit_ssa::Instruction>, ribit_ssa::IdAllocator) {
        let mut ctx = ribit_ssa::lower::Context::new(1024, crate::MEMORY_SIZE);

        ribit_ssa::lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X11),
                opcode::R::ADD,
            )),
            4,
        );

        ribit_ssa::lower::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                31,
                Some(register::RiscV::X11),
                Some(register::RiscV::X12),
                opcode::I::SRLI,
            )),
            4,
        );

        ribit_ssa::lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X11),
                Some(register::RiscV::X12),
                Some(register::RiscV::X11),
                opcode::R::AND,
            )),
            4,
        );

        ribit_ssa::lower::non_terminal(
            &mut ctx,
            instruction::Instruction::R(instruction::R::new(
                Some(register::RiscV::X10),
                Some(register::RiscV::X11),
                Some(register::RiscV::X10),
                opcode::R::ADD,
            )),
            4,
        );

        ribit_ssa::lower::terminal(
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
