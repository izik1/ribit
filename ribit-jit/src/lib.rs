#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]

#[macro_use]
extern crate static_assertions;

mod rt;
mod sbi;
pub mod x86_64;

use std::collections::HashMap;

use rasen::params::Register;
use ribit_ssa as ssa;
pub use rt::{Block, Runtime, Target};
use ssa::Constant;

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
    pub fn from_ssa_src(src: ssa::AnySource, map: &HashMap<ssa::Id, Register>) -> Option<Self> {
        match src {
            ssa::AnySource::Const(Constant::Int(i)) => {
                assert_eq!(i.bits(), 32);
                Some(Source::Val(i.unsigned()))
            }
            ssa::AnySource::Const(Constant::Bool(i)) => {
                // fixme: should we panic here?
                Some(Source::Val(i as u32))
            }
            ssa::AnySource::Ref(r) => map.get(&r.id).copied().map(Self::Register),
        }
    }

    #[inline]
    #[must_use]
    pub(crate) fn is_reg(self) -> bool {
        matches!(self, Self::Register(_))
    }

    #[inline]
    #[must_use]
    pub(crate) fn is_val(self) -> bool {
        matches!(self, Self::Val(_))
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::fmt;

    use rasen::params::Register;
    use ribit_ssa::Block;

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

    pub(crate) fn assemble_block(block: &str) -> Block {
        let output = ribit_asm::tokenize(block, true);
        for error in &output.errors {
            eprintln!("error: {}", error);
        }

        if !output.errors.is_empty() {
            panic!("failing due to previous error(s)");
        }

        let mut instructions = output.instructions;

        let mut ctx = ribit_ssa::lower::Context::new(1024, crate::MEMORY_SIZE);

        let last = instructions.remove(instructions.len() - 1);

        for instruction in instructions {
            ribit_ssa::lower::non_terminal(&mut ctx, instruction.instruction, instruction.len);
        }

        ribit_ssa::lower::terminal(ctx, last.instruction, last.len)
    }

    pub fn max_fn() -> Block {
        // todo: psudeos: `ret`
        assemble_block(
            r#"
                ADD x11, x10, x11
                SRLI x12, x11, 31
                AND x11, x11, x12
                ADD x10, x10, x11
                JALR x0, 0(x1)
            "#,
        )
    }
}
