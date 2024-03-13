#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]
#![deny(unsafe_op_in_unsafe_fn)]

#[macro_use]
extern crate static_assertions;

mod rt;
mod sbi;

use std::collections::HashMap;

use rasen::params::Register;
use ribit_ssa as ssa;
pub use rt::{Runtime, Target};
use ssa::Constant;

pub type InterpreterRuntime = rt::Runtime<rt::Interpreter>;

#[cfg(target_arch = "x86_64")]
pub type DefaultRuntime = AMD64Runtime;

#[cfg(not(target_arch = "x86_64"))]
pub type DefaultRuntime = InterpreterRuntime;

/// Runtime for X86-64 hosts.
#[cfg(target_arch = "x86_64")]
pub type AMD64Runtime = Runtime<rt::x86_64::rt::X86_64>;

pub const XLEN: usize = 32;

pub const MEMORY_SIZE: u32 = 1024 * 1024 * 16;

// ensure that memory size is a power of two.
const_assert_eq!(MEMORY_SIZE.count_ones(), 1);

pub enum SourcePair {
    RegReg(Register, Register),
    RegVal(Register, u32),
    ValReg(u32, Register),
}

impl SourcePair {
    fn from_ssa(pair: ssa::SourcePair, map: &HashMap<ssa::Id, Register>) -> Option<Self> {
        match pair {
            ssa::SourcePair::RefRef(lhs, rhs) => {
                Some(Self::RegReg(*map.get(&lhs.id)?, *map.get(&rhs.id)?))
            }
            ssa::SourcePair::RefConst(lhs, rhs) => {
                Some(Self::RegVal(*map.get(&lhs.id)?, get_val(rhs)))
            }
            ssa::SourcePair::ConstRef(lhs, rhs) => {
                Some(Self::ValReg(get_val(lhs), *map.get(&rhs.id)?))
            }
        }
    }
}

fn get_val(konst: Constant) -> u32 {
    match konst {
        Constant::Int(i) => {
            assert_eq!(i.bits(), 32);
            i.unsigned()
        }
        // fixme: should we panic here?
        Constant::Bool(i) => i as u32,
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Source {
    Val(u32),
    Register(Register),
}

impl Source {
    #[must_use]
    pub fn from_typed_source(
        src: ssa::Source<ssa::ty::I32>,
        map: &HashMap<ssa::Id, Register>,
    ) -> Option<Self> {
        match src {
            ssa::Source::Const(c) => Some(Self::Val(c)),
            ssa::Source::Ref(id) => map.get(&id).copied().map(Self::Register),
        }
    }

    #[must_use]
    pub fn from_ssa_src(src: ssa::AnySource, map: &HashMap<ssa::Id, Register>) -> Option<Self> {
        match src {
            ssa::AnySource::Const(konst) => Some(Source::Val(get_val(konst))),
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
                write!(f, "{idx} => [")?;

                let reg_count = regs.len();
                for (offset, reg) in regs.iter().enumerate() {
                    FmtRegister(*reg).fmt(f)?;
                    if offset < reg_count - 1 {
                        f.write_str(", ")?;
                    } else {
                        f.write_str("]\n")?;
                    }
                }
            }

            Ok(())
        }
    }

    pub struct FmtRegister(pub Register);

    impl fmt::Display for FmtRegister {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let s = match self.0 {
                Register::Zax => "zax",
                Register::Zcx => "zcx",
                Register::Zdx => "zdx",
                Register::Zbx => "zbx",
                Register::Zsp => "zsp",
                Register::Zbp => "zbp",
                Register::Zsi => "zsi",
                Register::Zdi => "zdi",
                Register::R8 => "r8",
                Register::R9 => "r9",
                Register::R10 => "r10",
                Register::R11 => "r11",
                Register::R12 => "r12",
                Register::R13 => "r13",
                Register::R14 => "r14",
                Register::R15 => "r15",
            };

            f.write_str(s)
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
