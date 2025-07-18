// unsafe code is only *denied* instead of forbidden.
// This is due to some of the core types requiring unsafe code.
// When unsafe code is used, it *must* be documented with a `safety`
//   comment explaining how it follows the safety contract
#![deny(unsafe_code)]
#![deny(unsafe_op_in_unsafe_fn)]
#![allow(clippy::match_bool)]
#![warn(clippy::must_use_candidate, clippy::clone_on_copy)]
#![no_std]

use core::fmt;

#[cfg(feature = "disassemble")]
pub mod disassemble;
pub mod instruction;
pub mod opcode;
pub mod register;

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Width {
    Byte,
    Word,
    DWord,
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Width::Byte => "byte",
            Width::Word => "word",
            Width::DWord => "dword",
        })
    }
}

pub struct DisplayDeferSlice<'a, T: fmt::Display>(pub &'a [T]);

impl<T: fmt::Display> fmt::Display for DisplayDeferSlice<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        DisplayDeferIter(self.0).fmt(f)
    }
}

pub struct DisplayDeferIter<I: IntoIterator<Item = T> + Copy, T: fmt::Display>(pub I);

impl<I: IntoIterator<Item = T> + Copy, T: fmt::Display> fmt::Display for DisplayDeferIter<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.0.into_iter();
        if let Some(first) = iter.next() {
            first.fmt(f)?;
            for item in iter {
                write!(f, "\n{item}")?;
            }
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum ReturnCode {
    #[allow(dead_code)]
    Normal = 0,
    EBreak = 1,
    ECall = 2,
}

impl ReturnCode {
    #[must_use]
    pub const fn new(code: u32) -> Option<Self> {
        match code {
            0 => Some(Self::Normal),
            1 => Some(Self::EBreak),
            2 => Some(Self::ECall),
            _ => None,
        }
    }
}
