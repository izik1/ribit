// unsafe code is only *denied* instead of forbidden.
// This is due to some of the core types requiring unsafe code.
// When unsafe code is used, it *must* be documented with a `safety`
//   comment explaining how it follows the safety contract
#![deny(unsafe_code)]

use std::fmt;

pub mod disassemble;
pub mod instruction;
pub mod opcode;
pub mod register;

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Width {
    Byte,
    Word,
    DWord,
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte => f.write_str("byte"),
            Self::Word => f.write_str("word"),
            Self::DWord => f.write_str("dword"),
        }
    }
}

pub struct DisplayDeferSlice<'a, T: std::fmt::Display>(pub &'a [T]);

impl<'a, T: std::fmt::Display> std::fmt::Display for DisplayDeferSlice<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            write!(f, "{}", first)?;
            for item in rest {
                write!(f, "\n{}", item)?;
            }
        }

        Ok(())
    }
}

#[repr(u32)]
pub enum ReturnCode {
    #[allow(dead_code)]
    Normal = 0,
    EBreak = 1,
    ECall = 2,
}

impl ReturnCode {
    pub fn new(code: u32) -> Option<Self> {
        match code {
            0 => Some(Self::Normal),
            1 => Some(Self::EBreak),
            2 => Some(Self::ECall),
            _ => None,
        }
    }
}
