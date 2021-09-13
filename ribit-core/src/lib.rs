use std::fmt;

pub mod decode;
pub mod disassemble;
pub mod instruction;
pub mod opcode;
pub mod register;

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
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

// todo: support trapping
#[derive(Debug)]
pub enum DecodeError {
    Other,
    InvalidInstruction(u32),
    Compressed(CompressedDecodeError),
    UnimplementedExtension(Extension, u32),
}

impl From<CompressedDecodeError> for DecodeError {
    fn from(e: CompressedDecodeError) -> Self {
        Self::Compressed(e)
    }
}

#[derive(Debug)]
pub enum CompressedDecodeError {
    InvalidInstruction(u16),
    UnimplementedExtension(Extension, u16),
}

#[derive(Debug, Copy, Clone)]
pub enum Extension {
    D,
    F,
}

impl std::fmt::Display for Extension {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::D => write!(f, "RV32-D"),
            Self::F => write!(f, "RV32-F"),
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
