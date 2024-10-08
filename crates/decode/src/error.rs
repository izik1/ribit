use core::fmt;

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

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::D => f.write_str("RV32-D"),
            Self::F => f.write_str("RV32-F"),
        }
    }
}
