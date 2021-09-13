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
