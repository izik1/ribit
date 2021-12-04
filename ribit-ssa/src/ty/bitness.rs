use std::fmt;
use std::num::NonZeroU8;

use ribit_core::Width;

// todo: arbitrary bitness
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Bitness(Width);

impl Bitness {
    pub const B8: Self = Self(Width::Byte);
    pub const B16: Self = Self(Width::Word);
    pub const B32: Self = Self(Width::DWord);

    pub fn new(bits: u8) -> Option<Bitness> {
        let bits = NonZeroU8::new(bits)?;

        let width = match bits.get() {
            0 => unreachable!(),
            8 => Width::Byte,
            16 => Width::Word,
            32 => Width::DWord,
            _ => return None,
        };

        Some(Self(width))
    }

    pub fn to_width(self) -> Width {
        self.0
    }

    pub fn to_bits(self) -> u8 {
        match self.0 {
            Width::Byte => 8,
            Width::Word => 16,
            Width::DWord => 32,
        }
    }
}

impl From<Width> for Bitness {
    fn from(w: Width) -> Self {
        Self(w)
    }
}

impl fmt::Display for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_bits())
    }
}

impl fmt::Debug for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Bitness").field(&self.to_bits()).finish()
    }
}

impl PartialOrd for Bitness {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_bits().partial_cmp(&other.to_bits())
    }
}

impl Ord for Bitness {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_bits().cmp(&other.to_bits())
    }
}
