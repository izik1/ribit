use std::cmp::Ordering;
use std::fmt;
use std::num::NonZeroU8;

use ribit_core::Width;

// todo: arbitrary bitness
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Bitness(Width);

impl Bitness {
    pub const B8: Self = Self(Width::Byte);
    pub const B16: Self = Self(Width::Word);
    pub const B32: Self = Self(Width::DWord);

    #[must_use]
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

    #[must_use]
    pub const fn to_bits(self) -> u8 {
        match self.0 {
            Width::Byte => 8,
            Width::Word => 16,
            Width::DWord => 32,
        }
    }

    #[inline(always)]
    #[must_use]
    pub const fn mask(self) -> u32 {
        // can't do `(1 << bits) - 1` because of underflows.
        if self.to_bits() >= 32 { u32::MAX } else { (1 << (self.to_bits() as u32)) - 1 }
    }
}

impl From<Width> for Bitness {
    fn from(w: Width) -> Self {
        Self(w)
    }
}

impl fmt::Display for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_bits().fmt(f)
    }
}

impl fmt::Debug for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Bitness").field(&self.to_bits()).finish()
    }
}

impl PartialOrd for Bitness {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Bitness {
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_bits().cmp(&other.to_bits())
    }
}
