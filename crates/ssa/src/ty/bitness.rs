use std::cmp::Ordering;
use std::fmt;
use std::num::NonZero;

use ribit_core::Width;

#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Bitness(NonZero<u8>);

impl Bitness {
    pub const B8: Self = Self::new(8).unwrap();
    pub const B16: Self = Self::new(16).unwrap();
    pub const B32: Self = Self::new(32).unwrap();

    #[must_use]
    pub const fn new(bits: u8) -> Option<Bitness> {
        match bits {
            1..=32 => Some(Self(NonZero::new(bits).unwrap())),
            _ => None,
        }
    }

    #[must_use]
    pub(crate) const fn from_width(width: Width) -> Self {
        match width {
            Width::Byte => Self::B8,
            Width::Word => Self::B16,
            Width::DWord => Self::B32,
        }
    }

    #[must_use]
    #[inline(always)]
    pub const fn to_bits(self) -> u8 {
        self.0.get()
    }

    #[inline(always)]
    #[must_use]
    pub const fn mask(self) -> u32 {
        u32::MAX >> (32 - self.0.get())
    }
}

impl From<Width> for Bitness {
    fn from(width: Width) -> Self {
        Self::from_width(width)
    }
}

impl fmt::Display for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Bitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Bitness").field(&self.0).finish()
    }
}

impl PartialOrd for Bitness {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Bitness {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}
