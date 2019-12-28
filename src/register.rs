use std::num::NonZeroU8;
use rasen::params::Register;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RiscV(NonZeroU8);

// all of these functions are super trivial and should *always* be inlined.
#[allow(clippy::inline_always)]
impl RiscV {
    #[inline(always)]
    #[must_use]
    pub fn new(inner: NonZeroU8) -> Option<Self> {
        (inner.get() < 32).then(|| Self(inner))
    }

    #[inline(always)]
    #[must_use]
    pub fn with_u8(v: u8) -> Option<Self> {
        NonZeroU8::new(v).and_then(Self::new)
    }

    /// # Safety
    /// Requires [`inner`] to be 1..=31
    #[inline(always)]
    #[must_use]
    pub const unsafe fn new_unchecked(inner: u8) -> Self {
        Self(NonZeroU8::new_unchecked(inner))
    }

    #[inline(always)]
    #[must_use]
    pub const fn get(self) -> u8 {
        self.0.get()
    }

    /// Returns the offset into a register array you'd have to find [`&self`]
    #[inline(always)]
    #[must_use]
    pub const fn as_offset(self) -> u32 {
        (self.0.get() * 4) as u32
    }

    pub const X1: Self = unsafe { Self::new_unchecked(1) };
    pub const X2: Self = unsafe { Self::new_unchecked(2) };
    pub const X3: Self = unsafe { Self::new_unchecked(3) };
    pub const X4: Self = unsafe { Self::new_unchecked(4) };
    pub const X5: Self = unsafe { Self::new_unchecked(5) };
    pub const X6: Self = unsafe { Self::new_unchecked(6) };
    pub const X7: Self = unsafe { Self::new_unchecked(7) };
    pub const X8: Self = unsafe { Self::new_unchecked(8) };
    pub const X9: Self = unsafe { Self::new_unchecked(9) };
    pub const X10: Self = unsafe { Self::new_unchecked(10) };
    pub const X11: Self = unsafe { Self::new_unchecked(11) };
    pub const X12: Self = unsafe { Self::new_unchecked(12) };
    pub const X13: Self = unsafe { Self::new_unchecked(13) };
    pub const X14: Self = unsafe { Self::new_unchecked(14) };
    pub const X15: Self = unsafe { Self::new_unchecked(15) };
    pub const X16: Self = unsafe { Self::new_unchecked(16) };
    pub const X17: Self = unsafe { Self::new_unchecked(17) };
    pub const X18: Self = unsafe { Self::new_unchecked(18) };
    pub const X19: Self = unsafe { Self::new_unchecked(19) };
    pub const X20: Self = unsafe { Self::new_unchecked(20) };
    pub const X21: Self = unsafe { Self::new_unchecked(21) };
    pub const X22: Self = unsafe { Self::new_unchecked(22) };
    pub const X23: Self = unsafe { Self::new_unchecked(23) };
    pub const X24: Self = unsafe { Self::new_unchecked(24) };
    pub const X25: Self = unsafe { Self::new_unchecked(25) };
    pub const X26: Self = unsafe { Self::new_unchecked(26) };
    pub const X27: Self = unsafe { Self::new_unchecked(27) };
    pub const X28: Self = unsafe { Self::new_unchecked(28) };
    pub const X29: Self = unsafe { Self::new_unchecked(29) };
    pub const X30: Self = unsafe { Self::new_unchecked(30) };
    pub const X31: Self = unsafe { Self::new_unchecked(31) };
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum Native {
    RDX,
    RCX,
    R8,
    R9,
    // RAX,
}

impl Native {
    pub fn as_rasen_reg(self) -> Register {
        match self {
            Self::RDX => Register::Zdx,
            Self::RCX => Register::Zcx,
            Self::R8 => Register::R8,
            Self::R9 => Register::R9,
        }
    }
}
