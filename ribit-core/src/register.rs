use std::num::NonZeroU8;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RiscV(NonZeroU8);

// all of these functions are super trivial and should *always* be inlined.
#[allow(clippy::inline_always)]
impl RiscV {
    #[inline(always)]
    #[must_use]
    pub const fn new(inner: NonZeroU8) -> Option<Self> {
        match inner.get() < 32 {
            true => Some(Self(inner)),
            false => None,
        }
    }

    #[inline(always)]
    #[must_use]
    pub const fn with_u8(v: u8) -> Option<Self> {
        match NonZeroU8::new(v) {
            Some(v) => Self::new(v),
            None => None,
        }
    }

    // internal function because `Option::unwrap` isn't const stable.
    #[track_caller]
    const fn with_u8_panicking(v: u8) -> Self {
        // just inline `Option::unwrap` for now.
        match Self::with_u8(v) {
            Some(it) => it,
            None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }

    /// # Safety
    /// Requires [`inner`] to be 1..=31
    #[inline(always)]
    #[must_use]
    #[allow(unsafe_code)]
    pub const unsafe fn new_unchecked(inner: u8) -> Self {
        // SAFETY: caller *must* provide a non-zero value for the `NonZeroU8`.
        Self(NonZeroU8::new_unchecked(inner))
    }

    #[inline(always)]
    #[must_use]
    pub const fn get(self) -> u8 {
        self.0.get()
    }

    pub const X1: Self = Self::with_u8_panicking(1);
    pub const X2: Self = Self::with_u8_panicking(2);
    pub const X3: Self = Self::with_u8_panicking(3);
    pub const X4: Self = Self::with_u8_panicking(4);
    pub const X5: Self = Self::with_u8_panicking(5);
    pub const X6: Self = Self::with_u8_panicking(6);
    pub const X7: Self = Self::with_u8_panicking(7);
    pub const X8: Self = Self::with_u8_panicking(8);
    pub const X9: Self = Self::with_u8_panicking(9);
    pub const X10: Self = Self::with_u8_panicking(10);
    pub const X11: Self = Self::with_u8_panicking(11);
    pub const X12: Self = Self::with_u8_panicking(12);
    pub const X13: Self = Self::with_u8_panicking(13);
    pub const X14: Self = Self::with_u8_panicking(14);
    pub const X15: Self = Self::with_u8_panicking(15);
    pub const X16: Self = Self::with_u8_panicking(16);
    pub const X17: Self = Self::with_u8_panicking(17);
    pub const X18: Self = Self::with_u8_panicking(18);
    pub const X19: Self = Self::with_u8_panicking(19);
    pub const X20: Self = Self::with_u8_panicking(20);
    pub const X21: Self = Self::with_u8_panicking(21);
    pub const X22: Self = Self::with_u8_panicking(22);
    pub const X23: Self = Self::with_u8_panicking(23);
    pub const X24: Self = Self::with_u8_panicking(24);
    pub const X25: Self = Self::with_u8_panicking(25);
    pub const X26: Self = Self::with_u8_panicking(26);
    pub const X27: Self = Self::with_u8_panicking(27);
    pub const X28: Self = Self::with_u8_panicking(28);
    pub const X29: Self = Self::with_u8_panicking(29);
    pub const X30: Self = Self::with_u8_panicking(30);
    pub const X31: Self = Self::with_u8_panicking(31);
}
