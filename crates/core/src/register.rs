use core::fmt;
use core::num::NonZeroU8;
use core::ops::{Index, IndexMut};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct File<T>(pub [T; RiscV::XLEN - 1]);

impl<T: fmt::Debug> fmt::Debug for File<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = ((RiscV::X1).get()..=RiscV::X31.get())
            .map(|it| RiscV::with_u8(it).unwrap())
            .map(|it| (it, &self[it]));

        f.debug_map().entries(iter).finish()
    }
}

impl<T> File<T> {
    #[must_use]
    pub const fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> Index<RiscV> for File<T> {
    type Output = T;

    #[allow(unsafe_code)]
    #[inline]
    fn index(&self, index: RiscV) -> &Self::Output {
        // Safety: index starts in the range `1..XLEN`, subtract 1 and we get `0..{XLEN - 1}` (which is exactly `Self::len`)
        let index = index.get() as usize - 1;

        unsafe { self.0.get_unchecked(index) }
    }
}

impl<T> IndexMut<RiscV> for File<T> {
    #[allow(unsafe_code)]
    #[inline]
    fn index_mut(&mut self, index: RiscV) -> &mut Self::Output {
        // Safety: index starts in the range `1..XLEN`, subtract 1 and we get `0..{XLEN - 1}` (which is exactly `Self::len`)
        let index = index.get() as usize - 1;

        unsafe { self.0.get_unchecked_mut(index) }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct RiscV(NonZeroU8);

impl fmt::Debug for RiscV {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "X{}", self.0)
    }
}

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

    /// # Safety
    /// Requires [`inner`] to be 1..=31
    #[inline(always)]
    #[must_use]
    #[allow(unsafe_code)]
    pub const unsafe fn new_unchecked(inner: u8) -> Self {
        // Safety: caller *must* provide a non-zero value for the `NonZeroU8`.
        // Safety: caller *must* provide a value < 32 to avoid library UB.
        unsafe { Self(NonZeroU8::new_unchecked(inner)) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn get(self) -> u8 {
        self.0.get()
    }

    pub const X1: Self = Self::with_u8(1).unwrap();
    pub const X2: Self = Self::with_u8(2).unwrap();
    pub const X3: Self = Self::with_u8(3).unwrap();
    pub const X4: Self = Self::with_u8(4).unwrap();
    pub const X5: Self = Self::with_u8(5).unwrap();
    pub const X6: Self = Self::with_u8(6).unwrap();
    pub const X7: Self = Self::with_u8(7).unwrap();
    pub const X8: Self = Self::with_u8(8).unwrap();
    pub const X9: Self = Self::with_u8(9).unwrap();
    pub const X10: Self = Self::with_u8(10).unwrap();
    pub const X11: Self = Self::with_u8(11).unwrap();
    pub const X12: Self = Self::with_u8(12).unwrap();
    pub const X13: Self = Self::with_u8(13).unwrap();
    pub const X14: Self = Self::with_u8(14).unwrap();
    pub const X15: Self = Self::with_u8(15).unwrap();
    pub const X16: Self = Self::with_u8(16).unwrap();
    pub const X17: Self = Self::with_u8(17).unwrap();
    pub const X18: Self = Self::with_u8(18).unwrap();
    pub const X19: Self = Self::with_u8(19).unwrap();
    pub const X20: Self = Self::with_u8(20).unwrap();
    pub const X21: Self = Self::with_u8(21).unwrap();
    pub const X22: Self = Self::with_u8(22).unwrap();
    pub const X23: Self = Self::with_u8(23).unwrap();
    pub const X24: Self = Self::with_u8(24).unwrap();
    pub const X25: Self = Self::with_u8(25).unwrap();
    pub const X26: Self = Self::with_u8(26).unwrap();
    pub const X27: Self = Self::with_u8(27).unwrap();
    pub const X28: Self = Self::with_u8(28).unwrap();
    pub const X29: Self = Self::with_u8(29).unwrap();
    pub const X30: Self = Self::with_u8(30).unwrap();
    pub const X31: Self = Self::with_u8(31).unwrap();

    pub const XLEN: usize = 32;
}
