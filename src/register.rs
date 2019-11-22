use assembler::mnemonic_parameter_types::registers::{
    Register16Bit, Register32Bit, Register64Bit, Register8Bit,
};

use std::num::NonZeroU8;

// todo: get rid of "Register" suffix

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RiscV(NonZeroU8);

// all of these functions are super trivial and should *always* be inlined.
#[allow(clippy::inline_always)]
impl RiscV {
    #[inline(always)]
    #[must_use]
    pub fn new(inner: NonZeroU8) -> Option<Self> {
        (inner.get() < 32).then_with(|| Self(inner))
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
        (self.0.get() << 2) as u32
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
    #[must_use]
    pub fn as_asm_reg64(self) -> Register64Bit {
        match self {
            Self::RDX => Register64Bit::RDX,
            Self::RCX => Register64Bit::RCX,
            Self::R8 => Register64Bit::R8,
            Self::R9 => Register64Bit::R9,
        }
    }

    #[must_use]
    pub fn as_asm_reg32(self) -> Register32Bit {
        match self {
            Self::RDX => Register32Bit::EDX,
            Self::RCX => Register32Bit::ECX,
            Self::R8 => Register32Bit::R8D,
            Self::R9 => Register32Bit::R9D,
        }
    }

    #[must_use]
    pub fn as_asm_reg16(self) -> Register16Bit {
        match self {
            Self::RDX => Register16Bit::DX,
            Self::RCX => Register16Bit::CX,
            Self::R8 => Register16Bit::R8W,
            Self::R9 => Register16Bit::R9W,
        }
    }

    #[must_use]
    pub fn as_asm_reg8(self) -> Register8Bit {
        match self {
            Self::RDX => Register8Bit::DL,
            Self::RCX => Register8Bit::CL,
            Self::R8 => Register8Bit::R8B,
            Self::R9 => Register8Bit::R9B,
        }
    }
}
