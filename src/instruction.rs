use crate::decode::{decode_rd, decode_rs, sign_extend};

use std::num::NonZeroU8;

use crate::opcode;
pub enum Instruction {
    R(RTypeInstruction),
    I(ITypeInstruction),
    S(STypeInstruction),
    B(BTypeInstruction),
    U(UTypeInstruction),
    J(JTypeInstruction),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RiscVRegister(NonZeroU8);

// all of these functions are super trivial and should *always* be inlined.
#[allow(clippy::inline_always)]
impl RiscVRegister {
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

#[derive(Debug)]
pub struct RTypeInstruction {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::R,
}

impl RTypeInstruction {
    pub(crate) fn new(
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::R,
    ) -> Self {
        Self {
            rs1,
            rs2,
            rd,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::R) -> Self {
        let (rs1, rs2) = decode_rs(instruction);
        let rd = decode_rd(instruction);
        Self {
            rs1,
            rs2,
            rd,
            opcode,
        }
    }
}

pub struct ITypeInstruction {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::I,
}

impl ITypeInstruction {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rd: Option<RiscVRegister>,
        opcode: opcode::I,
    ) -> Self {
        Self {
            imm,
            rs1,
            rd,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::I) -> Self {
        let imm = ((instruction >> 20) & 0x0fff) as u16;
        let rs1 = decode_rs(instruction).0;
        let rd = decode_rd(instruction);
        Self {
            imm,
            rs1,
            rd,
            opcode,
        }
    }
}

pub struct STypeInstruction {
    pub(crate) imm: u16,
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) opcode: opcode::S,
}

impl STypeInstruction {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        opcode: opcode::S,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::S) -> Self {
        let (rs1, rs2) = decode_rs(instruction);

        let imm = (((instruction >> 19) & 0b0000_1111_1110_0000)
            | ((instruction >> 7) & 0b0000_0000_0001_1111)) as u16;

        let imm = sign_extend(imm, 12);

        Self {
            rs1,
            rs2,
            imm,
            opcode,
        }
    }
}

pub struct BTypeInstruction {
    pub(crate) rs1: Option<RiscVRegister>,
    pub(crate) rs2: Option<RiscVRegister>,
    pub(crate) imm: u16,
    pub(crate) opcode: opcode::B,
}

impl BTypeInstruction {
    pub(crate) fn new(
        imm: u16,
        rs1: Option<RiscVRegister>,
        rs2: Option<RiscVRegister>,
        opcode: opcode::B,
    ) -> Self {
        Self {
            imm,
            rs1,
            rs2,
            opcode,
        }
    }
}

pub struct UTypeInstruction {
    imm: u32,
    rd: Option<RiscVRegister>,
    opcode: opcode::U,
}

impl UTypeInstruction {
    pub(crate) fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::U) -> Self {
        Self { imm, rd, opcode }
    }
}

pub struct JTypeInstruction {
    pub(crate) imm: u32,
    pub(crate) rd: Option<RiscVRegister>,
    pub(crate) opcode: opcode::J,
}

impl JTypeInstruction {
    pub(crate) fn new(imm: u32, rd: Option<RiscVRegister>, opcode: opcode::J) -> Self {
        Self { imm, rd, opcode }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: opcode::J) -> Self {
        let rd = decode_rd(instruction);
        // abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx -> 000a_dddd_dddd_cbbb_bbbb_bbb0
        let imm = ((instruction >> 11) & 0b0001_0000_0000_0000_0000_0000)
            | ((instruction >> 19) & 0b0000_0000_0000_0111_1111_1110)
            | ((instruction >> 9) & 0b0000_0000_0000_1000_0000_0000)
            | (instruction & 0b0000_1111_1111_0000_0000_0000);

        Self { imm, rd, opcode }
    }
}