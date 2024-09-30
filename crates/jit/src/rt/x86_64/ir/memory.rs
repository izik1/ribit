use rasen::params::mem::Scale;

use super::register::Register;

pub struct Memory {
    base: Option<Register>,
    index: Option<Register>,
    displacement: i32,
    scale: Scale,
    has_index: bool,
    relative: bool,
    force_32x: bool,
}

impl Memory {
    /// Creates a `Memory` instance with the given [`displacement`].
    /// # NOTE
    /// The displacement is automatically compressed to i8 if it's small enough.
    pub fn displacement(displacement: i32) -> Self {
        Self {
            base: None,
            index: None,
            displacement,
            scale: Scale::X1,
            has_index: false,
            relative: false,
            force_32x: false,
        }
    }

    pub fn base(base: Register) -> Self {
        Self::base_displacement(base, 0)
    }

    pub fn base_displacement(base: Register, displacement: i32) -> Self {
        Self {
            base: Some(base),
            index: None,
            displacement,
            scale: Scale::X1,
            has_index: false,
            relative: false,
            force_32x: false,
        }
    }

    /// # Errors
    /// When [`index`] is [`crate::Register::Zsp`], as Zsp can't be used as an index.
    pub fn base_index(base: Register, index: Register) -> Result<Self, ()> {
        Self::base_index_scale(base, index, Scale::X1)
    }

    /// # Errors
    /// When [`index`] is [`crate::Register::Zsp`], as Zsp can't be used as an index.
    pub fn base_index_scale(base: Register, index: Register, scale: Scale) -> Result<Self, ()> {
        if matches!(index, Register::Register(crate::Register::Zsp, _)) {
            Err(())
        } else {
            Ok(Self {
                base: Some(base),
                index: Some(index),
                displacement: 0,
                scale,
                has_index: true,
                relative: false,
                force_32x: false,
            })
        }
    }
}
