use std::fmt;

mod bitness;

pub use bitness::Bitness;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Type {
    Int(Bitness),
    /// Unit type, `()` in Rust, `void` in C
    Unit,
    // basically just an int1
    // Boolean,
}

impl Type {
    pub const I8: Self = Self::Int(Bitness::B8);
    pub const I16: Self = Self::Int(Bitness::B16);
    pub const I32: Self = Self::Int(Bitness::B32);
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(b) => write!(f, "i{}", b),
            Type::Unit => f.write_str("()"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Constant {
    Int(Int),
}

impl Constant {
    pub const fn i32(v: u32) -> Self {
        Self::Int(Int::i32(v))
    }

    pub const fn i16(v: u16) -> Self {
        Self::Int(Int::i16(v))
    }

    pub const fn i8(v: u8) -> Self {
        Self::Int(Int::i8(v))
    }

    pub const fn ty(self) -> Type {
        match self {
            Constant::Int(it) => it.ty(),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(i) => <Int as fmt::LowerHex>::fmt(i, f),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Int(pub Bitness, pub u32);

impl Int {
    pub const fn i32(v: u32) -> Self {
        Self(Bitness::B32, v)
    }

    pub const fn i16(v: u16) -> Self {
        Self(Bitness::B16, v as u32)
    }

    pub const fn i8(v: u8) -> Self {
        Self(Bitness::B8, v as u32)
    }

    pub const fn ty(self) -> Type {
        Type::Int(self.0)
    }

    /// Sign extends [`self`](Self) to a [`i32`].
    pub const fn signed(self) -> i32 {
        match self.0 {
            Bitness::B8 => self.1 as i8 as i32,
            Bitness::B16 => self.1 as i16 as i32,
            Bitness::B32 => self.1 as i32,
        }
    }

    /// Zero extends [`self`](Self) to a [`u32`].
    pub const fn unsigned(self) -> u32 {
        match self.0 {
            Bitness::B8 => self.1 as u8 as u32,
            Bitness::B16 => self.1 as u16 as u32,
            Bitness::B32 => self.1 as u32,
        }
    }

    pub fn bits(&self) -> u8 {
        self.0.to_bits()
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.sign_minus() {
            return self.signed().fmt(f);
        } else {
            return self.unsigned().fmt(f);
        }
    }
}

impl fmt::LowerHex for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let width = (self.0.to_bits() / 4) + (self.0.to_bits() & 0b11 > 0) as u8;
        let width = width as usize;
        write!(f, "{:01$x}", self.1, width)
    }
}
