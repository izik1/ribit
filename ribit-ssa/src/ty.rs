use std::fmt;

mod bitness;

pub use bitness::Bitness;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Type {
    Int(Bitness),
    /// Unit type, `()` in Rust, `void` in C
    Unit,
    // basically just an int1
    Boolean,
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
            Type::Boolean => f.write_str("bool"),
        }
    }
}

pub trait ConstTy {
    type Const;
    const TY: Type;
    fn downcast(constant: Constant) -> Option<Self::Const>;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct BoolTy;

impl ConstTy for BoolTy {
    type Const = bool;
    const TY: Type = Type::Boolean;

    fn downcast(constant: Constant) -> Option<Self::Const> {
        match constant {
            Constant::Bool(b) => Some(b),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Constant {
    Int(Int),
    Bool(bool),
}

impl From<bool> for Constant {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl Constant {
    #[must_use]
    pub const fn i32(v: u32) -> Self {
        Self::Int(Int::i32(v))
    }

    #[must_use]
    pub const fn i16(v: u16) -> Self {
        Self::Int(Int::i16(v))
    }

    #[must_use]
    pub const fn i8(v: u8) -> Self {
        Self::Int(Int::i8(v))
    }

    #[must_use]
    pub const fn ty(self) -> Type {
        match self {
            Constant::Int(it) => it.ty(),
            Constant::Bool(_) => Type::Boolean,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(it) => <Int as fmt::LowerHex>::fmt(it, f),
            Constant::Bool(it) => it.fmt(f),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct Int(pub Bitness, pub u32);

impl Int {
    #[must_use]
    pub const fn i32(v: u32) -> Self {
        Self(Bitness::B32, v)
    }

    #[must_use]
    pub const fn i16(v: u16) -> Self {
        Self(Bitness::B16, v as u32)
    }

    #[must_use]
    pub const fn i8(v: u8) -> Self {
        Self(Bitness::B8, v as u32)
    }

    #[must_use]
    pub const fn ty(self) -> Type {
        Type::Int(self.0)
    }

    /// Sign extends [`self`](Self) to a [`i32`].
    #[must_use]
    pub const fn signed(self) -> i32 {
        match self.0 {
            Bitness::B8 => self.1 as i8 as i32,
            Bitness::B16 => self.1 as i16 as i32,
            Bitness::B32 => self.1 as i32,
        }
    }

    /// Zero extends [`self`](Self) to a [`u32`].
    #[must_use]
    pub const fn unsigned(self) -> u32 {
        match self.0 {
            Bitness::B8 => self.1 as u8 as u32,
            Bitness::B16 => self.1 as u16 as u32,
            Bitness::B32 => self.1 as u32,
        }
    }

    #[must_use]
    pub fn bits(self) -> u8 {
        self.0.to_bits()
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.sign_minus() {
            self.signed().fmt(f)
        } else {
            self.unsigned().fmt(f)
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
