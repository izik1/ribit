use std::fmt;

mod bitness;

pub use bitness::Bitness;

use crate::icmp::PartialICmp;

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
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
            Self::Int(b) => write!(f, "i{b}"),
            Type::Unit => f.write_str("()"),
            Type::Boolean => f.write_str("bool"),
        }
    }
}

pub trait ConstTy {
    type Const;
    const TY: Type;
    fn downcast(constant: Constant) -> Option<Self::Const>;

    fn fmt_const(constant: &Self::Const, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Bool;

impl ConstTy for Bool {
    type Const = bool;
    const TY: Type = Type::Boolean;

    fn downcast(constant: Constant) -> Option<Self::Const> {
        match constant {
            Constant::Bool(b) => Some(b),
            _ => None,
        }
    }

    fn fmt_const(constant: &Self::Const, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <bool as fmt::Display>::fmt(constant, f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct I32;

impl ConstTy for I32 {
    type Const = u32;
    const TY: Type = Type::I32;

    fn downcast(constant: Constant) -> Option<Self::Const> {
        match constant {
            Constant::Int(Int(Bitness::B32, i)) => Some(i),
            _ => None,
        }
    }

    fn fmt_const(constant: &Self::Const, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Constant as fmt::Display>::fmt(&Constant::i32(*constant), f)
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum Constant {
    Int(Int),
    Bool(bool),
}

impl Constant {
    #[inline(always)]
    #[must_use]
    pub const fn umin(ty: Type) -> Self {
        match ty {
            Type::Int(bitness) => Self::Int(Int::umin(bitness)),
            Type::Boolean => Self::Bool(false),
            Type::Unit => panic!("invalid type for constant: Unit"),
        }
    }

    #[inline(always)]
    #[must_use]
    pub const fn umax(ty: Type) -> Self {
        match ty {
            Type::Int(bitness) => Self::Int(Int::umax(bitness)),
            Type::Boolean => Self::Bool(true),
            Type::Unit => panic!("invalid type for constant: Unit"),
        }
    }

    #[inline(always)]
    #[must_use]
    pub const fn smin(ty: Type) -> Self {
        match ty {
            Type::Int(bitness) => Self::Int(Int::smin(bitness)),
            Type::Boolean => Self::Bool(true),
            Type::Unit => panic!("invalid type for constant: Unit"),
        }
    }

    #[inline(always)]
    #[must_use]
    pub const fn smax(ty: Type) -> Self {
        match ty {
            Type::Int(bitness) => Self::Int(Int::smax(bitness)),
            Type::Boolean => Self::Bool(false),
            Type::Unit => panic!("invalid type for constant: Unit"),
        }
    }

    

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
    #[inline(always)]
    pub const fn ty(self) -> Type {
        match self {
            Constant::Int(it) => it.ty(),
            Constant::Bool(_) => Type::Boolean,
        }
    }
}

impl From<bool> for Constant {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<u32> for Constant {
    fn from(v: u32) -> Self {
        Self::i32(v)
    }
}

impl PartialICmp for Constant {
    fn partial_unsigned_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs.partial_unsigned_cmp(rhs),
            (Self::Int(lhs), Self::Int(rhs)) => lhs.partial_unsigned_cmp(rhs),
            _ => None,
        }
    }

    fn partial_signed_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs.partial_signed_cmp(rhs),
            (Self::Int(lhs), Self::Int(rhs)) => lhs.partial_signed_cmp(rhs),
            _ => None,
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

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub struct Int(pub Bitness, pub u32);

impl Int {
    #[inline(always)]
    #[must_use]
    pub const fn zero(bitness: Bitness) -> Self {
        Self(bitness, 0)
    }

    #[inline(always)]
    #[must_use]
    pub const fn umin(bitness: Bitness) -> Self {
        Self::zero(bitness)
    }

    #[inline(always)]
    #[must_use]
    pub const fn umax(bitness: Bitness) -> Self {
        Self(bitness, bitness.mask())
    }

    #[inline(always)]
    #[must_use]
    pub const fn smin(bitness: Bitness) -> Self {
        Self(bitness, i32::MIN.cast_unsigned() & bitness.mask())
    }

    #[inline(always)]
    #[must_use]
    pub const fn smax(bitness: Bitness) -> Self {
        Self(bitness, i32::MAX.cast_unsigned() & bitness.mask())
    }

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
    #[inline(always)]
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
            Bitness::B32 => self.1,
        }
    }

    #[must_use]
    pub fn bits(self) -> u8 {
        self.0.to_bits()
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.sign_minus() { self.signed().fmt(f) } else { self.unsigned().fmt(f) }
    }
}

impl fmt::LowerHex for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let width = (self.0.to_bits() / 4) + u8::from(self.0.to_bits() & 0b11 > 0);
        let width = width as usize;
        write!(f, "{:01$x}", self.1, width)
    }
}

impl PartialICmp for Int {
    fn partial_unsigned_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.bits() == other.bits()).then(|| self.unsigned().cmp(&other.unsigned()))
    }

    fn partial_signed_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.bits() == other.bits()).then(|| self.signed().cmp(&other.signed()))
    }
}

#[macro_export]
macro_rules! assert_types_eq {
    ($left:expr, $right:expr $(,)?) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                if *left_val != *right_val {
                    $crate::ty::mismatch(*left_val, *right_val)
                }
            }
        }
    };
}

#[macro_export]
macro_rules! debug_assert_types_eq {
    ($left:expr, $right:expr $(,)?) => {
        if cfg!(debug_assertions) {
            $crate::assert_types_eq!($left, $right);
        }
    };
}

#[inline(never)]
#[cold]
#[track_caller]
pub fn mismatch(ty1: Type, ty2: Type) -> ! {
    assert_ne!(ty1, ty2, "BUG: Fake type mismatch!");
    panic!("mismatched types: ({ty1} != {ty2})")
}
