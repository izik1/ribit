use core::fmt;

use crate::icmp::PartialICmp;
use crate::instruction::CommutativeBinArgs;
use crate::reference::Reference;
use crate::ty::{self, Constant, Int};
use crate::{AnySource, Bitness, CmpKind, CommutativeBinOp, ShiftOp};

#[must_use]
pub fn commutative_identity(args: CommutativeBinArgs) -> Option<Reference> {
    let CommutativeBinArgs { src1: lhs, src2: rhs, op } = args;
    ty::assert_types_eq!(lhs.ty, rhs.ty());

    match (rhs, op) {
        (AnySource::Const(c), CommutativeBinOp::And) if c == Constant::umax(c.ty()) => Some(lhs),

        (
            AnySource::Const(c),
            CommutativeBinOp::Xor | CommutativeBinOp::Add | CommutativeBinOp::Or,
        ) if c == Constant::umin(c.ty()) => Some(lhs),

        (AnySource::Ref(rhs), CommutativeBinOp::And | CommutativeBinOp::Or) if lhs.id == rhs.id => {
            Some(lhs)
        }

        _ => None,
    }
}

#[must_use]
pub fn commutative_absorb(args: CommutativeBinArgs) -> Option<Constant> {
    let CommutativeBinArgs { src1: lhs, src2: rhs, op } = args;
    let c = match (rhs, op) {
        (AnySource::Const(c), CommutativeBinOp::And) if c == Constant::umin(c.ty()) => {
            Constant::umin(c.ty())
        }
        (AnySource::Const(c), CommutativeBinOp::Or) if c == Constant::umax(c.ty()) => {
            Constant::umax(c.ty())
        }

        (AnySource::Ref(rhs), CommutativeBinOp::Xor) if lhs.id == rhs.id => Constant::umin(lhs.ty),

        _ => return None,
    };

    Some(c)
}

#[must_use]
pub(crate) fn neg(v: Constant) -> Constant {
    match v {
        Constant::Int(it) => {
            // `-SIGNED_MIN` -> `-SIGNED_MIN` -> `+SIGNED_MIN as unsigned`, which is perfect,
            // it matches with what you'd expect for `0i8 - 128i8 -> 0 + (-128i8) -> 0 + 128u8`, likewise for bigger integers.
            // this will only actually trigger for 32 bit integers because those are the biggest we can store.
            Constant::Int(Int::truncate(it.0, it.signed().wrapping_neg().cast_unsigned()))
        }
        Constant::Bool(_) => panic!("attempted to negate boolean"),
    }
}

#[must_use]
pub fn sub(lhs: Constant, rhs: Constant) -> Constant {
    let (lhs, rhs) = int_consts(lhs, rhs, "sub");

    Constant::Int(Int::truncate(lhs.0, lhs.unsigned().wrapping_sub(rhs.unsigned())))
}

#[must_use]
pub fn icmp(lhs: Constant, rhs: Constant, kind: CmpKind) -> bool {
    lhs.partial_cmp_with(&rhs, kind).unwrap_or_else(|| ty::mismatch(lhs.ty(), rhs.ty()))
}

pub(crate) fn icmp_constant(src2: Constant, op: CmpKind) -> Option<bool> {
    use crate::Inequality;

    let inequality = match op {
        // equal and not equal could be comparing to anything.
        CmpKind::Eq | CmpKind::Ne => return None,
        CmpKind::Inequality(inequality) => inequality,
    };

    // what does return a static value?
    let other = match inequality {
        // x u<  0    -> false
        // x u>= 0    -> true
        Inequality::Ult | Inequality::Uge => Constant::umin(src2.ty()),

        // x u<= umax -> true
        // x u>  umax -> false
        Inequality::Ule | Inequality::Ugt => Constant::umax(src2.ty()),

        // x s<  smin -> false
        // x s>= smin -> true
        Inequality::Slt | Inequality::Sge => Constant::smin(src2.ty()),

        // x s<= smax -> true
        // x s>  smax -> false
        Inequality::Sle | Inequality::Sgt => Constant::smax(src2.ty()),
    };

    (src2 == other).then_some(inequality.include_eq())
}

// todo: "just work with arbitrary constants of compatable types"
#[must_use]
pub fn shift(lhs: Constant, rhs: Constant, op: ShiftOp) -> Constant {
    let (lhs, rhs) = int_consts(lhs, rhs, op);

    let rhs = rhs.unsigned() & u32::from(lhs.0.to_bits() - 1);

    let value = match op {
        ShiftOp::Sll => lhs.unsigned() << rhs,
        ShiftOp::Srl => lhs.unsigned() >> rhs,
        ShiftOp::Sra => (lhs.signed() >> rhs).cast_unsigned(),
    };

    Constant::Int(Int::truncate(lhs.0, value))
}

#[must_use]
#[track_caller]
fn int_consts<O>(lhs: Constant, rhs: Constant, op: O) -> (Int, Int)
where
    O: fmt::Display,
{
    match (lhs, rhs) {
        (Constant::Int(lhs), Constant::Int(rhs)) if lhs.ty() == rhs.ty() => (lhs, rhs),
        _ => panic!("unsupported operation `{op}` between types: ({},{})", lhs.ty(), rhs.ty()),
    }
}

// todo: "just work with arbitrary constants of compatable types"
#[must_use]
pub fn commutative_binop(lhs: Constant, rhs: Constant, op: CommutativeBinOp) -> Constant {
    let (lhs, rhs) = int_consts(lhs, rhs, op);

    let val = match op {
        CommutativeBinOp::And => lhs.1 & rhs.1,
        CommutativeBinOp::Add => lhs.1.wrapping_add(rhs.1),
        CommutativeBinOp::Or => lhs.1 | rhs.1,
        CommutativeBinOp::Xor => lhs.1 ^ rhs.1,
    };

    Constant::Int(Int::truncate(lhs.0, val))
}

#[must_use]
pub fn extend_int(width: ribit_core::Width, src: Constant, signed: bool) -> Int {
    let target_bitness = Bitness::from(width);
    let value = match src {
        Constant::Int(i) => {
            assert!(target_bitness >= i.0);
            match signed {
                true => i.signed().cast_unsigned(),
                false => i.unsigned(),
            }
        }
        Constant::Bool(b) => {
            assert!(target_bitness.to_bits() >= 1);
            match signed {
                true => (-u32::from(b).cast_signed()).cast_unsigned(),
                false => u32::from(b),
            }
        }
    };

    Int::truncate(target_bitness, value)
}

#[cfg(test)]
mod test {
    use ribit_core::Width;

    use crate::{Constant, ShiftOp};

    #[test]
    fn sra_1() {
        let res = super::shift(Constant::i32(0x8_0000 << 12), Constant::i32(0x8), ShiftOp::Sra);
        assert_eq!(Constant::i32(0xff80_0000), res);
    }

    #[test]
    fn extend_int_signed_neg1() {
        let res = super::extend_int(Width::DWord, Constant::i8(0xff), true);

        assert_eq!(res.signed(), -1);

        let res = super::extend_int(Width::DWord, Constant::Bool(true), true);

        assert_eq!(res.signed(), -1);
    }

    #[test]
    fn neg_int_min() {
        let res = super::neg(Constant::i8(i8::MIN.cast_unsigned()));
        assert_eq!(res, Constant::i8(i8::MIN.cast_unsigned()));

        let res = super::neg(Constant::i16(i16::MIN.cast_unsigned()));
        assert_eq!(res, Constant::i16(i16::MIN.cast_unsigned()));

        let res = super::neg(Constant::i32(i32::MIN.cast_unsigned()));
        assert_eq!(res, Constant::i32(i32::MIN.cast_unsigned()));
    }
}
