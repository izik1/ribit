use core::fmt;

use crate::icmp::PartialICmp;
use crate::reference::Reference;
use crate::ty::{self, Constant, Int};
use crate::{AnySource, Bitness, CmpKind, CommutativeBinOp, ShiftKind};

#[must_use]
pub fn commutative_identity(
    lhs: Reference,
    rhs: AnySource,
    op: CommutativeBinOp,
) -> Option<Reference> {
    assert_eq!(lhs.ty, rhs.ty());

    match (rhs, op) {
        (AnySource::Const(Constant::Int(i)), CommutativeBinOp::And) if i.signed() == -1 => {
            Some(lhs)
        }

        (AnySource::Const(Constant::Bool(true)), CommutativeBinOp::And) => Some(lhs),
        (AnySource::Const(Constant::Bool(false)), CommutativeBinOp::Xor | CommutativeBinOp::Or) => {
            Some(lhs)
        }

        (
            AnySource::Const(Constant::Int(i)),
            CommutativeBinOp::Xor | CommutativeBinOp::Add | CommutativeBinOp::Or,
        ) if i.unsigned() == 0 => Some(lhs),

        (AnySource::Ref(rhs), CommutativeBinOp::And)
        | (AnySource::Ref(rhs), CommutativeBinOp::Or)
            if lhs.id == rhs.id =>
        {
            Some(lhs)
        }

        _ => None,
    }
}

#[must_use]
pub fn commutative_absorb(
    lhs: Reference,
    rhs: AnySource,
    op: CommutativeBinOp,
) -> Option<Constant> {
    let c = match (rhs, op) {
        (AnySource::Const(Constant::Int(i)), CommutativeBinOp::And) if i.unsigned() == 0 => {
            Constant::Int(i)
        }

        (AnySource::Const(Constant::Int(i)), CommutativeBinOp::Or) if i.signed() == -1 => {
            Constant::Int(i)
        }

        (AnySource::Const(Constant::Bool(false)), CommutativeBinOp::And) => Constant::Bool(false),
        (AnySource::Const(Constant::Bool(true)), CommutativeBinOp::Or) => Constant::Bool(true),

        (AnySource::Ref(rhs), CommutativeBinOp::Xor) if (lhs.id == rhs.id) => match lhs.ty {
            crate::Type::Int(b) => Constant::Int(Int(b, 0)),
            crate::Type::Unit => panic!(),
            crate::Type::Boolean => Constant::Bool(false),
        },

        _ => return None,
    };

    Some(c)
}

#[must_use]
pub(crate) fn neg(v: Constant) -> Constant {
    match v {
        Constant::Int(it) => {
            // the only weird case here is -MIN = MIN, which is fine for this function, and weirdly also fine for turning `sub ref, const` into `add ref, -const`
            let val = it.signed().wrapping_neg() as u32;

            // can't do `(1 << bits) - 1` because of underflows.
            let mask = if it.bits() >= 32 { u32::MAX } else { (1 << (it.bits() as u32)) - 1 };

            Constant::Int(Int(it.0, val & mask))
        }
        Constant::Bool(_) => panic!("attempted to negate boolean"),
    }
}

#[must_use]
pub fn sub(lhs: Constant, rhs: Constant) -> Constant {
    struct Op;
    impl fmt::Display for Op {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("sub")
        }
    }

    u32_op_consts(|lhs, rhs, Op| lhs.wrapping_sub(rhs), lhs, rhs, Op)
}

#[must_use]
pub fn icmp(lhs: Constant, rhs: Constant, kind: CmpKind) -> bool {
    lhs.partial_cmp_with(&rhs, kind).unwrap_or_else(|| ty::mismatch(lhs.ty(), rhs.ty()))
}

// todo: "just work with arbitrary constants of compatable types"
#[must_use]
pub fn shift(src1: Constant, src2: Constant, op: ShiftKind) -> Constant {
    u32_op_consts(shift_u32, src1, src2, op)
}

#[must_use]
fn shift_u32(src1: u32, src2: u32, op: ShiftKind) -> u32 {
    match op {
        ShiftKind::Sll => src1 << (src2 & 0x1f),
        ShiftKind::Srl => src1 >> (src2 & 0x1f),
        ShiftKind::Sra => ((src1 as i32) >> (src2 & 0x1f)) as u32,
    }
}

#[must_use]
fn u32_op_consts<O, F>(f: F, src1: Constant, src2: Constant, op: O) -> Constant
where
    F: FnOnce(u32, u32, O) -> u32,
    O: fmt::Display,
{
    match (src1, src2) {
        (Constant::Int(Int(Bitness::B32, lhs)), Constant::Int(Int(Bitness::B32, rhs))) => {
            Constant::i32(f(lhs, rhs, op))
        }

        (lhs, rhs) => {
            panic!("unsupported operation `{op}` between types: ({},{})", lhs.ty(), rhs.ty())
        }
    }
}

// todo: "just work with arbitrary constants of compatable types"
#[must_use]
pub fn commutative_binop(src1: Constant, src2: Constant, op: CommutativeBinOp) -> Constant {
    u32_op_consts(commutative_binop_u32, src1, src2, op)
}

#[must_use]
fn commutative_binop_u32(src1: u32, src2: u32, op: CommutativeBinOp) -> u32 {
    match op {
        CommutativeBinOp::And => src1 & src2,
        CommutativeBinOp::Add => src1.wrapping_add(src2),
        CommutativeBinOp::Or => src1 | src2,
        CommutativeBinOp::Xor => src1 ^ src2,
    }
}

#[must_use]
pub fn extend_int(width: ribit_core::Width, src: Constant, signed: bool) -> Int {
    let target_bitness = Bitness::from(width);
    let value = match src {
        Constant::Int(i) => {
            assert!(target_bitness >= i.0);
            match signed {
                true => i.signed() as u32,
                false => i.unsigned(),
            }
        }
        Constant::Bool(b) => {
            assert!(target_bitness.to_bits() >= 1);
            match signed {
                true => -(b as u32 as i32) as u32,
                false => b as u32,
            }
        }
    };
    let value = value & (1 << (target_bitness.to_bits() - 1));
    Int(target_bitness, value)
}

#[cfg(test)]
mod test {
    use crate::ShiftKind;

    #[test]
    fn sra_1() {
        let res = super::shift_u32(0x80000 << 12, 0x8, ShiftKind::Sra);
        assert_eq!(0xff800000, res);
    }
}
