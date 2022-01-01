use crate::reference::Reference;
use crate::ty::{Constant, Int};
use crate::{AnySource, BinOp, Bitness, CmpKind, CommutativeBinOp};

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

        (AnySource::Const(Constant::Bool(false)), CommutativeBinOp::And) => (Constant::Bool(false)),
        (AnySource::Const(Constant::Bool(true)), CommutativeBinOp::Or) => (Constant::Bool(true)),

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
pub fn cmp(src1: u32, src2: u32, mode: CmpKind) -> u32 {
    let res = match mode {
        CmpKind::Eq => src1 == src2,
        CmpKind::Ne => src1 != src2,
        CmpKind::Sge => (src1 as i32) >= (src2 as i32),
        CmpKind::Sl => (src1 as i32) < (src2 as i32),
        CmpKind::Uge => src1 >= src2,
        CmpKind::Ul => src1 < src2,
    };

    res as u32
}

#[must_use]
pub fn cmp_int(lhs: Int, rhs: Int, op: CmpKind) -> bool {
    assert_eq!(lhs.bits(), rhs.bits());

    match op {
        CmpKind::Eq => lhs.unsigned() == rhs.unsigned(),
        CmpKind::Ne => lhs.unsigned() != rhs.unsigned(),
        CmpKind::Sge => lhs.signed() >= rhs.signed(),
        CmpKind::Sl => lhs.signed() < rhs.signed(),
        CmpKind::Uge => lhs.unsigned() >= rhs.unsigned(),
        CmpKind::Ul => lhs.unsigned() < rhs.unsigned(),
    }
}

#[must_use]
pub fn binop(src1: u32, src2: u32, op: BinOp) -> u32 {
    match op {
        BinOp::Sll => src1 << (src2 & 0x1f),
        BinOp::Srl => src1 >> (src2 & 0x1f),
        BinOp::Sra => ((src1 as i32) >> (src2 & 0x1f)) as u32,
        BinOp::Sub => src1.wrapping_sub(src2),
    }
}

#[must_use]
pub fn commutative_binop(src1: u32, src2: u32, op: CommutativeBinOp) -> u32 {
    match op {
        CommutativeBinOp::And => src1 & src2,
        CommutativeBinOp::Add => src1.wrapping_add(src2),
        CommutativeBinOp::Or => src1 | src2,
        CommutativeBinOp::Xor => src1 ^ src2,
    }
}

#[must_use]
pub fn select(cond: u32, if_true: u32, if_false: u32) -> u32 {
    if cond >= 1 {
        if_true
    } else {
        if_false
    }
}

#[must_use]
pub fn partial_select_int(
    cond: bool,
    if_true: Option<Constant>,
    if_false: Option<Constant>,
) -> Option<Constant> {
    if cond {
        if_true
    } else {
        if_false
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
                true => (0_i32 - b as u32 as i32) as u32,
                false => b as u32,
            }
        }
    };
    let value = value & (1 << (target_bitness.to_bits() - 1));
    Int(target_bitness, value)
}

#[cfg(test)]
mod test {
    use crate::BinOp;

    #[test]
    fn sra_1() {
        let res = super::binop(0x80000 << 12, 0x8, BinOp::Sra);
        assert_eq!(0xff800000, res);
    }
}
