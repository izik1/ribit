use crate::ty::{Constant, Int};
use crate::{BinOp, CmpKind, Bitness};

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
        BinOp::And => src1 & src2,
        BinOp::Add => src1.wrapping_add(src2),
        BinOp::Or => src1 | src2,
        BinOp::Sll => src1 << (src2 & 0x1f),
        BinOp::Srl => src1 >> (src2 & 0x1f),
        BinOp::Sra => ((src1 as i32) >> (src2 & 0x1f)) as u32,
        BinOp::Sub => src1.wrapping_sub(src2),
        BinOp::Xor => src1 ^ src2,
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
pub fn extend_int(width: ribit_core::Width, src: Constant, signed:  bool) -> Int {
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
    let value = value & (1 << target_bitness.to_bits() - 1);
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
