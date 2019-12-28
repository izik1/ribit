use super::CmpKind;
use crate::ssa::BinOp;

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
pub fn binop(src1: u32, src2: u32, op: BinOp) -> u32 {
    match op {
        BinOp::And => src1 & src2,
        BinOp::Add => src1.wrapping_add(src2),
        BinOp::Or => src1 | src2,
        BinOp::Sll => src1 << (src2 & 0x1f),
        BinOp::Srl => src1 >> (src2 & 0x1f),
        BinOp::Sra => ((src1 as i32) << (src2 & 0x1f)) as u32,
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
pub fn partial_select(cond: u32, if_true: Option<u32>, if_false: Option<u32>) -> Option<u32> {
    if cond >= 1 {
        if_true
    } else {
        if_false
    }
}

#[must_use]
pub fn try_select(cond: Option<u32>, if_true: Option<u32>, if_false: Option<u32>) -> Option<u32> {
    cond.and_then(|cond| partial_select(cond, if_true, if_false))
}