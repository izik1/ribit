use crate::{sign_extend, sign_extend_32};

pub(crate) fn i(instruction: u32) -> u16 {
    sign_extend((instruction >> 20) as u16 & 0x0fff, 12)
}

pub(crate) fn s(instruction: u32) -> u16 {
    let imm = (((instruction >> 20) & 0b0000_1111_1110_0000)
        | ((instruction >> 7) & 0b0000_0000_0001_1111)) as u16;

    sign_extend(imm, 12)
}

pub(crate) fn b(instruction: u32) -> u16 {
    let imm = (((instruction >> 19) & 0b0001_0000_0000_0000)
        | ((instruction >> 20) & 0b0000_0111_1110_0000)
        | ((instruction << 4) & 0b0000_1000_0000_0000)
        | ((instruction >> 7) & 0b0000_0000_0001_1110)) as u16;
    sign_extend(imm, 13)
}

pub(crate) fn j(instruction: u32) -> u32 {
    // abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx -> instruction::000a_dddd_dddd_cbbb_bbbb_bbb0
    let imm = ((instruction >> 11) & 0b0001_0000_0000_0000_0000_0000)
        | ((instruction >> 20) & 0b0000_0000_0000_0111_1111_1110)
        | ((instruction >> 9) & 0b0000_0000_0000_1000_0000_0000)
        | (instruction & 0b0000_1111_1111_0000_0000_0000);

    sign_extend_32(imm, 21)
}
