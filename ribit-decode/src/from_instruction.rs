use ribit_core::{instruction, opcode, Width};

use crate::{decode_rd, decode_rs, sign_extend, sign_extend_32};

pub fn r(instruction: u32, opcode: opcode::R) -> instruction::R {
    let (rs1, rs2) = decode_rs(instruction);
    let rd = decode_rd(instruction);
    instruction::R { rs1, rs2, rd, opcode }
}

pub fn i(instruction: u32, opcode: opcode::I) -> instruction::I {
    let imm = sign_extend((instruction >> 20) as u16 & 0x0fff, 12);
    let rs1 = decode_rs(instruction).0;
    let rd = decode_rd(instruction);
    instruction::I { imm, rs1, rd, opcode }
}

pub fn ijump(instruction: u32, opcode: opcode::IJump) -> instruction::IJump {
    let imm = sign_extend((instruction >> 20) as u16, 12);
    let rs1 = decode_rs(instruction).0;
    let rd = decode_rd(instruction);
    instruction::IJump { imm, rs1, rd, opcode }
}

pub fn imem(instruction: u32, opcode: opcode::IMem) -> instruction::IMem {
    let imm = sign_extend(((instruction >> 20) & 0x0fff) as u16, 12);
    let rs1 = decode_rs(instruction).0;
    let rd = decode_rd(instruction);
    instruction::IMem { imm, rs1, rd, opcode }
}

pub fn s(instruction: u32, width: Width) -> instruction::S {
    let (rs1, rs2) = decode_rs(instruction);

    let imm = (((instruction >> 20) & 0b0000_1111_1110_0000)
        | ((instruction >> 7) & 0b0000_0000_0001_1111)) as u16;

    let imm = sign_extend(imm, 12);

    instruction::S { imm, rs1, rs2, width }
}

pub fn u(instruction: u32, opcode: opcode::U) -> instruction::U {
    let rd = decode_rd(instruction);

    let imm = instruction & 0xffff_f000;

    instruction::U { imm, rd, opcode }
}

pub fn j(instruction: u32, opcode: opcode::J) -> instruction::J {
    let rd = decode_rd(instruction);
    // abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx -> instruction::000a_dddd_dddd_cbbb_bbbb_bbb0
    let imm = ((instruction >> 11) & 0b0001_0000_0000_0000_0000_0000)
        | ((instruction >> 20) & 0b0000_0000_0000_0111_1111_1110)
        | ((instruction >> 9) & 0b0000_0000_0000_1000_0000_0000)
        | (instruction & 0b0000_1111_1111_0000_0000_0000);

    let imm = sign_extend_32(imm, 21);

    instruction::J { imm, rd, opcode }
}
