use super::BlockBuilder;
use crate::{instruction, opcode, register};

use assembler::mnemonic_parameter_types::registers::Register32Bit;

pub fn conditional(builder: &mut BlockBuilder, instruction: instruction::B, continue_pc: u32) {
    const EAX: Register32Bit = Register32Bit::EAX;
    const ECX: Register32Bit = Register32Bit::ECX;

    let instruction::B {
        rs1,
        rs2,
        imm,
        opcode,
    } = instruction;

    let jump_addr = continue_pc.wrapping_add(imm as i16 as u32);

    // todo: try to optimize obviously false/true branches with a [rewrite + warn, rewrite, warn, <neither>] feature set
    // None would allow for less time spent in code-gen (because you don't have to find the optimizable branches)
    // Warn is good for actually testing RISC-V code generation (_not_ this JIT)
    // Rewrite is good for when non-canonical unconditional branches are common for some reason.

    // The operation looks something like this on a high level
    // ; both registers:
    // mov eax, <continue_pc>
    // cmp <native_reg1>, <native_reg2>
    // mov ecx, <jump_addr>
    // CMOV<cond> eax, ecx
    // ret
    // ; one register:
    // mov eax, <one branch path>
    // test <native_reg1>, <native_reg1>
    // mov ecx, <the other branch path>
    // cmod<cond> eax, ecx

    match (rs1, rs2) {
        (rs1, rs2) if rs1 == rs2 => {
            conditional_same_reg(builder, continue_pc, jump_addr, opcode);
            return;
        }

        // None == None
        (None, None) => unreachable!(),

        (Some(rs), None) | (None, Some(rs)) => {
            // if we have rs2 we need to invert the addresses for jumping.
            let (true_addr, false_addr) =
                rs1.map_or((continue_pc, jump_addr), |_| (jump_addr, continue_pc));

            if conditional_cmp_0(builder, false_addr, true_addr, opcode, rs) {
                return; // the branch is now unconditional, return
            }
        }

        (Some(rs1), Some(rs2)) => {
            let (rs1, rs2) = builder.ez_alloc2(rs1, rs2);

            builder
                .stream
                .cmp_Register32Bit_Register32Bit(rs1.as_asm_reg32(), rs2.as_asm_reg32());
        }
    };

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(EAX, continue_pc.into());

    // we're about to write over ECX, so make sure nothing is using it.
    builder
        .register_manager
        .clobber(register::Native::RCX, &mut builder.stream);

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(ECX, jump_addr.into());

    match opcode {
        opcode::B::BEQ => builder.stream.cmove_Register32Bit_Register32Bit(EAX, ECX),
        opcode::B::BNE => builder.stream.cmovne_Register32Bit_Register32Bit(EAX, ECX),
        opcode::B::BLT => builder.stream.cmovl_Register32Bit_Register32Bit(EAX, ECX),
        opcode::B::BGE => builder.stream.cmovge_Register32Bit_Register32Bit(EAX, ECX),
        opcode::B::BLTU => builder.stream.cmovb_Register32Bit_Register32Bit(EAX, ECX),
        opcode::B::BGEU => builder.stream.cmovae_Register32Bit_Register32Bit(EAX, ECX),
    }
}

// returns true if the branch becomes unconditional (always)
fn conditional_same_reg(
    builder: &mut BlockBuilder,
    false_addr: u32,
    true_addr: u32,
    opcode: opcode::B,
) {
    let addr = match opcode {
        opcode::B::BEQ | opcode::B::BGE | opcode::B::BGEU => true_addr,
        opcode::B::BNE | opcode::B::BLT | opcode::B::BLTU => false_addr,
    };

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, addr.into())
}

/// generates a branch based off of `cmp rs, 0`, if you want to use rs2 instead of rs1, make sure to swap
///  `[false_addr]` and `[true_addr]` as well.
/// returns true if the branch becomes unconditional.
fn conditional_cmp_0(
    builder: &mut BlockBuilder,
    false_addr: u32,
    true_addr: u32,
    opcode: opcode::B,
    rs: register::RiscV,
) -> bool {
    // there's some low hanging register contention fruit, cmp can optionally take a mem argument.
    let unconditonal_addr = match opcode {
        opcode::B::BGEU => Some(true_addr),
        opcode::B::BLTU => Some(false_addr),
        _ => None,
    };

    if let Some(addr) = unconditonal_addr {
        builder
            .stream
            .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, addr.into());
        return true;
    }

    let rs = builder.ez_alloc(rs);

    builder
        .stream
        .test_Register32Bit_Register32Bit(rs.as_asm_reg32(), rs.as_asm_reg32());
    false
}
