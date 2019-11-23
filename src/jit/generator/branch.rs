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
        cmp_mode,
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
            let addr = cmp_same_reg(continue_pc, jump_addr, cmp_mode);
            builder.mov_eax_imm(addr.into());
            return;
        }

        // None == None
        (None, None) => unreachable!(),

        (Some(rs), None) | (None, Some(rs)) => {
            // if we have rs2 we need to invert the addresses for jumping.
            let (true_addr, false_addr) =
                rs1.map_or((continue_pc, jump_addr), |_| (jump_addr, continue_pc));

            if let Some(addr) = cmp_0(builder, false_addr, true_addr, cmp_mode, rs) {
                builder.mov_eax_imm(addr.into());
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

    builder.mov_eax_imm(continue_pc);

    // we're about to write over ECX, so make sure nothing is using it.
    builder
        .register_manager
        .clobber(register::Native::RCX, &mut builder.stream);

    builder.mov_r32_imm32(register::Native::RCX, jump_addr);

    match cmp_mode {
        opcode::Cmp::Eq => builder.stream.cmove_Register32Bit_Register32Bit(EAX, ECX),
        opcode::Cmp::Ne => builder.stream.cmovne_Register32Bit_Register32Bit(EAX, ECX),
        opcode::Cmp::Lt => builder.stream.cmovl_Register32Bit_Register32Bit(EAX, ECX),
        opcode::Cmp::Ge => builder.stream.cmovge_Register32Bit_Register32Bit(EAX, ECX),
        opcode::Cmp::Ltu => builder.stream.cmovb_Register32Bit_Register32Bit(EAX, ECX),
        opcode::Cmp::Geu => builder.stream.cmovae_Register32Bit_Register32Bit(EAX, ECX),
    }
}

pub fn cmp_same_reg<T>(false_val: T, true_val: T, cmp_mode: opcode::Cmp) -> T {
    match cmp_mode {
        opcode::Cmp::Eq | opcode::Cmp::Ge | opcode::Cmp::Geu => true_val,
        opcode::Cmp::Ne | opcode::Cmp::Lt | opcode::Cmp::Ltu => false_val,
    }
}

// returns one of the input Ts if the branch becomes unconditional
pub fn cmp_0<T>(
    builder: &mut BlockBuilder,
    false_val: T,
    true_val: T,
    cmp_mode: opcode::Cmp,
    rs: register::RiscV,
) -> Option<T> {
    // there's some low hanging register contention fruit, cmp can optionally take a mem argument.
    match cmp_mode {
        opcode::Cmp::Geu => return Some(true_val),
        opcode::Cmp::Ltu => return Some(false_val),
        _ => {}
    };

    let rs = builder.ez_alloc(rs);

    builder
        .stream
        .test_Register32Bit_Register32Bit(rs.as_asm_reg32(), rs.as_asm_reg32());

    None
}
