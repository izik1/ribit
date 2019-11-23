use super::BlockBuilder;
use crate::{instruction, jit::alloc::LoadProfile, opcode, register};

use assembler::mnemonic_parameter_types::registers::{Register32Bit, Register8Bit};

pub fn branch_conditional(
    builder: &mut BlockBuilder,
    instruction: instruction::B,
    continue_pc: u32,
) {
    use Register32Bit::EAX;
    use Register32Bit::ECX;

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

            if let Some(addr) = cmp_0(false_addr, true_addr, cmp_mode) {
                builder.mov_eax_imm(addr.into());
                return; // the branch is now unconditional, return
            }

            builder.mov_eax_imm(continue_pc);
            let rs = builder.ez_alloc(rs);
            builder.test_r32(rs);
        }

        (Some(rs1), Some(rs2)) => {
            let (rs1, rs2) = builder.ez_alloc2(rs1, rs2);

            builder.mov_eax_imm(continue_pc);
            builder
                .stream
                .cmp_Register32Bit_Register32Bit(rs1.as_asm_reg32(), rs2.as_asm_reg32());
        }
    };

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
pub fn cmp_0<T>(false_val: T, true_val: T, cmp_mode: opcode::Cmp) -> Option<T> {
    // there's some low hanging register contention fruit, cmp can optionally take a mem argument.
    match cmp_mode {
        opcode::Cmp::Geu => Some(true_val),
        opcode::Cmp::Ltu => Some(false_val),
        _ => None,
    }
}

pub fn bool_cmp(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    rs1: Option<register::RiscV>,
    rs2: Option<register::RiscV>,
    cmp_mode: opcode::Cmp,
) {
    let native_rd = match (rs1, rs2) {
        (rs1, rs2) if rs1 == rs2 => {
            let value = cmp_same_reg(0, 1, cmp_mode);

            let _ =
                builder
                    .register_manager
                    .alloc(rd, &[rd], &mut builder.stream, LoadProfile::Lazy);
            builder.write_register_imm(rd, value, true);
            return;
        }

        // None == None
        (None, None) => unreachable!(),

        (Some(rs), None) | (None, Some(rs)) => {
            // if we have rs2 we need to invert the conditions.
            let (true_val, false_val) = rs1.map_or((0, 1), |_| (1, 0));

            if let Some(value) = cmp_0(false_val, true_val, cmp_mode) {
                let _ = builder.register_manager.alloc(
                    rd,
                    &[rd],
                    &mut builder.stream,
                    LoadProfile::Lazy,
                );
                builder.write_register_imm(rd, value, true);
                return; // the branch is now unconditional, return
            }

            let (rs, rd) = builder.register_manager.alloc_2(
                rs,
                rd,
                &[rs, rd],
                &mut builder.stream,
                LoadProfile::Eager,
                LoadProfile::Lazy,
            );

            // this has to be before the cmp, because it trashes flags
            let ret = if rs == rd {
                builder.mov_eax_imm(0);
                None
            } else {
                Some(rd)
            };

            builder.test_r32(rs);

            ret
        }
        (Some(rs1), Some(rs2)) => {
            let regs = [rs1, rs2, rd];
            let rd =
                builder
                    .register_manager
                    .alloc(rd, &regs, &mut builder.stream, LoadProfile::Lazy);
            let rs1 =
                builder
                    .register_manager
                    .alloc(rs1, &regs, &mut builder.stream, LoadProfile::Eager);
            let rs2 =
                builder
                    .register_manager
                    .alloc(rs2, &regs, &mut builder.stream, LoadProfile::Eager);

            // this has to be before the cmp, because it trashes flags
            let ret = if rd == rs1 || rd == rs2 {
                builder.mov_eax_imm(0);
                None
            } else {
                Some(rd)
            };

            builder
                .stream
                .cmp_Register32Bit_Register32Bit(rs1.as_asm_reg32(), rs2.as_asm_reg32());

            ret
        }
    };

    let cmp_reg = native_rd.map_or(Register8Bit::AL, register::Native::as_asm_reg8);

    match cmp_mode {
        opcode::Cmp::Lt => builder.stream.setb_Register8Bit(cmp_reg),
        opcode::Cmp::Ltu => builder.stream.setl_Register8Bit(cmp_reg),
        _ => todo!("extended S_ instruction (non-standard, low priority)"),
    }

    if let None = native_rd {
        let native_rd =
            builder
                .register_manager
                .alloc(rd, &[rd], &mut builder.stream, LoadProfile::Lazy);
        builder
            .stream
            .mov_Register32Bit_Register32Bit(native_rd.as_asm_reg32(), Register32Bit::EAX);
    }
}
