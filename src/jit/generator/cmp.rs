use super::BlockBuilder;
use crate::{
    instruction,
    jit::alloc::{LoadProfile, StoreProfile},
    opcode, register,
};

use assembler::mnemonic_parameter_types::registers::{Register32Bit, Register8Bit};

fn bool_cmp<T>(
    rs1: Option<register::RiscV>,
    rs2: Option<register::RiscV>,
    false_value: T,
    true_value: T,
    cmp_mode: opcode::Cmp,
) -> CmpValue<T> {
    match (rs1, rs2) {
        (rs1, rs2) if rs1 == rs2 => {
            CmpValue::Const(cmp_same_val(false_value, true_value, cmp_mode))
        }

        // None == None
        (None, None) => unreachable!(),

        (Some(rs), None) | (None, Some(rs)) => {
            // if we have rs2 we need to invert the conditions.
            let (true_value, false_value) = match rs1.is_some() {
                true => (true_value, false_value),
                false => (false_value, true_value),
            };

            match cmp_0(false_value, true_value, cmp_mode) {
                Some(const_value) => CmpValue::Const(const_value),
                None => CmpValue::Test(rs),
            }
        }

        (Some(rs1), Some(rs2)) => CmpValue::Cmp(rs1, rs2),
    }
}

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

    // The operation looks something like this on a high level
    // ; both registers:
    // mov eax, <continue_pc>
    // cmp <native_reg1>, <native_reg2>
    // mov ecx, <jump_addr>
    // cmov<cond> eax, ecx
    // ret
    // ; one register:
    // mov eax, <one branch path>
    // test <native_reg1>, <native_reg1>
    // mov ecx, <the other branch path>
    // cmov<cond> eax, ecx

    match bool_cmp(rs1, rs2, continue_pc, jump_addr, cmp_mode) {
        CmpValue::Const(addr) => {
            builder.mov_eax_imm(addr);
            return;
        }

        CmpValue::Test(rs) => {
            builder.mov_eax_imm(continue_pc);

            let rs = builder.ez_alloc(rs);
            builder.test_r32(rs);
        }

        CmpValue::Cmp(rs1, rs2) => {
            let (rs1, rs2) = builder.ez_alloc2(rs1, rs2);

            builder.mov_eax_imm(continue_pc);
            builder.cmp_r32_r32(rs1, rs2);
        }
    }

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

pub fn cmp_same_val<T>(false_val: T, true_val: T, cmp_mode: opcode::Cmp) -> T {
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

enum CmpValue<T> {
    Const(T),
    Test(register::RiscV),
    Cmp(register::RiscV, register::RiscV),
}

fn set_bool_conditional_internal<F>(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    cmp_mode: opcode::Cmp,
    f: F,
) where
    F: FnOnce(&mut BlockBuilder, register::RiscV, opcode::Cmp) -> Option<Register8Bit>,
{
    if let Some(cmp_reg) = f(builder, rd, cmp_mode) {
        match cmp_mode {
            opcode::Cmp::Lt => builder.stream.setb_Register8Bit(cmp_reg),
            opcode::Cmp::Ltu => builder.stream.setl_Register8Bit(cmp_reg),
            _ => todo!("extended S(I)_ instruction (non-standard, low priority)"),
        }

        if cmp_reg == Register8Bit::AL {
            let native_rd =
                builder
                    .register_manager
                    .alloc(rd, &[], &mut builder.stream, LoadProfile::Lazy);

            builder.register_manager.set_dirty(rd);

            builder
                .stream
                .mov_Register32Bit_Register32Bit(native_rd.as_asm_reg32(), Register32Bit::EAX);
        }
    }
}

pub fn set_bool_conditional_imm(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
    immediate: u32,
    cmp_mode: opcode::Cmp,
) {
    set_bool_conditional_internal(builder, rd, cmp_mode, |builder, rd, cmp_mode| {
        match (rs, immediate) {
            (None, _) => {
                builder.write_register_imm(
                    rd,
                    cmp_same_val(0, 1, cmp_mode),
                    Some(StoreProfile::Allocate),
                );
                None
            }

            (Some(rs), 0) => {
                if let Some(const_value) = cmp_0(0, 1, cmp_mode) {
                    builder.write_register_imm(rd, const_value, Some(StoreProfile::Allocate));
                    return None;
                }

                let (native_rd, rs) = builder.register_manager.alloc_2(
                    (rd, rs),
                    &[rd, rs],
                    &mut builder.stream,
                    (LoadProfile::Lazy, LoadProfile::Eager),
                );

                if rs == native_rd {
                    // this has to be before the cmp, because it trashes flags
                    builder.mov_eax_imm(0);

                    builder.test_r32(rs);
                    Some(Register8Bit::AL)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.test_r32(rs);
                    Some(native_rd.as_asm_reg8())
                }
            }

            (Some(rs), _) => {
                let (native_rd, rs) = builder.register_manager.alloc_2(
                    (rd, rs),
                    &[rd, rs],
                    &mut builder.stream,
                    (LoadProfile::Lazy, LoadProfile::Eager),
                );

                if rs == native_rd {
                    // this has to be before the cmp, because it trashes flags
                    builder.mov_eax_imm(0);

                    builder.test_r32(rs);
                    Some(Register8Bit::AL)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder
                        .stream
                        .cmp_Register32Bit_Immediate32Bit(rs.as_asm_reg32(), immediate.into());
                    Some(native_rd.as_asm_reg8())
                }
            }
        }
    });
}

pub fn set_bool_conditional(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    rs1: Option<register::RiscV>,
    rs2: Option<register::RiscV>,
    cmp_mode: opcode::Cmp,
) {
    set_bool_conditional_internal(builder, rd, cmp_mode, |builder, rd, cmp_mode| {
        match bool_cmp(rs1, rs2, 0, 1, cmp_mode) {
            CmpValue::Const(v) => {
                builder.write_register_imm(rd, v, Some(StoreProfile::Allocate));
                None
            }

            CmpValue::Test(rs) => {
                let (native_rd, rs) = builder.register_manager.alloc_2(
                    (rd, rs),
                    &[rd, rs],
                    &mut builder.stream,
                    (LoadProfile::Lazy, LoadProfile::Eager),
                );

                if rs == native_rd {
                    // this has to be before the cmp, because it trashes flags
                    builder.mov_eax_imm(0);

                    builder.test_r32(rs);
                    Some(Register8Bit::AL)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.test_r32(rs);
                    Some(native_rd.as_asm_reg8())
                }
            }

            CmpValue::Cmp(rs1, rs2) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));

                if native_rd == rs1 || native_rd == rs2 {
                    // this has to be before the cmp, because it trashes flags
                    builder.mov_eax_imm(0);

                    builder.cmp_r32_r32(rs1, rs2);
                    Some(Register8Bit::AL)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.cmp_r32_r32(rs1, rs2);
                    Some(native_rd.as_asm_reg8())
                }
            }
        }
    });
}
