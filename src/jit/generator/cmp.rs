use super::BlockBuilder;
use crate::{
    instruction,
    jit::alloc::{LoadProfile, StoreProfile},
    opcode, register,
};

use rasen::params::{imm::Imm32, reg::Reg32, Register};

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
    start_pc: u32,
    len: u32,
) {
    let instruction::B {
        rs1,
        rs2,
        imm,
        cmp_mode,
    } = instruction;

    let continue_pc: u32 = start_pc.wrapping_add(len);

    let jump_addr = start_pc.wrapping_add(imm as i16 as u32);

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
        .clobber(Register::Zcx, &mut builder.stream);

    builder.mov_r32_imm32(Register::Zcx, jump_addr);

    let cmp_func = match cmp_mode {
        opcode::Cmp::Eq => rasen::Assembler::cmove_reg_reg,
        opcode::Cmp::Ne => rasen::Assembler::cmovne_reg_reg,
        opcode::Cmp::Lt => rasen::Assembler::cmovl_reg_reg,
        opcode::Cmp::Ge => rasen::Assembler::cmovge_reg_reg,
        opcode::Cmp::Ltu => rasen::Assembler::cmovb_reg_reg,
        opcode::Cmp::Geu => rasen::Assembler::cmovae_reg_reg,
    };

    cmp_func(&mut builder.stream, Reg32::ZAX, Reg32::ZCX).unwrap();

    match cmp_mode {
        opcode::Cmp::Eq => builder
            .stream
            .cmove_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
        opcode::Cmp::Ne => builder
            .stream
            .cmovne_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
        opcode::Cmp::Lt => builder
            .stream
            .cmovl_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
        opcode::Cmp::Ge => builder
            .stream
            .cmovge_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
        opcode::Cmp::Ltu => builder
            .stream
            .cmovb_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
        opcode::Cmp::Geu => builder
            .stream
            .cmovae_reg_reg(Reg32::ZAX, Reg32::ZCX)
            .unwrap(),
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
    F: FnOnce(&mut BlockBuilder, register::RiscV, opcode::Cmp) -> Option<Register>,
{
    if let Some(cmp_reg) = f(builder, rd, cmp_mode) {
        match cmp_mode {
            opcode::Cmp::Lt => builder.stream.setb_reg8(cmp_reg).unwrap(),
            opcode::Cmp::Ltu => builder.stream.setl_reg8(cmp_reg).unwrap(),
            _ => todo!("extended S(I)_ instruction (non-standard, low priority)"),
        }

        if cmp_reg == Register::Zax {
            let native_rd =
                builder
                    .register_manager
                    .alloc(rd, &[], &mut builder.stream, LoadProfile::Lazy);

            builder.register_manager.set_dirty(rd);

            builder
                .stream
                .mov_reg_reg(Reg32(native_rd), Reg32::ZAX)
                .unwrap();
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
                    Some(Register::Zax)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.test_r32(rs);
                    Some(native_rd)
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
                    Some(Register::Zax)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.stream.cmp_reg_imm(rs, Imm32(immediate)).unwrap();
                    Some(native_rd)
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
                    Some(Register::Zax)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.test_r32(rs);
                    Some(native_rd)
                }
            }

            CmpValue::Cmp(rs1, rs2) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));

                if native_rd == rs1 || native_rd == rs2 {
                    // this has to be before the cmp, because it trashes flags
                    builder.mov_eax_imm(0);

                    builder.cmp_r32_r32(rs1, rs2);
                    Some(Register::Zax)
                } else {
                    builder.register_manager.set_dirty(rd);
                    builder.mov_r32_imm32(native_rd, 0);

                    builder.cmp_r32_r32(rs1, rs2);
                    Some(native_rd)
                }
            }
        }
    });
}
