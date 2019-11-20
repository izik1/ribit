use super::{alloc::RegisterManager, BasicBlock, InstructionInfo, JitContext};
use assembler::mnemonic_parameter_types::{
    memory::Memory,
    registers::{Register32Bit, Register64Bit},
};
use assembler::InstructionStream;

use crate::register::{NativeRegister, RiscVRegister};

use crate::{
    instruction::{self, Instruction},
    opcode,
};

pub(super) fn generate_register_writeback(
    basic_block: &mut InstructionStream,
    native_reg: NativeRegister,
    rv_reg: RiscVRegister,
) {
    basic_block.mov_Any32BitMemory_Register32Bit(
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
        native_reg.as_assembly_reg32(),
    );
}

pub(super) fn generate_register_read(
    basic_block: &mut InstructionStream,
    native_reg: NativeRegister,
    rv_reg: RiscVRegister,
) {
    basic_block.mov_Register32Bit_Any32BitMemory(
        native_reg.as_assembly_reg32(),
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
    );
}

// todo: support native_register arguments
pub(super) fn generate_register_write_imm(
    basic_block: &mut InstructionStream,
    rv_reg: RiscVRegister,
    value: u32,
) {
    basic_block.mov_Any32BitMemory_Immediate32Bit(
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
        value.into(),
    );
}

pub(super) fn generate_basic_block_end(
    block: &mut InstructionStream,
    branch: InstructionInfo,
    reg_manager: &mut super::alloc::RegisterManager,
) {
    let next_start_address = branch.end_address();

    let branch_instruction = branch.instruction;

    match branch_instruction {
        Instruction::J(instruction::J {
            imm,
            rd,
            opcode: opcode::J::JAL,
        }) => {
            let res_pc = next_start_address.wrapping_add(imm);

            // before we return, and before we `generate_register_write_imm` we need to make sure that all the registers are done.
            reg_manager.free_all(block);

            if let Some(rd) = rd {
                generate_register_write_imm(block, rd, next_start_address);
            }

            block.mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, res_pc.into());

            block.ret();
        }

        Instruction::I(instruction::I {
            imm,
            rd,
            opcode: opcode::I::JALR,
            rs1,
        }) => {
            // before we return, and before we `generate_register_write_imm` we need to make sure that all the registers are done.
            reg_manager.free_all(block);

            if let Some(rd) = rd {
                generate_register_write_imm(block, rd, next_start_address);
            }

            block.mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, (imm as i16).into());

            if let Some(rs1) = rs1 {
                block.add_Register32Bit_Any32BitMemory(
                    Register32Bit::EAX,
                    Memory::base_64_displacement(Register64Bit::RDI, rs1.as_offset().into()),
                );
            }

            block.btr_Register32Bit_Immediate8Bit(Register32Bit::EAX, 0_u8.into());

            block.ret();
        }

        Instruction::I(_) | Instruction::R(_) | Instruction::S(_) | Instruction::U(_) => {
            unreachable!("blocks can only end on a branch?")
        }
        Instruction::B(instr) => generate_branch(block, instr, reg_manager, next_start_address),
    }
}

fn generate_branch(
    block: &mut InstructionStream,
    instruction: instruction::B,
    reg_manager: &mut super::alloc::RegisterManager,
    continue_pc: u32,
) {
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
    // mov eax, <continue_pc>
    // cmp <native_reg1>, <native_reg2>
    // mov edx, <jump_addr> ; this is hacky: CMOV doesn't support immediates
    // CMOV<cond> eax, edx
    // ret

    // fixme: need to handle x0
    let rs1 = rs1.expect("todo");
    let rs2 = rs2.expect("todo");
    let native_rs1 = reg_manager.alloc(rs1, &[rs1, rs2], block);
    let native_rs2 = reg_manager.alloc(rs2, &[rs1, rs2], block);

    block.mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, (continue_pc).into());

    block.cmp_Register32Bit_Register32Bit(
        native_rs1.as_assembly_reg32(),
        native_rs2.as_assembly_reg32(),
    );

    // hack: we don't know what registers were allocated for `native_rs1` and `native_rs2`, but we need to clobber ECX.
    reg_manager.free_all(block);

    block.mov_Register32Bit_Immediate32Bit(Register32Bit::ECX, jump_addr.into());

    match opcode {
        opcode::B::BEQ => {
            block.cmove_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
        opcode::B::BNE => {
            block.cmovne_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
        opcode::B::BLT => {
            block.cmovl_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
        opcode::B::BGE => {
            block.cmovge_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
        opcode::B::BLTU => {
            block.cmovb_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
        opcode::B::BGEU => {
            block.cmovae_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX)
        }
    }

    block.ret()
}

fn generate_instruction(
    block: &mut InstructionStream,
    instruction: InstructionInfo,
    reg_manager: &mut RegisterManager,
) {
    let next_start_address = instruction.end_address();
    let InstructionInfo {
        instruction,
        start_address,
        ..
    } = instruction;

    match instruction {
        Instruction::J(_)
        | Instruction::B(_)
        | Instruction::I(instruction::I {
            opcode: opcode::I::JALR,
            ..
        }) => unreachable!("blocks cannot contain a branch"),

        Instruction::S(instruction) => {
            generate_stype_instruction(block, instruction, reg_manager, next_start_address)
        }

        _ => todo!("implement remaining instructions"),
    }
}

// todo: broken
fn generate_stype_instruction(
    block: &mut InstructionStream,
    instruction: instruction::S,
    reg_manager: &mut RegisterManager,
    next_start_address: u32,
) {
    let instruction::S {
        imm: _imm,
        rs1,
        rs2,
        opcode: _opcode,
    } = instruction;

    // todo: handle rs1 == 0 || rs2 == 0

    if let (Some(rs1), Some(rs2)) = (rs1, rs2) {
        let _native_rs1 = reg_manager.alloc(rs1, &[rs1, rs2], block);
        let _native_rs2 = reg_manager.alloc(rs2, &[rs1, rs2], block);
    }

    todo!("store instruction");

    // todo: handle blocks getting tainted
}

pub(super) fn generate_basic_block(
    ctx: &mut JitContext,
    block_instrs: Vec<InstructionInfo>,
    branch: InstructionInfo,
) {
    let start_pc = block_instrs
        .first()
        .unwrap_or_else(|| &branch)
        .start_address;
    let end_pc = branch.end_address();

    let mut block = ctx
        .buffer
        .instruction_stream(&assembler::InstructionStreamHints::default());
    let mut reg_manager = RegisterManager::new();

    for instruction in block_instrs {
        generate_instruction(&mut block, instruction, &mut reg_manager)
    }

    generate_basic_block_end(&mut block, branch, &mut reg_manager);

    let funct: BasicBlock = unsafe { std::mem::transmute(block.start_instruction_pointer()) };
    block.finish();

    let insert_idx = ctx
        .ranges
        .binary_search_by_key(&start_pc, |range| range.start)
        .unwrap_or_else(|e| e);

    ctx.blocks.insert(insert_idx, funct);
    ctx.ranges.insert(insert_idx, start_pc..end_pc);
}

extern "sysv64" fn check_ranges(pc: u32, ctx: &mut JitContext, address: u32) -> bool {
    let mut early_exit = false;

    let mut range_start: Option<usize> = None;
    let mut range_end: Option<usize> = None;

    for (idx, range) in ctx.ranges.iter().enumerate() {
        if range.contains(&address) {
            early_exit |= range.contains(&pc) && pc <= address;
            range_start = range_start.or(Some(idx));
        }

        if !range.contains(&address) && range_start != None {
            range_end = Some(idx);
            break;
        }
    }

    if let Some(range_start) = range_start {
        let range_end = range_end.unwrap_or_else(|| ctx.ranges.len());

        if range_start > range_end {
            unsafe { std::hint::unreachable_unchecked() }
        }

        if range_end > ctx.ranges.len() {
            unsafe { std::hint::unreachable_unchecked() }
        }

        ctx.ranges
            .splice(range_start..range_end, std::iter::empty());

        if range_end > ctx.blocks.len() {
            unsafe { std::hint::unreachable_unchecked() }
        }

        ctx.blocks
            .splice(range_start..range_end, std::iter::empty());
    }

    early_exit
}
