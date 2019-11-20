use super::{alloc::RegisterManager, BasicBlock, CheckRanges};

use assembler::mnemonic_parameter_types::{
    memory::Memory,
    registers::{Register32Bit, Register64Bit},
};

use assembler::InstructionStream;

use crate::{
    instruction::{self, Instruction},
    opcode, register,
};

pub struct BlockBuilder<'a> {
    stream: assembler::InstructionStream<'a>,
    register_manager: RegisterManager,
    check_ranges: CheckRanges,
}

impl<'a> BlockBuilder<'a> {
    pub(super) fn start(
        stream: assembler::InstructionStream<'a>,
        check_ranges: CheckRanges,
    ) -> Self {
        Self {
            stream,
            register_manager: RegisterManager::new(),
            check_ranges,
        }
    }

    pub fn make_instruction(&mut self, instruction: instruction::Info) {
        generate_instruction(self, instruction);
    }

    pub fn complete(mut self, branch_instruction: instruction::Info) -> BasicBlock {
        end_basic_block(&mut self, branch_instruction);

        // deconstruct self to avoid drop panic.
        let BlockBuilder {
            stream,
            register_manager,
            ..
        } = self;

        assert!(register_manager.is_cleared());

        let funct: BasicBlock = unsafe { std::mem::transmute(stream.start_instruction_pointer()) };
        stream.finish();

        funct
    }
}

pub(super) fn generate_register_writeback(
    basic_block: &mut InstructionStream,
    native_reg: register::Native,
    rv_reg: register::RiscV,
) {
    basic_block.mov_Any32BitMemory_Register32Bit(
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
        native_reg.as_assembly_reg32(),
    );
}

pub(super) fn generate_register_read(
    basic_block: &mut InstructionStream,
    native_reg: register::Native,
    rv_reg: register::RiscV,
) {
    basic_block.mov_Register32Bit_Any32BitMemory(
        native_reg.as_assembly_reg32(),
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
    );
}

// todo: support native_register arguments
pub(super) fn generate_register_write_imm(
    basic_block: &mut InstructionStream,
    rv_reg: register::RiscV,
    value: u32,
) {
    basic_block.mov_Any32BitMemory_Immediate32Bit(
        Memory::base_64_displacement(Register64Bit::RDI, rv_reg.as_offset().into()),
        value.into(),
    );
}

fn end_basic_block(builder: &mut BlockBuilder, branch: instruction::Info) {
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
            builder.register_manager.free_all(&mut builder.stream);

            if let Some(rd) = rd {
                generate_register_write_imm(&mut builder.stream, rd, next_start_address);
            }

            builder
                .stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, res_pc.into());

            builder.stream.ret();
        }

        Instruction::I(instruction::I {
            imm,
            rd,
            opcode: opcode::I::JALR,
            rs1,
        }) => {
            // before we return, and before we `generate_register_write_imm` we need to make sure that all the registers are done.
            builder.register_manager.free_all(&mut builder.stream);

            if let Some(rd) = rd {
                // todo: have this interact better with `builder.register_manager`
                generate_register_write_imm(&mut builder.stream, rd, next_start_address);
            }

            builder
                .stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, (imm as i16).into());

            if let Some(rs1) = rs1 {
                builder.stream.add_Register32Bit_Any32BitMemory(
                    Register32Bit::EAX,
                    Memory::base_64_displacement(Register64Bit::RDI, rs1.as_offset().into()),
                );
            }

            builder
                .stream
                .btr_Register32Bit_Immediate8Bit(Register32Bit::EAX, 0_u8.into());

            builder.stream.ret();
        }

        Instruction::I(_) | Instruction::R(_) | Instruction::S(_) | Instruction::U(_) => {
            unreachable!("blocks can only end on a branch?")
        }
        Instruction::B(instr) => generate_branch(builder, instr, next_start_address),
    }
}

fn generate_branch(builder: &mut BlockBuilder, instruction: instruction::B, continue_pc: u32) {
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
    let native_rs1 = builder
        .register_manager
        .alloc(rs1, &[rs1, rs2], &mut builder.stream);
    let native_rs2 = builder
        .register_manager
        .alloc(rs2, &[rs1, rs2], &mut builder.stream);

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, (continue_pc).into());

    builder.stream.cmp_Register32Bit_Register32Bit(
        native_rs1.as_assembly_reg32(),
        native_rs2.as_assembly_reg32(),
    );

    // hack: we don't know what registers were allocated for `native_rs1` and `native_rs2`, but we need to clobber ECX.
    builder.register_manager.free_all(&mut builder.stream);

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(Register32Bit::ECX, jump_addr.into());

    match opcode {
        opcode::B::BEQ => builder
            .stream
            .cmove_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
        opcode::B::BNE => builder
            .stream
            .cmovne_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
        opcode::B::BLT => builder
            .stream
            .cmovl_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
        opcode::B::BGE => builder
            .stream
            .cmovge_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
        opcode::B::BLTU => builder
            .stream
            .cmovb_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
        opcode::B::BGEU => builder
            .stream
            .cmovae_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::ECX),
    }

    builder.stream.ret()
}

fn generate_instruction(builder: &mut BlockBuilder, instruction: instruction::Info) {
    let next_start_address = instruction.end_address();
    let instruction::Info {
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
            generate_stype_instruction(builder, instruction, next_start_address)
        }

        _ => todo!("implement remaining instructions"),
    }
}

// todo: broken
fn generate_stype_instruction(
    builder: &mut BlockBuilder,
    instruction: instruction::S,
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
        let _native_rs1 = builder
            .register_manager
            .alloc(rs1, &[rs1, rs2], &mut builder.stream);
        let _native_rs2 = builder
            .register_manager
            .alloc(rs2, &[rs1, rs2], &mut builder.stream);
    }

    todo!("store instruction");

    // todo: handle blocks getting tainted
}
