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

    fn write_register_imm(&mut self, register: register::RiscV, value: u32, read_allocated: bool) {
        // optimal writes are as follows:
        // match (used_again, in_register) {
        //     (false, false) => generate_register_write_imm(...),
        //     (false, true) => generate_register_write_imm(...), // and mark it as free without writing it out if it's dirty.
        //     (true, false) => generate_register_write_imm2(...), // make sure to read it in first, mark as dirty.
        //     (true, true) => generate_register_write_imm2(...) // just overwrite it, mark as dirty.
        // }

        if let Some(native_reg) = self.register_manager.find_native_register(register) {
            // todo: add an option to free the register with this write (see table at start of function)
            if read_allocated || !self.register_manager.needs_load(register) {
                self.register_manager
                    .load(register, &mut self.stream)
                    .unwrap();
                self.register_manager.set_dirty(register).unwrap();
                generate_register_write_imm2(&mut self.stream, native_reg, value);
                return;
            }
        }

        generate_register_write_imm(&mut self.stream, register, value);
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

/// writes a immediate value to a register.
fn generate_register_write_imm2(
    stream: &mut InstructionStream,
    native_reg: register::Native,
    value: u32,
) {
    if value != 0 {
        stream.mov_Register32Bit_Immediate32Bit(native_reg.as_assembly_reg32(), value.into());
    } else {
        stream.xor_Register32Bit_Register32Bit(
            native_reg.as_assembly_reg32(),
            native_reg.as_assembly_reg32(),
        );
    }
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

            if let Some(rd) = rd {
                builder.write_register_imm(rd, next_start_address, false);

                // before we return we need to make sure that all the registers are done.
                builder.register_manager.free_all(&mut builder.stream);
            }

            builder
                .stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, res_pc.into());
        }

        Instruction::I(instruction::I {
            imm,
            rd,
            opcode: opcode::I::JALR,
            rs1,
        }) => {
            if let Some(rd) = rd {
                builder.write_register_imm(rd, next_start_address, false);

                // before we return we need to make sure that all the registers are done.
                builder.register_manager.free_all(&mut builder.stream);
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
        }

        Instruction::I(_) | Instruction::R(_) | Instruction::S(_) | Instruction::U(_) => {
            unreachable!("blocks can only end on a branch?")
        }
        Instruction::B(instr) => generate_branch(builder, instr, next_start_address),
    }

    builder.stream.ret();
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
    // ; both registers:
    // mov eax, <continue_pc>
    // cmp <native_reg1>, <native_reg2>
    // mov edx, <jump_addr>
    // CMOV<cond> eax, edx
    // ret
    // ; one register:
    // mov eax, <one branch path>
    // test <native_reg1>, <native_reg1>
    // mov edx, <the other branch path>
    // cmod<cond> eax, edx

    let early_return = match (rs1, rs2) {
        (None, None) => generate_branch_same_reg(builder, continue_pc, jump_addr, opcode),

        (Some(rs1), Some(rs2)) if rs1 == rs2 => {
            generate_branch_same_reg(builder, continue_pc, jump_addr, opcode)
        }

        (Some(rs1), None) => generate_branch_cmp_0(builder, continue_pc, jump_addr, opcode, rs1),

        (None, Some(rs2)) => generate_branch_cmp_0(builder, jump_addr, continue_pc, opcode, rs2),

        (Some(rs1), Some(rs2)) => {
            let native_rs1 = builder
                .register_manager
                .alloc(rs1, &[rs1, rs2], &mut builder.stream);

            let native_rs2 = builder
                .register_manager
                .alloc(rs2, &[rs1, rs2], &mut builder.stream);

            builder
                .register_manager
                .load(rs1, &mut builder.stream)
                .unwrap();
            builder
                .register_manager
                .load(rs2, &mut builder.stream)
                .unwrap();

            builder.stream.cmp_Register32Bit_Register32Bit(
                native_rs1.as_assembly_reg32(),
                native_rs2.as_assembly_reg32(),
            );

            false
        }
    };

    // the branch has been made unconditional and so we should return early to avoid mucking it up.
    if early_return {
        return;
    }

    builder
        .stream
        .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, (continue_pc).into());

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
}

// returns true if the branch becomes unconditional (always)
fn generate_branch_same_reg(
    builder: &mut BlockBuilder,
    false_addr: u32,
    true_addr: u32,
    opcode: opcode::B,
) -> bool {
    match opcode {
        // always true
        opcode::B::BEQ | opcode::B::BGE | opcode::B::BGEU => builder
            .stream
            .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, true_addr.into()),

        // always false
        opcode::B::BNE | opcode::B::BLT | opcode::B::BLTU => builder
            .stream
            .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, false_addr.into()),
    }

    true
}

/// generates a branch based off of `cmp rs, 0`, if you want to use rs2 instead of rs1, make sure to swap
///  `[false_addr]` and `[true_addr]` as well.
/// returns true if the branch becomes unconditional.
fn generate_branch_cmp_0(
    builder: &mut BlockBuilder,
    false_addr: u32,
    true_addr: u32,
    opcode: opcode::B,
    rs: register::RiscV,
) -> bool {
    // there's some low hanging register contention fruit, cmp can optionally take a mem argument.
    match opcode {
        // always true
        opcode::B::BGEU => {
            builder
                .stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, true_addr.into());

            return true;
        }

        // always false
        opcode::B::BLTU => {
            builder
                .stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, false_addr.into());
            return true;
        }
        _ => {}
    }

    let native_rs = builder
        .register_manager
        .alloc(rs, &[rs], &mut builder.stream);

    builder
        .register_manager
        .load(rs, &mut builder.stream)
        .unwrap();

    builder.stream.test_Register32Bit_Register32Bit(
        native_rs.as_assembly_reg32(),
        native_rs.as_assembly_reg32(),
    );
    false
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
