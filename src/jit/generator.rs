use super::{
    alloc::{LoadProfile, RegisterManager},
    BasicBlock, CheckRanges,
};

mod branch;
mod memory;

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

        assert!(self.register_manager.is_cleared());

        let funct: BasicBlock =
            unsafe { std::mem::transmute(self.stream.start_instruction_pointer()) };
        self.stream.finish();

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

    fn ez_alloc(&mut self, register: register::RiscV) -> register::Native {
        self.register_manager
            .alloc(register, &[register], &mut self.stream, LoadProfile::Eager)
    }

    fn ez_alloc2(
        &mut self,
        register1: register::RiscV,
        register2: register::RiscV,
    ) -> (register::Native, register::Native) {
        self.register_manager.alloc_2(
            register1,
            register2,
            &[register1, register2],
            &mut self.stream,
            LoadProfile::Eager,
        )
    }

    fn mov_eax_imm(&mut self, imm: u32) {
        if imm == 0 {
            self.stream
                .xor_Register32Bit_Register32Bit(Register32Bit::EAX, Register32Bit::EAX);
        } else {
            self.stream
                .mov_Register32Bit_Immediate32Bit(Register32Bit::EAX, imm.into());
        }
    }
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
    register: register::Native,
    value: u32,
) {
    let register = register.as_asm_reg32();
    if value == 0 {
        stream.xor_Register32Bit_Register32Bit(register, register);
    } else {
        stream.mov_Register32Bit_Immediate32Bit(register, value.into());
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
            }

            builder.mov_eax_imm(res_pc);
        }

        Instruction::I(instruction::I {
            imm,
            rd,
            opcode: opcode::I::JALR,
            rs1,
        }) => {
            if let Some(rd) = rd {
                builder.write_register_imm(rd, next_start_address, false);
            }

            if let Some(rs1) = rs1 {
                builder.mov_eax_imm(imm as i16 as u32);
                builder.stream.add_Register32Bit_Any32BitMemory(
                    Register32Bit::EAX,
                    Memory::base_64_displacement(Register64Bit::RDI, rs1.as_offset().into()),
                );

                builder
                    .stream
                    .btr_Register32Bit_Immediate8Bit(Register32Bit::EAX, 0_u8.into());
            } else {
                let imm = imm as i16 as u32;
                // avoid generating a btr for a constant by clearing the bit here.
                builder.mov_eax_imm(imm & !1);
            }
        }

        Instruction::I(_) | Instruction::R(_) | Instruction::S(_) | Instruction::U(_) => {
            unreachable!("blocks can only end on a branch?")
        }

        Instruction::B(instr) => branch::conditional(builder, instr, next_start_address),
    }

    builder.register_manager.free_all(&mut builder.stream);
    builder.stream.ret();
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

        Instruction::S(instruction) => generate_store_instruction(builder, instruction),

        Instruction::I(_) => todo!("I type instructions are not yet implemented"),
        Instruction::U(_) => todo!("U type instructions are not yet implemented"),

        Instruction::R(instruction::R {
            rs1,
            rs2,
            rd,
            opcode,
        }) => todo!("R type instructions need to be implemented"),
    }
}

fn generate_store_instruction(builder: &mut BlockBuilder, instruction: instruction::S) {
    let instruction::S {
        imm,
        rs1,
        rs2,
        width,
    } = instruction;

    match (rs1, rs2) {
        (None, None) => memory::store_src_0(builder, memory::Memory::new(width, imm)),
        (Some(base), None) => {
            let base = builder.ez_alloc(base);
            memory::dyn_address(builder, base, imm);

            memory::store_src_0(builder, memory::Memory::mem_eax(width));
        }

        (None, Some(src)) => {
            let src = builder.ez_alloc(src);
            memory::store(builder, memory::Memory::new(width, imm), src);
        }

        (Some(base), Some(src)) => {
            let (base, src) = builder.ez_alloc2(base, src);

            memory::dyn_address(builder, base, imm);

            memory::store(builder, memory::Memory::mem_eax(width), src);
        }
    }

    // todo: handle blocks getting tainted -- not required due to a lack of `FENCE.I`.
}
