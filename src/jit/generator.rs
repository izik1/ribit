use super::{
    alloc::{LoadProfile, RegisterManager, StoreProfile},
    BasicBlock, CheckRanges,
};

mod cmp;
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

    fn write_register_imm(
        &mut self,
        register: register::RiscV,
        value: u32,
        store_profile: Option<StoreProfile>,
    ) {
        match store_profile {
            None => match self.register_manager.find_native_register(register) {
                Some(native_reg) => {
                    self.register_manager.set_dirty(register);
                    self.mov_r32_imm32(native_reg, value);
                }

                None => generate_register_write_imm(&mut self.stream, register, value),
            },

            Some(StoreProfile::Allocate) => {
                let native_reg = self.register_manager.alloc(
                    register,
                    &[register],
                    &mut self.stream,
                    LoadProfile::Lazy,
                );
                self.register_manager.set_dirty(register);
                self.mov_r32_imm32(native_reg, value);
            }

            Some(StoreProfile::Free) => {
                match self.register_manager.find_native_register(register) {
                    Some(native_reg) => {
                        self.register_manager.set_dirty(register);
                        self.mov_r32_imm32(native_reg, value);
                        let _ = self.register_manager.free(register, &mut self.stream);
                    }

                    None => generate_register_write_imm(&mut self.stream, register, value),
                }
            }
        }
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

    fn mov_r32_imm32(&mut self, register: register::Native, imm: u32) {
        let register = register.as_asm_reg32();
        if imm == 0 {
            self.stream
                .xor_Register32Bit_Register32Bit(register, register);
        } else {
            self.stream
                .mov_Register32Bit_Immediate32Bit(register, imm.into());
        }
    }

    fn register_mov(&mut self, dest: register::RiscV, src: register::RiscV) {
        if dest != src {
            let native_src = self.ez_alloc(src);
            let native_dest = self.register_manager.alloc(
                dest,
                &[dest, src],
                &mut self.stream,
                LoadProfile::Lazy,
            );

            self.stream.mov_Register32Bit_Register32Bit(
                native_dest.as_asm_reg32(),
                native_src.as_asm_reg32(),
            );
            self.register_manager.set_dirty(dest);
        }
    }

    fn test_r32(&mut self, register: register::Native) {
        let register = register.as_asm_reg32();
        self.stream
            .test_Register32Bit_Register32Bit(register, register);
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
                builder.write_register_imm(rd, next_start_address, Some(StoreProfile::Free));
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
                builder.write_register_imm(rd, next_start_address, Some(StoreProfile::Free));
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

        Instruction::Sys(instruction::Sys { opcode }) => {
            use crate::jit::BlockReturn;
            use crate::jit::ReturnCode;
            let return_code = match opcode {
                opcode::RSys::EBREAK => ReturnCode::EBreak,
                opcode::RSys::ECALL => ReturnCode::ECall,
            };

            let return_value = BlockReturn::from_parts(next_start_address, return_code).as_u64();
            builder
                .stream
                .mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, return_value.into());
        }

        Instruction::I(_) | Instruction::R(_) | Instruction::S(_) | Instruction::U(_) => {
            unreachable!("blocks can only end on a branch?")
        }

        Instruction::B(instr) => cmp::branch_conditional(builder, instr, next_start_address),
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
        | Instruction::Sys(_)
        | Instruction::I(instruction::I {
            opcode: opcode::I::JALR,
            ..
        }) => unreachable!("blocks cannot contain a branch"),

        Instruction::S(instruction) => generate_store_instruction(builder, instruction),

        Instruction::I(_) => todo!("I type instructions are not yet implemented"),
        Instruction::U(_) => todo!("U type instructions are not yet implemented"),

        Instruction::R(instruction) => generate_register_instruction(builder, instruction),
    }
}

macro_rules! unwrap_or_return {
    ($exp:expr) => {
        if let Some(inner) = $exp {
            inner
        } else {
            return;
        }
    };
}

fn generate_register_instruction(builder: &mut BlockBuilder, instruction: instruction::R) {
    let instruction::R {
        rs1,
        rs2,
        rd,
        opcode,
    } = instruction;

    // None of these instructions do anything if RD is 0
    let rd = unwrap_or_return!(rd);

    match opcode {
        opcode::R::Shift(opcode) => generate_rshift_instruction(builder, rd, rs2, rs1, opcode),
        opcode::R::Math(opcode) => generate_rmath_instruction(builder, rd, rs2, rs1, opcode),
    }
}

fn generate_rmath_instruction(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    rs2: Option<register::RiscV>,
    rs1: Option<register::RiscV>,
    opcode: opcode::RMath,
) {
    match opcode {
        // rd = if cmp(rs1, rs2) {1} else {0}
        opcode::RMath::SCond(cmp_mode) => cmp::bool_cmp(builder, rd, rs1, rs2, cmp_mode),
        _ => todo!("Rmath::_"),
    }
}

fn generate_rshift_instruction(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    rs2: Option<register::RiscV>,
    rs1: Option<register::RiscV>,
    opcode: opcode::RShift,
) {
    let rs = if let Some(rs) = rs1 {
        rs
    } else {
        // no rs -> always 0
        builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate));
        return;
    };

    // high level:
    // mov dest, src ; ommited if not needed
    // <shift> dest, shamt

    // before shifting we need to move src -> dest
    builder.register_mov(rd, rs);

    let shamt = unwrap_or_return!(rs2).get().into();

    let dest = builder.ez_alloc(rd).as_asm_reg32();

    match opcode {
        // todo: figure out if lea would be better for 1 < shamt < 4.
        // todo: use lea for shamt == 1 IFF rd != rs
        opcode::RShift::SLLI => builder.stream.shl_Register32Bit_Immediate8Bit(dest, shamt),
        opcode::RShift::SRLI => builder.stream.shr_Register32Bit_Immediate8Bit(dest, shamt),
        opcode::RShift::SRAI => builder.stream.sar_Register32Bit_Immediate8Bit(dest, shamt),
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
