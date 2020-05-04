use super::{
    alloc::{LoadProfile, RegisterManager, StoreProfile},
    CheckRanges,
};

mod cmp;
mod math;
mod memory;

use crate::jit::{self, Assembler};
use crate::{
    instruction::{self, Instruction},
    opcode, register, ssa, Width,
};
use rasen::params::imm::{Imm16, Imm8};
use rasen::params::reg::{Reg16, Reg8};
use rasen::params::{
    imm::Imm32,
    mem::{Mem, Mem32},
    reg::Reg32,
    Register,
};
use std::collections::HashMap;
use std::io;

pub struct BlockBuilder<'a, 'b: 'a> {
    stream: Assembler<'a, 'b>,
    register_manager: RegisterManager,
    check_ranges: CheckRanges,
}

impl<'a, 'b: 'a> BlockBuilder<'a, 'b> {
    pub(super) fn start(stream: Assembler<'a, 'b>, check_ranges: CheckRanges) -> Self {
        if !raw_cpuid::CpuId::new()
            .get_extended_feature_info()
            .map_or(false, |feats| feats.has_bmi2())
        {
            panic!("Make the crate author support code gen on x86 without bmi2");
        }

        Self {
            stream,
            register_manager: RegisterManager::new(),
            check_ranges,
        }
    }

    fn mov_reg_src(&mut self, dest: Register, src: jit::Source) -> io::Result<()> {
        match src {
            // Don't need to move because src and dest are the same.
            jit::Source::Register(src) if src == dest => Ok(()),

            jit::Source::Register(src) => self.stream.mov_reg_reg(Reg32(dest), Reg32(src)),
            jit::Source::Val(val) => {
                // todo: make the following function return a result instead of Ok wrapping an unwrap.
                self.mov_r32_imm32(dest, val);
                Ok(())
            }
        }
    }

    #[allow(unused)]
    pub fn make_block(
        &mut self,
        instructions: &[ssa::Instruction],
        allocs: &HashMap<ssa::Id, Register>,
        clobbers: &HashMap<usize, Vec<Register>>,
    ) -> io::Result<()> {
        for (idx, instruction) in instructions.iter().enumerate() {
            match instruction {
                &ssa::Instruction::LoadConst { dest, src } => {
                    // todo: make the following function return a result instead of Ok wrapping an unwrap.
                    self.mov_r32_imm32(*allocs.get(&dest).expect("dest not allocated!?"), src);
                    Ok(())
                }

                &ssa::Instruction::BinOp {
                    dest,
                    src1,
                    src2,
                    op,
                } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");
                    let src1 =
                        jit::Source::from_ssa_src(src1, allocs).expect("src1 not allocated!?");
                    let src2 =
                        jit::Source::from_ssa_src(src2, allocs).expect("src2 not allocated!?");

                    math::binop(self, dest, src1, src2, op)
                }

                &ssa::Instruction::ReadReg { dest, base, src } => {
                    let base =
                        jit::Source::from_ssa_src(base, allocs).expect("base not allocated!?");

                    let mem = Mem32(memory::src_rv_reg(base, src));
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");
                    self.stream.mov_reg_mem(dest, mem)
                }

                &ssa::Instruction::ReadMem {
                    dest,
                    src,
                    base,
                    sign_extend,
                    width,
                } => {
                    let base =
                        jit::Source::from_ssa_src(base, allocs).expect("base not allocated!?");

                    let src = jit::Source::from_ssa_src(src, allocs).expect("src not allocated!?");

                    let mem = memory::src_src(base, src);

                    let dest = Reg32(*allocs.get(&dest).expect("dest not allocated!?"));

                    match (width, sign_extend) {
                        (Width::Byte, true) => self.stream.movsx_reg_mem8(dest, mem),
                        (Width::Byte, false) => self.stream.movzx_reg_mem8(dest, mem),
                        (Width::Word, true) => self.stream.movsx_reg_mem16(dest, mem),
                        (Width::Word, false) => self.stream.movzx_reg_mem16(dest, mem),
                        (Width::DWord, _) => self.stream.mov_reg_mem(dest, mem),
                    }
                }

                &ssa::Instruction::WriteReg { dest, base, src } => {
                    let base =
                        jit::Source::from_ssa_src(base, allocs).expect("base not allocated!?");

                    let mem = Mem32(memory::src_rv_reg(base, dest));

                    let src = jit::Source::from_ssa_src(src, allocs).expect("src not allocated!?");

                    match src {
                        jit::Source::Val(src) => self.stream.mov_mem_imm(mem, Imm32(src)),
                        jit::Source::Register(src) => self.stream.mov_mem_reg(mem, src),
                    }
                }

                &ssa::Instruction::WriteMem {
                    src,
                    base,
                    addr,
                    width,
                } => {
                    let base =
                        jit::Source::from_ssa_src(base, allocs).expect("base not allocated!?");
                    let src = jit::Source::from_ssa_src(src, allocs).expect("src not allocated!?");
                    let addr =
                        jit::Source::from_ssa_src(addr, allocs).expect("addr not allocated!?");

                    let mem = memory::src_src(base, addr);

                    match (src, width) {
                        (jit::Source::Val(v), Width::Byte) => {
                            self.stream.mov_mem_imm(mem, Imm8(v as u8))
                        }
                        (jit::Source::Val(v), Width::Word) => {
                            self.stream.mov_mem_imm(mem, Imm16(v as u16))
                        }
                        (jit::Source::Val(v), Width::DWord) => {
                            self.stream.mov_mem_imm(mem, Imm32(v))
                        }
                        (jit::Source::Register(r), Width::Byte) => {
                            self.stream.mov_mem_reg(mem, Reg8(r))
                        }
                        (jit::Source::Register(r), Width::Word) => {
                            self.stream.mov_mem_reg(mem, Reg16(r))
                        }
                        (jit::Source::Register(r), Width::DWord) => {
                            self.stream.mov_mem_reg(mem, Reg32(r))
                        }
                    }
                }

                &ssa::Instruction::Cmp {
                    dest,
                    src1,
                    src2,
                    kind,
                } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");
                    let src1 =
                        jit::Source::from_ssa_src(src1, allocs).expect("src1 not allocated!?");
                    let src2 =
                        jit::Source::from_ssa_src(src2, allocs).expect("src2 not allocated!?");

                    todo!("Measure kind and use that")
                }

                // todo(perf): split into `dest = if cond { if_true } else { <undefined> }; dest = if !cond { if_false } else { dest }
                //  iff: `is_true` & `is_false` are both `Source::Val` and `cond` is not.
                &ssa::Instruction::Select {
                    dest,
                    cond,
                    if_true,
                    if_false,
                } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");

                    let if_true = jit::Source::from_ssa_src(if_true, allocs)
                        .expect("if_true not allocated!?");

                    let if_false = jit::Source::from_ssa_src(if_false, allocs)
                        .expect("if_false not allocated!?");

                    let cond =
                        jit::Source::from_ssa_src(cond, allocs).expect("cond not allocated!?");

                    // fixme: handle cond being const. It's actually quite trivial
                    let cond = cond.reg().expect("cond should be a register!");

                    let clobber_reg = clobbers.get(&idx).and_then(|regs| regs.get(0)).copied();
                    cmp::select(self, dest, cond, if_true, if_false, clobber_reg)
                }

                ssa::Instruction::Arg { .. } => continue, // todo: force something to be done with this?
                ssa::Instruction::Fence => todo!(),
                ssa::Instruction::ReadStack { .. } => todo!(),
                ssa::Instruction::WriteStack { .. } => todo!(),
                ssa::Instruction::Ret { .. } => todo!("Enforce non branch instruction?"),
            }?;
        }

        Ok(())
    }

    pub fn make_instruction(&mut self, instruction: instruction::Info) {
        generate_instruction(self, instruction);
    }

    pub fn complete(mut self, branch_instruction: instruction::Info) {
        end_basic_block(&mut self, branch_instruction);

        assert!(self.register_manager.is_cleared());

        self.stream.finish().unwrap();
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
                let native_reg =
                    self.register_manager
                        .alloc(register, &[], &mut self.stream, LoadProfile::Lazy);
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

    fn ez_alloc(&mut self, register: register::RiscV) -> Register {
        self.register_manager
            .alloc(register, &[], &mut self.stream, LoadProfile::Eager)
    }

    fn ez_alloc2(
        &mut self,
        register1: register::RiscV,
        register2: register::RiscV,
    ) -> (Register, Register) {
        self.register_manager.alloc_2(
            (register1, register2),
            &[register1, register2],
            &mut self.stream,
            (LoadProfile::Eager, LoadProfile::Eager),
        )
    }

    pub fn ez_alloc_3op(
        &mut self,
        rd: register::RiscV,
        srcs: (register::RiscV, register::RiscV),
    ) -> (Register, Register, Register) {
        self.register_manager.alloc_3(
            (rd, srcs.0, srcs.1),
            &mut self.stream,
            (LoadProfile::Lazy, LoadProfile::Eager, LoadProfile::Eager),
        )
    }

    fn mov_eax_imm(&mut self, imm: u32) {
        if imm == 0 {
            self.stream.xor_reg_reg(Reg32::ZAX, Reg32::ZAX).unwrap();
        } else {
            self.stream.mov_reg_imm(Reg32::ZAX, Imm32(imm)).unwrap();
        }
    }

    fn mov_r32_imm32(&mut self, register: Register, imm: u32) {
        let register = Reg32(register);
        if imm == 0 {
            self.stream.xor_reg_reg(register, register).unwrap();
        } else {
            self.stream.mov_reg_imm(register, Imm32(imm)).unwrap();
        }
    }

    fn register_mov(&mut self, dest: register::RiscV, src: register::RiscV) {
        if dest != src {
            let (native_dest, native_src) = self.register_manager.alloc_2(
                (dest, src),
                &[dest, src],
                &mut self.stream,
                (LoadProfile::Lazy, LoadProfile::Eager),
            );

            self.stream
                .mov_reg_reg(Reg32(native_dest), Reg32(native_src))
                .unwrap();
            self.register_manager.set_dirty(dest);
        }
    }

    fn test_r32(&mut self, register: Register) {
        let register = Reg32(register);
        self.stream.test_reg_reg(register, register).unwrap();
    }

    fn cmp_r32_r32(&mut self, register1: Register, register2: Register) {
        self.stream
            .cmp_reg_reg(Reg32(register1), Reg32(register2))
            .unwrap();
    }
}

// todo: support native_register arguments
pub(super) fn generate_register_write_imm(
    basic_block: &mut Assembler,
    rv_reg: register::RiscV,
    value: u32,
) {
    basic_block
        .mov_mem_imm(
            Mem32(Mem::base_displacement(
                Register::Zdi,
                rv_reg.as_offset() as i32,
            )),
            Imm32(value),
        )
        .unwrap();
}

fn end_basic_block(builder: &mut BlockBuilder, branch: instruction::Info) {
    let start_address = branch.start_address;
    let len = branch.len;
    let next_start_address = branch.end_address();

    let branch_instruction = branch.instruction;

    match branch_instruction {
        Instruction::J(instruction::J {
            imm,
            rd,
            opcode: opcode::J::JAL,
        }) => {
            let res_pc = start_address.wrapping_add(imm);

            if let Some(rd) = rd {
                builder.write_register_imm(rd, next_start_address, Some(StoreProfile::Free));
            }

            builder.mov_eax_imm(res_pc);
        }

        Instruction::IJump(instruction::IJump {
            imm,
            rd,
            opcode: opcode::IJump::JALR,
            rs1,
        }) => {
            if let Some(rd) = rd {
                builder.write_register_imm(rd, next_start_address, Some(StoreProfile::Free));
            }

            if let Some(rs1) = rs1 {
                builder.mov_eax_imm(imm as i16 as u32);

                // need to free all the registers before reading from mem (stupid and unoptimized, but it prevents bugs)
                builder.register_manager.free_all(&mut builder.stream);

                builder
                    .stream
                    .add_reg_mem(
                        Register::Zax,
                        Mem32(Mem::base_displacement(
                            Register::Zdi,
                            rs1.as_offset() as i32,
                        )),
                    )
                    .unwrap();

                builder.stream.btr_reg_imm8(Reg32::ZAX, 0_u8).unwrap();
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
                .mov_reg_imm64(Register::Zax, return_value)
                .unwrap();
        }

        Instruction::I(_)
        | Instruction::IMem(_)
        | Instruction::R(_)
        | Instruction::S(_)
        | Instruction::U(_) => unreachable!("blocks can only end on a branch?"),

        Instruction::B(instr) => cmp::branch_conditional(builder, instr, start_address, len),
    }

    builder.register_manager.free_all(&mut builder.stream);
    builder.stream.ret().unwrap();
}

fn generate_instruction(builder: &mut BlockBuilder, instruction: instruction::Info) {
    let instruction::Info {
        instruction,
        start_address,
        ..
    } = instruction;

    match instruction {
        Instruction::J(_) | Instruction::B(_) | Instruction::Sys(_) | Instruction::IJump(_) => {
            unreachable!("blocks cannot contain a branch")
        }

        Instruction::S(instruction) => generate_store_instruction(builder, instruction).unwrap(),

        Instruction::I(instruction) => generate_immediate_instruction(builder, instruction),
        Instruction::IMem(instruction) => generate_immediate_mem_instruction(builder, instruction),
        Instruction::U(instruction::U { opcode, imm, rd }) => {
            if let Some(rd) = rd {
                let value = match opcode {
                    opcode::U::AUIPC => start_address.wrapping_add(imm),
                    opcode::U::LUI => imm,
                };

                builder.write_register_imm(rd, value, Some(StoreProfile::Allocate));
            }
        }

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

fn generate_immediate_mem_instruction(builder: &mut BlockBuilder, instruction: instruction::IMem) {
    let instruction::IMem {
        opcode,
        imm,
        rs1,
        rd,
    } = instruction;

    let rd = unwrap_or_return!(rd);

    match opcode {
        opcode::IMem::FENCE => todo!("FENCE (nop on single hart system? MFENCE?)"),
        opcode::IMem::LD(width) => memory::load_rs_imm(builder, rd, rs1, imm, width, true),
        opcode::IMem::LDU(width) => memory::load_rs_imm(builder, rd, rs1, imm, width, false),
    }
}

fn generate_immediate_instruction(builder: &mut BlockBuilder, instruction: instruction::I) {
    let instruction::I {
        opcode,
        imm,
        rs1,
        rd,
    } = instruction;

    let imm = imm as i16 as u32;
    let rd = unwrap_or_return!(rd);

    match opcode {
        opcode::I::ADDI => math::addi(builder, imm, rd, rs1),
        opcode::I::SICond(cmp_mode) => {
            cmp::set_bool_conditional_imm(builder, rd, rs1, imm, cmp_mode)
        }

        opcode::I::XORI => math::xori(builder, imm, rd, rs1),
        opcode::I::ORI => math::ori(builder, imm, rd, rs1),
        opcode::I::ANDI => math::andi(builder, imm, rd, rs1),
        opcode::I::SLLI => math::shifti(builder, imm, rd, rs1, math::ShiftKind::LL),
        opcode::I::SRLI => math::shifti(builder, imm, rd, rs1, math::ShiftKind::RL),
        opcode::I::SRAI => math::shifti(builder, imm, rd, rs1, math::ShiftKind::RA),
    }
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

    // a list of identities (a op 0 == ?)
    // S<cc>: depends on CC
    // ADD: additive
    // AND: multiplicive
    // OR: additive
    // XOR: additive
    // SLL: additive
    // SRL: additive
    // SUB: additive
    // SRA: additive

    match opcode {
        // rd = if cmp(rs1, rs2) {1} else {0}
        opcode::R::SCond(cmp_mode) => cmp::set_bool_conditional(builder, rd, rs1, rs2, cmp_mode),

        opcode::R::ADD => match (rs1, rs2) {
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .lea_reg_mem(native_rd, Mem32(Mem::base_index(rs1, rs2).unwrap()))
                    .unwrap();
            }
        },

        opcode::R::AND => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // and eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.stream.mov_reg_reg(Reg32::ZAX, Reg32(rs2)).unwrap();
                builder.stream.add_reg_reg(Reg32::ZAX, Reg32(rs1)).unwrap();
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .mov_reg_reg(Reg32(native_rd), Reg32::ZAX)
                    .unwrap();
            }
            _ => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
        },

        opcode::R::OR => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // or eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.stream.mov_reg_reg(Reg32::ZAX, Reg32(rs2)).unwrap();
                builder.stream.or_reg_reg(Reg32::ZAX, Reg32(rs1)).unwrap();
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .mov_reg_reg(Reg32(native_rd), Reg32::ZAX)
                    .unwrap();
            }
        },

        opcode::R::XOR => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // xor eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.stream.mov_reg_reg(Reg32::ZAX, Reg32(rs2)).unwrap();
                builder.stream.xor_reg_reg(Reg32::ZAX, Reg32(rs1)).unwrap();
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .mov_reg_reg(Reg32(native_rd), Reg32::ZAX)
                    .unwrap();
            }
        },

        opcode::R::SLL => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // shl eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .shlx_reg_reg_reg(Reg32(native_rd), Reg32(rs1), Reg32(rs2))
                    .unwrap();
            }
        },

        opcode::R::SRL => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // shr eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .shrx_reg_reg_reg(Reg32(native_rd), Reg32(rs1), Reg32(rs2))
                    .unwrap();
            }
        },

        opcode::R::SUB => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // sub eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.stream.mov_reg_reg(Reg32::ZAX, Reg32(rs2)).unwrap();
                builder.stream.sub_reg_reg(Reg32::ZAX, Reg32(rs1)).unwrap();
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .mov_reg_reg(Reg32(native_rd), Reg32::ZAX)
                    .unwrap();
            }
        },

        opcode::R::SRA => match (rs1, rs2) {
            // todo: opt: avoid:
            // mov eax, rs2
            // sar eax, rs1
            // mov rd, eax
            // if [rs2, rs1].contains(rd)
            // or if rs1 == rs2
            (None, None) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
            (Some(rs), None) | (None, Some(rs)) => builder.register_mov(rd, rs),
            (Some(rs1), Some(rs2)) => {
                let (native_rd, rs1, rs2) = builder.ez_alloc_3op(rd, (rs1, rs2));
                builder.register_manager.set_dirty(rd);
                builder
                    .stream
                    .sarx_reg_reg_reg(Reg32(native_rd), Reg32(rs1), Reg32(rs2))
                    .unwrap();
            }
        },

        opcode::R::MUL
        | opcode::R::MULH
        | opcode::R::MULHSU
        | opcode::R::MULHU
        | opcode::R::DIV
        | opcode::R::DIVU
        | opcode::R::REM
        | opcode::R::REMU => todo!("M Extension (impl required)"),
    }
}

fn generate_store_instruction(
    builder: &mut BlockBuilder,
    instruction: instruction::S,
) -> std::io::Result<()> {
    let instruction::S {
        imm,
        rs1,
        rs2,
        width,
    } = instruction;

    match (rs1, rs2) {
        (None, None) => memory::store_src_0(builder, memory::Memory::new(width, imm)),
        (None, Some(base)) => {
            let base = builder.ez_alloc(base);
            memory::dyn_address(builder, base, imm)?;

            memory::store_src_0(builder, memory::Memory::mem_eax(width))
        }

        (Some(src), None) => {
            let src = builder.ez_alloc(src);
            memory::store(builder, memory::Memory::new(width, imm), src)
        }

        (Some(src), Some(base)) => {
            let (base, src) = builder.ez_alloc2(base, src);

            memory::dyn_address(builder, base, imm)?;

            memory::store(builder, memory::Memory::mem_eax(width), src)
        }
    }

    // todo: handle blocks getting tainted -- not required due to a lack of `FENCE.I`.
}
