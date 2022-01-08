use std::collections::HashMap;
use std::convert::TryFrom;
use std::io;

use rasen::params::imm::{Imm16, Imm32, Imm8};
use rasen::params::mem::Mem32;
use rasen::params::reg::{Reg16, Reg32, Reg64, Reg8};
use rasen::params::Register;
use ribit_core::{ReturnCode, Width};
use ribit_ssa as ssa;
use ssa::{instruction, Bitness, StackIndex, Type};

use crate::rt::x86_64::Assembler;
use crate::Source;

mod cmp;
mod math;
mod memory;

pub struct BlockBuilder<'a, 'b: 'a> {
    stream: Assembler<'a, 'b>,
}

impl<'a, 'b: 'a> BlockBuilder<'a, 'b> {
    pub(super) fn start(stream: Assembler<'a, 'b>) -> Self {
        if !raw_cpuid::CpuId::new()
            .get_extended_feature_info()
            .map_or(false, |feats| feats.has_bmi2())
        {
            panic!("Make the crate author support code gen on x86 without bmi2");
        }

        Self { stream }
    }

    fn mov_reg_src(&mut self, dest: Register, src: crate::Source) -> io::Result<()> {
        match src {
            // Don't need to move because src and dest are the same.
            crate::Source::Register(src) if src == dest => Ok(()),
            crate::Source::Register(src) => self.stream.mov_reg_reg(Reg32(dest), Reg32(src)),
            crate::Source::Val(val) => self.mov_r32_imm32(dest, val),
        }
    }

    pub fn make_block(
        &mut self,
        instructions: &[ssa::Instruction],
        allocs: &HashMap<ssa::Id, Register>,
        clobbers: &HashMap<usize, Vec<Register>>,
    ) -> io::Result<()> {
        let max_stack = ssa::analysis::max_stack(instructions);

        // ensure that we remain in the redzone for now
        // todo: support actually modifying the stack as a fallback.
        assert!(max_stack.unwrap_or(StackIndex(0)).offset(true) >= -128);

        for (idx, instruction) in instructions.iter().enumerate() {
            match instruction {
                &ssa::Instruction::BinOp { dest, src, op } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");

                    let src1 = crate::Source::from_ssa_src(src.lhs(), allocs)
                        .expect("lhs not allocated!?");

                    let src2 = crate::Source::from_ssa_src(src.rhs(), allocs)
                        .expect("src2 not allocated!?");

                    math::binop(self, dest, src1, src2, op)
                }

                &ssa::Instruction::CommutativeBinOp { dest, src1, src2, op } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");
                    let src1 = *allocs.get(&src1.id).expect("src1 not allocated!?");
                    let src2 =
                        crate::Source::from_ssa_src(src2, allocs).expect("src2 not allocated!?");

                    math::commutative_binop(self, dest, src1, src2, op)
                }

                &ssa::Instruction::ReadReg { dest, base, src } => {
                    let base = crate::Source::from_ssa_src(base.upcast(), allocs)
                        .expect("base not allocated!?");

                    let mem = Mem32(memory::src_rv_reg(base, src));
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");
                    self.stream.mov_reg_mem(dest, mem)
                }

                &ssa::Instruction::ReadMem { dest, src, base, sign_extend, width } => {
                    let base = crate::Source::from_ssa_src(base.upcast(), allocs)
                        .expect("base not allocated!?");

                    let src =
                        crate::Source::from_ssa_src(src, allocs).expect("src not allocated!?");

                    let mem = memory::src_src(base, src);

                    let dest = Reg32(*allocs.get(&dest).expect("dest not allocated!?"));

                    // todo: handle address space wraps
                    match (width, sign_extend) {
                        (Width::Byte, true) => self.stream.movsx_reg_mem8(dest, mem),
                        (Width::Byte, false) => self.stream.movzx_reg_mem8(dest, mem),
                        (Width::Word, true) => self.stream.movsx_reg_mem16(dest, mem),
                        (Width::Word, false) => self.stream.movzx_reg_mem16(dest, mem),
                        (Width::DWord, _) => self.stream.mov_reg_mem(dest, mem),
                    }
                }

                &ssa::Instruction::WriteReg { dest, base, src } => {
                    let base = crate::Source::from_ssa_src(base.upcast(), allocs)
                        .expect("base not allocated!?");

                    let mem = Mem32(memory::src_rv_reg(base, dest));

                    let src =
                        crate::Source::from_ssa_src(src, allocs).expect("src not allocated!?");

                    match src {
                        crate::Source::Val(src) => self.stream.mov_mem_imm(mem, Imm32(src)),
                        crate::Source::Register(src) => self.stream.mov_mem_reg(mem, src),
                    }
                }

                &ssa::Instruction::WriteMem { src, base, addr, width } => {
                    let base = crate::Source::from_ssa_src(base.upcast(), allocs)
                        .expect("base not allocated!?");
                    let src =
                        crate::Source::from_ssa_src(src, allocs).expect("src not allocated!?");
                    let addr = crate::Source::from_ssa_src(addr.upcast(), allocs)
                        .expect("addr not allocated!?");

                    let mem = memory::src_src(base, addr);

                    // todo: handle address space wraps
                    match (src, width) {
                        (crate::Source::Val(v), Width::Byte) => {
                            self.stream.mov_mem_imm(mem, Imm8(v as u8))
                        }
                        (crate::Source::Val(v), Width::Word) => {
                            self.stream.mov_mem_imm(mem, Imm16(v as u16))
                        }
                        (crate::Source::Val(v), Width::DWord) => {
                            self.stream.mov_mem_imm(mem, Imm32(v))
                        }
                        (crate::Source::Register(r), Width::Byte) => {
                            self.stream.mov_mem_reg(mem, Reg8(r))
                        }
                        (crate::Source::Register(r), Width::Word) => {
                            self.stream.mov_mem_reg(mem, Reg16(r))
                        }
                        (crate::Source::Register(r), Width::DWord) => {
                            self.stream.mov_mem_reg(mem, Reg32(r))
                        }
                    }
                }

                &ssa::Instruction::Cmp { dest, src, kind } => {
                    let dest = *allocs.get(&dest).expect("dest not allocated!?");

                    let src1 = crate::Source::from_ssa_src(src.lhs(), allocs)
                        .expect("lhs not allocated!?");

                    let src2 = crate::Source::from_ssa_src(src.rhs(), allocs)
                        .expect("src2 not allocated!?");

                    cmp::set_bool_conditional(self, dest, src1, src2, kind)
                }

                // todo(perf): split into `dest = if cond { if_true } else { <undefined> }; dest = if !cond { if_false } else { dest }
                //  iff: `is_true` & `is_false` are both `Source::Val` and `cond` is not.
                ssa::Instruction::Select(it) => {
                    let dest = *allocs.get(&it.dest).expect("dest not allocated!?");

                    let if_true = crate::Source::from_ssa_src(it.if_true, allocs)
                        .expect("if_true not allocated!?");

                    let if_false = crate::Source::from_ssa_src(it.if_false, allocs)
                        .expect("if_false not allocated!?");

                    let cond = *allocs.get(&it.cond.id).expect("cond not allocated!?");

                    let clobber_reg = clobbers.get(&idx).and_then(|regs| regs.get(0)).copied();
                    cmp::select(self, dest, cond, if_true, if_false, clobber_reg)
                }

                ssa::Instruction::Arg { .. } => continue, // todo: force something to be done with this?

                // fixme: don't ignore fences
                ssa::Instruction::Fence => continue,

                ssa::Instruction::ReadStack { dest, src } => {
                    let dest = *allocs.get(dest).expect("dest not allocated!?");

                    self.stream.mov_reg_mem(Reg32(dest), Mem32(memory::stack(*src, true)))
                }

                ssa::Instruction::WriteStack { dest, src } => {
                    let src = *allocs.get(&src.id).expect("src not allocated!?");

                    self.stream.mov_mem_reg(Mem32(memory::stack(*dest, true)), Reg32(src))
                }

                ssa::Instruction::ExtInt(it) => self.ext_int(it, allocs),
            }?;
        }

        Ok(())
    }

    fn ext_int(
        &mut self,
        it: &instruction::ExtInt,
        allocs: &HashMap<ssa::Id, Register>,
    ) -> Result<(), io::Error> {
        let width = Bitness::from(it.width);
        let dest = *allocs.get(&it.dest).expect("dest not allocated!?");
        let src_width = match it.src.ty {
            Type::Int(bitness) => bitness.to_bits(),
            Type::Boolean => 1,
            Type::Unit => panic!("unexpected `()` type"),
        };
        let src = *allocs.get(&it.src.id).expect("src not allocated!?");

        match it.signed {
            true if width.to_bits() != src_width => {
                let m = 32 - src_width;
                if dest != src {
                    self.stream.mov_reg_reg(Reg32(dest), Reg32(src))?;
                }

                self.stream.shl_reg_imm8(Reg32(dest), m)?;
                self.stream.shr_reg_imm8(Reg32(dest), m)
            }
            _ if dest != src => self.stream.mov_reg_reg(Reg32(dest), Reg32(src)),
            // accidental nop.
            _ => Ok(()),
        }
    }

    pub fn complete(
        mut self,
        terminator: &ssa::Terminator,
        allocs: &HashMap<ssa::Id, Register>,
        clobbers: &[Register],
    ) -> io::Result<()> {
        match *terminator {
            ssa::Terminator::Ret { code, addr } => {
                // note: addr is in the low dword, code is high dword

                let addr = crate::Source::from_ssa_src(addr, allocs).expect("addr not allocated!?");
                let code = crate::Source::from_ssa_src(code, allocs).expect("code not allocated!?");

                match (code, addr) {
                    (Source::Val(code), Source::Val(addr)) => {
                        let code = ReturnCode::new(code).expect("code must be a valid ReturnCode");
                        self.mov_zax_imm64(super::BlockReturn::from_parts(addr, code).as_u64())?;
                    }

                    (Source::Register(code @ Register::Zax), Source::Val(0)) => {
                        // this automatically leaves the lower 32 bits empty
                        self.stream.shl_reg_imm8(Reg64(code), 32)?;
                    }

                    (Source::Register(code @ Register::Zax), Source::Val(addr)) => {
                        debug_assert_eq!(clobbers.get(0), Some(&Register::Zax));
                        let clobber_reg =
                            *clobbers.get(1).expect("Expected 2 clobbers (first being Zax)");

                        // this complex string of instructions is needed becaause x86 can't do imm64s
                        self.stream.shl_reg_imm8(Reg64(code), 32)?;
                        self.mov_r32_imm32(clobber_reg, addr)?;
                        self.stream.or_reg_reg(Reg64::ZAX, Reg64(clobber_reg))?;
                    }

                    (Source::Val(0), Source::Register(Register::Zax)) => {
                        // clear the upper 32 bits of Rax
                        // todo: use `mov <clobber>, eax` to do this instead
                        self.stream.or_reg_reg(Reg32(Register::Zax), Reg32(Register::Zax))?;
                    }

                    (Source::Val(code), Source::Register(Register::Zax)) => {
                        debug_assert_eq!(clobbers.get(0), Some(&Register::Zax));
                        let clobber_reg =
                            *clobbers.get(1).expect("Expected 2 clobbers (first being Zax)");

                        // clear the upper 32 bits of Rax
                        // todo: use `mov <clobber>, eax` to do this instead
                        self.stream.or_reg_reg(Reg32::ZAX, Reg32::ZAX)?;

                        self.mov_r32_imm32(clobber_reg, code)?;
                        self.stream.shl_reg_imm8(Reg64(clobber_reg), 32)?;

                        self.stream.or_reg_reg(Reg64::ZAX, Reg64(clobber_reg))?;
                    }

                    (Source::Register(code), Source::Val(addr)) => {
                        self.stream.shl_reg_imm8(Reg64(code), 32)?;
                        self.mov_r32_imm32(Register::Zax, addr)?;
                        self.stream.or_reg_reg(Reg64::ZAX, Reg64(code))?;
                    }

                    (Source::Val(code), Source::Register(addr)) => {
                        self.mov_r32_imm32(Register::Zax, code)?;
                        self.stream.shl_reg_imm8(Reg64::ZAX, 32)?;

                        // clear the upper 32 bits of addr
                        // todo: use `mov <clobber>, addr` to do this instead
                        self.stream.or_reg_reg(Reg32(addr), Reg32(addr))?;

                        self.stream.or_reg_reg(Reg64::ZAX, Reg64(addr))?;
                    }

                    (Source::Register(code), Source::Register(addr)) => {
                        self.stream.shl_reg_imm8(Reg64(code), 32)?;
                        if code == Register::Zax {
                            self.stream.or_reg_reg(Reg64::ZAX, Reg64(addr))?;
                        } else if addr == Register::Zax {
                            // clear the upper 32 bits of Rax
                            // todo: use `mov <clobber>, eax` to do this instead
                            self.stream.or_reg_reg(Reg32::ZAX, Reg32::ZAX)?;

                            self.stream.or_reg_reg(Reg64::ZAX, Reg64(code))?;
                        } else {
                            // clear the upper 32 bits of addr
                            // todo: use `mov <clobber>, addr` to do this instead
                            self.stream.or_reg_reg(Reg32(addr), Reg32(addr))?;

                            self.stream.or_reg_reg(Reg64(code), Reg64(addr))?;
                            self.stream.mov_reg_reg(Reg64::ZAX, Reg64(code))?;
                        }
                    }
                }

                self.stream.ret()
            }
        }
    }

    fn mov_zax_imm64(&mut self, imm: u64) -> io::Result<()> {
        if let Ok(imm) = u32::try_from(imm) {
            self.mov_r32_imm32(Register::Zax, imm)
        } else {
            self.stream.mov_reg_imm64(Register::Zax, imm)
        }
    }

    fn mov_r32_imm32(&mut self, register: Register, imm: u32) -> io::Result<()> {
        let register = Reg32(register);
        if imm == 0 {
            self.stream.xor_reg_reg(register, register)
        } else {
            self.stream.mov_reg_imm(register, Imm32(imm))
        }
    }
}
