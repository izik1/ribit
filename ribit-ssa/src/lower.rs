#[cfg(test)]
mod test;

use ribit_core::{instruction, opcode, register, Width};

use super::{BinOp, CmpKind, Id, Instruction, Source};
use crate::reference::Reference;
use crate::ty::{Bitness, Constant, Int};
use crate::{eval, Arg, Block, IdAllocator, Terminator};

pub struct Context {
    id_allocator: IdAllocator,
    pub pc: Source,
    registers: [Option<Source>; 32],
    register_arg: Option<Source>,
    memory_arg: Option<Source>,
    registers_written: u32,
    instructions: Vec<Instruction>,
    memory_size: u32,
}

impl Context {
    #[must_use]
    pub fn new(start_pc: u32, memory_size: u32) -> Self {
        assert!(memory_size.is_power_of_two());

        let mut self_ = Self {
            id_allocator: IdAllocator::new(),
            pc: Source::Const(Constant::i32(start_pc)),
            registers: [None; 32],
            register_arg: None,
            memory_arg: None,
            registers_written: 0x0000_0000,
            instructions: vec![],
            memory_size,
        };

        self_.register_arg = Some(self_.arg(Arg::Register));
        self_.memory_arg = Some(self_.arg(Arg::Memory));
        self_
    }

    fn arg(&mut self, arg: Arg) -> Source {
        self.instruction(|id| Instruction::Arg { dest: id, src: arg })
    }

    pub fn add_pc(&mut self, src: Source) -> Source {
        self.pc = self.add(src, self.pc);
        self.pc
    }

    pub fn override_pc(&mut self, src: Source) {
        self.pc = src;
    }

    fn instruction<F: FnOnce(Id) -> Instruction>(&mut self, f: F) -> Source {
        let id = self.id_allocator.allocate();

        let instr = f(id);
        let ty = instr.ty();

        self.instructions.push(instr);
        Source::Ref(Reference { ty, id })
    }

    pub fn binop(&mut self, op: BinOp, src1: Source, src2: Source) -> Source {
        assert_eq!(src1.ty(), src2.ty());

        if let (Some(src1), Some(src2)) = (src1.constant(), src2.constant()) {
            // "just work with arbitrary constants of compatable types"
            match (src1, src2) {
                (Constant::Int(Int(Bitness::B32, lhs)), Constant::Int(Int(Bitness::B32, rhs))) => {
                    Source::Const(Constant::i32(eval::binop(lhs, rhs, op)))
                }

                (Constant::Int(lhs), Constant::Int(rhs)) => {
                    panic!("binop between unsupported types: ({},{})", lhs.ty(), rhs.ty())
                }
            }
        } else {
            self.instruction(|dest| Instruction::BinOp { dest, src1, src2, op })
        }
    }

    pub fn read_register(&mut self, reg: register::RiscV) -> Source {
        self.registers[reg.get() as usize].unwrap_or_else(|| {
            let id = self.id_allocator.allocate();
            let instr = Instruction::ReadReg {
                dest: id,
                base: self.register_arg.expect("Register arg wasn't initialized?"),
                src: reg,
            };

            let ty = instr.ty();

            self.instructions.push(instr);

            let r = Source::Ref(Reference { ty, id });

            self.registers[reg.get() as usize] = Some(r);

            r
        })
    }

    pub fn load_register(&mut self, reg: Option<register::RiscV>) -> Source {
        reg.map_or(Source::Const(Constant::i32(0)), |reg| self.read_register(reg))
    }

    pub fn read_memory(&mut self, src: Source, width: Width, sign_extend: bool) -> Source {
        let base = self.memory_arg.expect("Memory arg wasn't initialized?");
        self.instruction(|dest| Instruction::ReadMem { dest, src, base, width, sign_extend })
    }

    pub fn write_register(&mut self, reg: register::RiscV, val: Source) {
        self.registers[reg.get() as usize] = Some(val);
        self.registers_written |= 1 << reg.get();
    }

    pub fn write_memory(&mut self, addr: Source, val: Source, width: Width) {
        self.instructions.push(Instruction::WriteMem {
            addr,
            base: self.memory_arg.expect("Memory arg wasn't initialized?"),
            src: val,
            width,
        });
    }

    pub fn or(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Or, src1, src2)
    }

    pub fn sll(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Sll, src1, src2)
    }

    pub fn srl(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Srl, src1, src2)
    }

    pub fn sra(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Sra, src1, src2)
    }

    pub fn xor(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Xor, src1, src2)
    }

    pub fn and(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::And, src1, src2)
    }

    pub fn add(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Add, src1, src2)
    }

    pub fn sub(&mut self, src1: Source, src2: Source) -> Source {
        self.binop(BinOp::Sub, src1, src2)
    }

    pub fn cmp(&mut self, src1: Source, src2: Source, mode: CmpKind) -> Source {
        if let (Some(src1), Some(src2)) = (src1.constant(), src2.constant()) {
            match (src1, src2) {
                (Constant::Int(lhs), Constant::Int(rhs)) => {
                    Source::Const(Constant::Int(eval::cmp_int(lhs, rhs, mode)))
                }
            }
        } else {
            self.instruction(|dest| Instruction::Cmp { dest, src1, src2, kind: mode })
        }
    }

    pub fn select(&mut self, cond: Source, if_true: Source, if_false: Source) -> Source {
        let selected = cond.constant().and_then(|cond| match cond {
            Constant::Int(cond) => {
                eval::partial_select_int(cond, if_true.constant(), if_false.constant())
            }
        });

        if let Some(res) = selected {
            Source::Const(res)
        } else {
            self.instruction(|dest| Instruction::Select { dest, if_true, if_false, cond })
        }
    }

    pub fn mem_mask(&mut self, address: Source) -> Source {
        let memory_size = Source::Const(Constant::i32(self.memory_size - 1));

        self.and(address, memory_size)
    }

    pub fn fence(&mut self) {
        self.instructions.push(Instruction::Fence)
    }

    #[must_use]
    pub fn ret_with_code(mut self, addr: Source, code: Source) -> Block {
        for (idx, src) in self.registers.iter().enumerate().skip(1) {
            let dest = register::RiscV::with_u8(idx as u8).unwrap();

            if let Some(src) = *src {
                if (self.registers_written >> dest.get()) & 1 == 1 {
                    self.instructions.push(Instruction::WriteReg {
                        dest,
                        base: self.register_arg.expect("Register arg wasn't initialized?"),
                        src,
                    });
                }
            }
        }

        Block {
            allocator: self.id_allocator,
            instructions: self.instructions,
            terminator: Terminator::Ret { addr, code },
        }
    }

    #[must_use]
    pub fn ret_with_addr(self, addr: Source) -> Block {
        self.ret_with_code(addr, Source::Const(Constant::i32(0)))
    }

    #[must_use]
    pub fn ret(self) -> Block {
        let pc = self.pc;
        self.ret_with_addr(pc)
    }
}

pub fn non_terminal(ctx: &mut Context, instruction: instruction::Instruction, len: u32) {
    match instruction {
        instruction::Instruction::J(_)
        | instruction::Instruction::Sys(_)
        | instruction::Instruction::B(_)
        | instruction::Instruction::IJump(_) => panic!("Instruction was a terminal"),

        instruction::Instruction::IMem(instruction::IMem { opcode, imm, rs1, rd }) => {
            match opcode {
                // todo: finer grained fences.
                opcode::IMem::FENCE => ctx.fence(),
                opcode::IMem::LD(width) => {
                    let imm = imm as i16 as u32;
                    let imm = Source::Const(Constant::i32(imm));

                    let src = ctx.load_register(rs1);
                    let offset = ctx.add(src, imm);
                    let offset = ctx.mem_mask(offset);

                    let mem = ctx.read_memory(offset, width, true);

                    if let Some(rd) = rd {
                        ctx.write_register(rd, mem);
                    }
                }
                // todo: deduplicate with previous
                opcode::IMem::LDU(width) => {
                    let imm = imm as i16 as u32;
                    let imm = Source::Const(Constant::i32(imm));

                    let src = ctx.load_register(rs1);
                    let offset = ctx.add(src, imm);
                    let offset = ctx.mem_mask(offset);

                    let mem = ctx.read_memory(offset, width, false);

                    if let Some(rd) = rd {
                        ctx.write_register(rd, mem);
                    }
                }
            }
        }

        instruction::Instruction::I(instruction::I { opcode, imm, rs1, rd }) => {
            let imm = imm as i16 as u32;
            let src = ctx.load_register(rs1);
            let imm = Source::Const(Constant::i32(imm));

            let res = match opcode {
                opcode::I::ADDI => ctx.add(src, imm),
                opcode::I::XORI => ctx.xor(src, imm),
                opcode::I::ANDI => ctx.and(src, imm),
                opcode::I::SLLI => ctx.sll(src, imm),
                opcode::I::SRLI => ctx.srl(src, imm),
                opcode::I::SRAI => ctx.sra(src, imm),
                opcode::I::ORI => ctx.or(src, imm),
                opcode::I::SICond(cmp_mode) => ctx.cmp(src, imm, cmp_mode.into()),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }

        instruction::Instruction::R(instruction::R { rs1, rs2, rd, opcode }) => {
            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            #[allow(clippy::match_same_arms)]
            let res = match opcode {
                opcode::R::ADD => ctx.add(src1, src2),
                opcode::R::SUB => ctx.sub(src1, src2),
                opcode::R::SLL => ctx.sll(src1, src2),
                opcode::R::SCond(cmp) => ctx.cmp(src1, src2, cmp.into()),
                opcode::R::XOR => ctx.xor(src1, src2),
                opcode::R::SRL => ctx.srl(src1, src2),
                opcode::R::SRA => ctx.sra(src1, src2),
                opcode::R::OR => ctx.or(src1, src2),
                opcode::R::AND => ctx.and(src1, src2),
                opcode::R::MUL => todo!(),
                opcode::R::MULH => todo!(),
                opcode::R::MULHSU => todo!(),
                opcode::R::MULHU => todo!(),
                opcode::R::DIV => todo!(),
                opcode::R::DIVU => todo!(),
                opcode::R::REM => todo!(),
                opcode::R::REMU => todo!(),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }
        instruction::Instruction::S(instruction::S { width, rs1, rs2, imm }) => {
            let imm = imm as i16 as u32;
            let imm = Source::Const(Constant::i32(imm));

            let src1 = ctx.load_register(rs1);
            let addr = ctx.add(src1, imm);
            let addr = ctx.mem_mask(addr);

            let src2 = ctx.load_register(rs2);
            ctx.write_memory(addr, src2, width);
        }
        instruction::Instruction::U(instruction::U { opcode, imm, rd }) => {
            // ensure that the immediate only uses the upper 20 bits.
            let imm = imm & 0xffff_f000;
            let imm = Source::Const(Constant::i32(imm));

            let res = match opcode {
                opcode::U::LUI => imm,
                opcode::U::AUIPC => ctx.add(ctx.pc, imm),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }
    }

    ctx.add_pc(Source::Const(Constant::i32(len)));
}

#[must_use]
pub fn terminal(mut ctx: Context, instruction: instruction::Instruction, len: u32) -> Block {
    let len = Source::Const(Constant::i32(len));

    match instruction {
        instruction::Instruction::J(instruction::J { opcode: opcode::J::JAL, rd, imm }) => {
            // load pc + len into rd (the link register)
            if let Some(rd) = rd {
                // we specifically need to avoid modifying `pc`, since we need the
                // old value for adding the immediate.

                let next_pc = ctx.add(ctx.pc, len);

                ctx.write_register(rd, next_pc);
            }

            let imm = Source::Const(Constant::i32(imm));

            ctx.add_pc(imm);

            ctx.ret()
        }

        instruction::Instruction::IJump(instruction::IJump {
            opcode: opcode::IJump::JALR,
            rd,
            imm,
            rs1,
        }) => {
            let imm = imm as i16 as u32;

            // read the source before writing to the link register since they can be the same.
            let src = rs1.map(|it| ctx.read_register(it));

            // load pc + len into rd (the link register)
            if let Some(rd) = rd {
                let next_pc = ctx.add_pc(len);

                ctx.write_register(rd, next_pc);
            }

            // adding 0 is the same as not adding, so don't bother with a match here.
            if let Some(mut src) = src {
                if imm != 0 {
                    let imm = Source::Const(Constant::i32(imm));
                    src = ctx.add(src, imm);
                }

                src = ctx.and(src, Source::Const(Constant::i32(!1)));
                ctx.override_pc(src);
            } else {
                let imm = Source::Const(Constant::i32(imm));
                let imm = ctx.and(imm, Source::Const(Constant::i32(!1)));
                ctx.override_pc(imm);
            }

            ctx.ret()
        }

        instruction::Instruction::Sys(instruction::Sys { opcode }) => {
            use ribit_core::ReturnCode;
            let return_code = match opcode {
                opcode::RSys::EBREAK => ReturnCode::EBreak,
                opcode::RSys::ECALL => ReturnCode::ECall,
            } as u32;

            let return_code = Source::Const(Constant::i32(return_code));

            let pc = ctx.add_pc(len);

            ctx.ret_with_code(pc, return_code)
        }

        instruction::Instruction::B(instruction::B { imm, rs1, rs2, cmp_mode }) => {
            let imm = Source::Const(Constant::i32(imm as i16 as u32));

            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            let cmp = ctx.cmp(src1, src2, cmp_mode.into());

            let continue_pc = ctx.add(ctx.pc, len);

            let jump_pc = ctx.add(ctx.pc, imm);

            let addr = ctx.select(cmp, jump_pc, continue_pc);

            ctx.ret_with_addr(addr)
        }

        _ => panic!("Instruction wasn't a terminal"),
    }
}
