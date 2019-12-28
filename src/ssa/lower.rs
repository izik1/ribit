use super::{BinOp, CmpKind, Id, Instruction, Source};
use crate::ssa::eval;
use crate::{instruction, opcode, register, Width};

pub struct Context {
    next_id: Id,
    pub pc: Source,
    registers: [Option<Source>; 32],
    registers_written: u32,
    instructions: Vec<Instruction>,
}

impl Context {
    pub fn new(start_pc: u32) -> Self {
        Self {
            next_id: Id(0),
            pc: Source::Val(start_pc),
            registers: [None; 32],
            registers_written: 0x0000_0000,
            instructions: vec![],
        }
    }

    pub fn add_pc(&mut self, src: Source) -> Source {
        self.pc = self.add(src, self.pc);
        self.pc
    }

    fn new_id(&mut self) -> Id {
        let id_num = self.next_id.0;

        assert!(id_num < u16::max_value());

        std::mem::replace(&mut self.next_id, Id(id_num + 1))
    }

    fn instruction<F: FnOnce(Id) -> Instruction>(&mut self, f: F) -> Source {
        let id = self.new_id();
        self.instructions.push(f(id));
        Source::Id(id)
    }

    pub fn binop(&mut self, op: BinOp, src1: Source, src2: Source) -> Source {
        if let (Some(src1), Some(src2)) = (src1.val(), src2.val()) {
            Source::Val(eval::binop(src1, src2, op))
        } else {
            self.instruction(|dest| Instruction::BinOp {
                dest,
                src1,
                src2,
                op,
            })
        }
    }

    pub fn read_register(&mut self, reg: register::RiscV) -> Source {
        self.registers[reg.get() as usize].unwrap_or_else(|| {
            let id = self.new_id();

            self.instructions
                .push(Instruction::ReadReg { dest: id, src: reg });

            self.registers[reg.get() as usize] = Some(Source::Id(id));

            Source::Id(id)
        })
    }

    pub fn load_register(&mut self, reg: Option<register::RiscV>) -> Source {
        reg.map_or(Source::Val(0), |reg| self.read_register(reg))
    }

    pub fn read_memory(&mut self, src: Source, width: Width, sign_extend: bool) -> Source {
        self.instruction(|dest| Instruction::ReadMem {
            dest,
            src,
            width,
            sign_extend,
        })
    }

    pub fn write_register(&mut self, reg: register::RiscV, val: Source) {
        self.registers[reg.get() as usize] = Some(val);
        self.registers_written |= 1 << reg.get();
    }

    pub fn write_memory(&mut self, addr: Source, val: Source, width: Width) {
        self.instructions.push(Instruction::WriteMem {
            addr,
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
        if let (Some(src1), Some(src2)) = (src1.val(), src2.val()) {
            Source::Val(eval::cmp(src1, src2, mode))
        } else {
            self.instruction(|dest| Instruction::Cmp {
                dest,
                src1,
                src2,
                kind: mode,
            })
        }
    }

    pub fn select(&mut self, cond: Source, if_true: Source, if_false: Source) -> Source {
        if let Some(res) = eval::try_select(cond.val(), if_true.val(), if_false.val()) {
            Source::Val(res)
        } else {
            self.instruction(|dest| Instruction::Select {
                dest,
                if_true,
                if_false,
                cond,
            })
        }
    }

    pub fn fence(&mut self) {
        self.instructions.push(Instruction::Fence)
    }

    #[must_use]
    pub fn ret_with_code(mut self, addr: Source, code: Source) -> Vec<Instruction> {
        for (idx, src) in self.registers.iter().enumerate().skip(1) {
            let dest = register::RiscV::with_u8(idx as u8).unwrap();

            if let Some(src) = *src {
                if (self.registers_written >> dest.get()) & 1 == 1 {
                    self.instructions.push(Instruction::WriteReg { dest, src });
                }
            }
        }

        self.instructions.push(Instruction::Ret { addr, code });

        // todo: assert well formedness here.
        self.instructions
    }

    #[must_use]
    pub fn ret_with_addr(self, addr: Source) -> Vec<Instruction> {
        self.ret_with_code(addr, Source::Val(0))
    }

    #[must_use]
    pub fn ret(self) -> Vec<Instruction> {
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

        instruction::Instruction::IMem(instruction::IMem {
            opcode,
            imm,
            rs1,
            rd,
        }) => match opcode {
            // todo: finer grained fences.
            opcode::IMem::FENCE => ctx.fence(),
            opcode::IMem::LD(width) => {
                let imm = imm as i16 as u32;

                let src = ctx.load_register(rs1);
                let offset = ctx.add(src, Source::Val(imm));

                let mem = ctx.read_memory(offset, width, true);

                if let Some(rd) = rd {
                    ctx.write_register(rd, mem);
                }
            }
            // todo: deduplicate with previous
            opcode::IMem::LDU(width) => {
                let imm = imm as i16 as u32;

                let src = ctx.load_register(rs1);
                let offset = ctx.add(src, Source::Val(imm));

                let mem = ctx.read_memory(offset, width, false);

                if let Some(rd) = rd {
                    ctx.write_register(rd, mem);
                }
            }
        },

        instruction::Instruction::I(instruction::I {
            opcode,
            imm,
            rs1,
            rd,
        }) => {
            let imm = imm as i16 as u32;
            let src = ctx.load_register(rs1);

            let res = match opcode {
                opcode::I::ADDI => ctx.add(src, Source::Val(imm)),
                opcode::I::XORI => ctx.xor(src, Source::Val(imm)),
                opcode::I::ANDI => ctx.and(src, Source::Val(imm)),
                opcode::I::SLLI => ctx.sll(src, Source::Val(imm)),
                opcode::I::SRLI => ctx.srl(src, Source::Val(imm)),
                opcode::I::SRAI => ctx.sra(src, Source::Val(imm)),
                opcode::I::ORI => ctx.or(src, Source::Val(imm)),
                opcode::I::SICond(cmp_mode) => ctx.cmp(src, Source::Val(imm), cmp_mode.into()),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }

        instruction::Instruction::R(instruction::R {
            rs1,
            rs2,
            rd,
            opcode,
        }) => {
            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

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
        instruction::Instruction::S(instruction::S {
            width,
            rs1,
            rs2,
            imm,
        }) => {
            let imm = imm as i16 as u32;

            let src1 = ctx.load_register(rs1);
            let addr = ctx.add(src1, Source::Val(imm));

            let src2 = ctx.load_register(rs2);
            ctx.write_memory(addr, src2, width);
        }
        instruction::Instruction::U(instruction::U { opcode, imm, rd }) => {
            // ensure that the immediate only uses the upper 20 bits.
            let imm = imm & 0xffff_f000;

            let res = match opcode {
                opcode::U::LUI => Source::Val(imm),
                opcode::U::AUIPC => ctx.add(ctx.pc, Source::Val(imm)),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }
    }

    ctx.add_pc(Source::Val(len));
}

#[must_use]
pub fn terminal(
    mut ctx: Context,
    instruction: instruction::Instruction,
    len: u32,
) -> Vec<self::Instruction> {
    match instruction {
        instruction::Instruction::J(instruction::J {
            opcode: opcode::J::JAL,
            rd,
            imm,
        }) => {
            // load pc + len into rd (the link register)
            if let Some(rd) = rd {
                // we specifically need to avoid modifying `pc`, since we need the
                // old value for adding the immediate.
                let next_pc = ctx.add(ctx.pc, Source::Val(len));

                ctx.write_register(rd, next_pc);
            }

            ctx.add_pc(Source::Val(imm));

            ctx.ret()
        }

        instruction::Instruction::IJump(instruction::IJump {
            opcode: opcode::IJump::JALR,
            rd,
            imm,
            rs1,
        }) => {
            let imm = imm as i16 as u32;

            // load pc + len into rd (the link register)
            if let Some(rd) = rd {
                // we specifically need to avoid modifying `pc`, since we need the
                // old value for adding the immediate.
                let next_pc = ctx.add(ctx.pc, Source::Val(len));

                ctx.write_register(rd, next_pc);
            }

            ctx.add_pc(Source::Val(imm));

            // adding 0 is the same as not adding, so don't bother with a match here.
            if let Some(src) = rs1 {
                let src = ctx.read_register(src);
                ctx.add_pc(src);
            }

            ctx.ret()
        }

        instruction::Instruction::Sys(instruction::Sys { opcode }) => {
            use crate::ReturnCode;
            let return_code = match opcode {
                opcode::RSys::EBREAK => ReturnCode::EBreak,
                opcode::RSys::ECALL => ReturnCode::ECall,
            } as u32;

            let pc = ctx.add_pc(Source::Val(len));

            ctx.ret_with_code(pc, Source::Val(return_code))
        }

        instruction::Instruction::B(instruction::B {
            imm,
            rs1,
            rs2,
            cmp_mode,
        }) => {
            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            let cmp = ctx.cmp(src1, src2, cmp_mode.into());

            let continue_pc = ctx.add(ctx.pc, Source::Val(len));

            let jump_pc = ctx.add(ctx.pc, Source::Val(imm as i16 as u32));

            let addr = ctx.select(cmp, jump_pc, continue_pc);

            ctx.ret_with_addr(addr)
        }

        _ => panic!("Instruction wasn't a terminal"),
    }
}

#[cfg(test)]
mod test {
    use super::Context;
    use crate::DisplayDeferSlice;
    use crate::{instruction, opcode, register};

    use insta::{assert_display_snapshot, assert_ron_snapshot};

    #[test]
    fn jal_basic() {
        let ctx = Context::new(0);

        let instrs = super::terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn sys_break() {
        let ctx = Context::new(0);

        let instrs = super::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn addi_nop() {
        let mut ctx = Context::new(0);
        super::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(0, None, None, opcode::I::ADDI)),
            4,
        );

        let instrs = super::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn branch_0_0_eq() {
        let ctx = Context::new(0);
        let instrs = super::terminal(
            ctx,
            instruction::Instruction::B(instruction::B::new(1024, None, None, opcode::Cmp::Eq)),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn branch_0_x1_eq() {
        let ctx = Context::new(0);
        let instrs = super::terminal(
            ctx,
            instruction::Instruction::B(instruction::B::new(
                1024,
                None,
                Some(register::RiscV::X1),
                opcode::Cmp::Eq,
            )),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn addi_no_dest() {
        let mut ctx = Context::new(0);
        super::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                50,
                Some(register::RiscV::X1),
                None,
                opcode::I::ADDI,
            )),
            4,
        );

        let instrs = super::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn addi_no_src() {
        let mut ctx = Context::new(0);
        super::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                50,
                None,
                Some(register::RiscV::X2),
                opcode::I::ADDI,
            )),
            4,
        );

        let instrs = super::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
        );

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }
}
