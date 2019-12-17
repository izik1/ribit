use std::mem;

use super::{BinOp, CmpKind, Id, Instruction, InstructionId, Source};

use crate::{instruction, opcode, register};

pub struct Context {
    next_id: Id,
    pub pc: Id,
    instructions: Vec<Instruction>,
}

impl Context {
    const fn initial_pc(pc: u32) -> Instruction {
        let instr = Instruction::LoadConst {
            dest: Id(0),
            src: pc,
        };

        instr
    }

    pub fn new(start_pc: u32) -> Self {
        let pc = Self::initial_pc(start_pc);

        Self {
            next_id: Id(1),
            pc: Id(0),
            instructions: vec![pc],
        }
    }

    pub fn add_pc(&mut self, src: Source) -> Id {
        let id = self.new_id();

        self.instructions.push(Instruction::BinOp {
            op: BinOp::Add,
            dest: id,
            src1: src,
            src2: Source::Id(mem::replace(&mut self.pc, id)),
        });

        id
    }

    fn new_id(&mut self) -> Id {
        let id_num = self.next_id.0;
        std::mem::replace(&mut self.next_id, Id(id_num + 1))
    }

    fn instr_with_id<F: FnOnce(Id) -> Instruction>(&mut self, f: F) -> Id {
        let id = self.new_id();
        self.instructions.push(f(id));
        id
    }

    pub fn binop(&mut self, op: BinOp, src1: Source, src2: Source) -> Id {
        self.instr_with_id(|dest| Instruction::BinOp {
            dest,
            src1,
            src2,
            op,
        })
    }

    pub fn read_register(&mut self, reg: register::RiscV) -> Id {
        let id = self.new_id();

        self.instructions
            .push(Instruction::ReadReg { dest: id, src: reg });

        id
    }

    pub fn load_register(&mut self, reg: Option<register::RiscV>) -> Id {
        if let Some(reg) = reg {
            self.read_register(reg)
        } else {
            self.load_const(0)
        }
    }

    pub fn write_register(&mut self, reg: register::RiscV, val: Source) -> InstructionId {
        self.instructions.push(Instruction::WriteReg {
            dest: reg,
            src: val,
        });

        InstructionId(self.instructions.len() - 1)
    }

    pub fn load_const(&mut self, const_val: u32) -> Id {
        self.instr_with_id(|dest| Instruction::LoadConst {
            dest,
            src: const_val,
        })
    }

    pub fn or(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Or, src1, src2)
    }

    pub fn sll(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Sll, src1, src2)
    }

    pub fn srl(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Srl, src1, src2)
    }

    pub fn sra(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Sra, src1, src2)
    }

    pub fn xor(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Xor, src1, src2)
    }

    pub fn and(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::And, src1, src2)
    }

    pub fn add(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Add, src1, src2)
    }

    pub fn sub(&mut self, src1: Source, src2: Source) -> Id {
        self.binop(BinOp::Sub, src1, src2)
    }

    pub fn cmp(&mut self, src1: Source, src2: Source, mode: CmpKind) -> Id {
        self.instr_with_id(|dest| Instruction::Cmp {
            dest,
            src1,
            src2,
            kind: mode,
        })
    }

    pub fn select(&mut self, cond: Source, if_true: Source, if_false: Source) -> Id {
        self.instr_with_id(|dest| Instruction::Select {
            dest,
            if_true,
            if_false,
            cond,
        })
    }

    pub fn fence(&mut self) {
        self.instructions.push(Instruction::Fence)
    }

    pub fn ret_with_code(mut self, addr: Source, code: Source) -> Vec<Instruction> {
        self.instructions.push(Instruction::Ret { addr, code });

        // todo: assert well formedness here.
        self.instructions
    }

    pub fn ret_with_addr(self, addr: Source) -> Vec<Instruction> {
        self.ret_with_code(addr, Source::Val(0))
    }

    pub fn ret(self) -> Vec<Instruction> {
        let pc = self.pc;
        self.ret_with_addr(Source::Id(pc))
    }
}

impl std::ops::Index<InstructionId> for Context {
    type Output = Instruction;

    fn index(&self, idx: InstructionId) -> &Self::Output {
        &self.instructions[idx.0]
    }
}

pub fn lower_non_terminal(ctx: &mut Context, instr: instruction::Info) {
    let instruction::Info {
        instruction,
        start_address: _start_address,
        len,
    } = instr;

    match instruction {
        instruction::Instruction::J(_)
        | instruction::Instruction::Sys(_)
        | instruction::Instruction::B(_)
        | instruction::Instruction::IJump(_) => panic!("Instruction was a terminal"),

        instruction::Instruction::IMem(instruction::IMem {
            opcode,
            imm: _imm,
            rs1: _rs1,
            rd: _rd,
        }) => {
            match opcode {
                // todo: finer grained fences.
                opcode::IMem::FENCE => {
                    ctx.fence();
                    ctx.add_pc(Source::Val(len));
                }
                opcode::IMem::LD(_) => todo!(),
                opcode::IMem::LDU(_) => todo!(),
            }
        }

        instruction::Instruction::I(instruction::I {
            opcode,
            imm,
            rs1,
            rd,
        }) => {
            let imm = imm as i16 as u32;
            let src = ctx.load_register(rs1);

            let res = match opcode {
                opcode::I::ADDI => ctx.add(Source::Id(src), Source::Val(imm)),
                opcode::I::SICond(cmp_mode) => {
                    ctx.cmp(Source::Id(src), Source::Val(imm), cmp_mode.into())
                }
                opcode::I::XORI => ctx.xor(Source::Id(src), Source::Val(imm)),
                opcode::I::ORI => ctx.or(Source::Id(src), Source::Val(imm)),
                opcode::I::ANDI => ctx.and(Source::Id(src), Source::Val(imm)),
                opcode::I::SLLI => ctx.sll(Source::Id(src), Source::Val(imm)),
                opcode::I::SRLI => ctx.srl(Source::Id(src), Source::Val(imm)),
                opcode::I::SRAI => ctx.sra(Source::Id(src), Source::Val(imm)),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, Source::Id(res));
            }

            ctx.add_pc(Source::Val(len));
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
                opcode::R::ADD => ctx.add(Source::Id(src1), Source::Id(src2)),
                opcode::R::SUB => ctx.sub(Source::Id(src1), Source::Id(src2)),
                opcode::R::SLL => ctx.sll(Source::Id(src1), Source::Id(src2)),
                opcode::R::SCond(cmp) => ctx.cmp(Source::Id(src1), Source::Id(src2), cmp.into()),
                opcode::R::XOR => ctx.xor(Source::Id(src1), Source::Id(src2)),
                opcode::R::SRL => ctx.srl(Source::Id(src1), Source::Id(src2)),
                opcode::R::SRA => ctx.sra(Source::Id(src1), Source::Id(src2)),
                opcode::R::OR => ctx.or(Source::Id(src1), Source::Id(src2)),
                opcode::R::AND => ctx.and(Source::Id(src1), Source::Id(src2)),
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
                ctx.write_register(rd, Source::Id(res));
            }

            ctx.add_pc(Source::Val(len));
        }
        instruction::Instruction::S(_) => todo!(),
        instruction::Instruction::U(instruction::U { opcode, imm, rd }) => {
            // ensure that the immediate only uses the upper 20 bits.
            let imm = imm & 0xffff_f000;

            let res = match opcode {
                opcode::U::LUI => ctx.load_const(imm),
                opcode::U::AUIPC => ctx.add(Source::Id(ctx.pc), Source::Val(imm)),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, Source::Id(res));
            }

            ctx.add_pc(Source::Val(len));
            todo!()
        }
    }
}

pub fn lower_terminal(mut ctx: Context, instr: instruction::Info) -> Vec<self::Instruction> {
    let instruction::Info {
        instruction,
        start_address: _start_address,
        len,
    } = instr;

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
                let next_pc = ctx.add(Source::Id(ctx.pc), Source::Val(len));

                ctx.write_register(rd, Source::Id(next_pc));
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
                let next_pc = ctx.add(Source::Id(ctx.pc), Source::Val(len));

                ctx.write_register(rd, Source::Id(next_pc));
            }

            ctx.add_pc(Source::Val(imm));

            // adding 0 is the same as not adding, so don't bother with a match here.
            if let Some(src) = rs1 {
                let src = ctx.read_register(src);
                ctx.add_pc(Source::Id(src));
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

            ctx.ret_with_code(Source::Id(pc), Source::Val(return_code))
        }

        instruction::Instruction::B(instruction::B {
            imm,
            rs1,
            rs2,
            cmp_mode,
        }) => {
            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            let cmp = ctx.cmp(Source::Id(src1), Source::Id(src2), cmp_mode.into());

            let current_pc = Source::Id(ctx.pc);

            let continue_pc = ctx.add(current_pc, Source::Val(len));

            let jump_pc = ctx.add(current_pc, Source::Val(imm as i16 as u32));

            ctx.pc = ctx.select(
                Source::Id(cmp),
                Source::Id(jump_pc),
                Source::Id(continue_pc),
            );

            ctx.ret()
        }

        _ => panic!("Instruction wasn't a terminal"),
    }
}

#[cfg(test)]
mod test {
    use super::Context;
    use crate::ssa::lower::lower_non_terminal;
    use crate::{instruction, opcode, register};

    fn cmp_instrs(expected: &[&str], actual: &[super::Instruction]) {
        for (idx, instr) in actual.iter().enumerate() {
            assert_eq!(expected[idx], format!("{}", instr));
            println!("{}", instr);
        }
    }

    #[test]
    fn jal_basic() {
        let ctx = Context::new(0);

        let instrs = super::lower_terminal(
            ctx,
            instruction::Info::new(
                instruction::Instruction::J(instruction::J {
                    imm: 4096,
                    rd: Some(register::RiscV::X4),
                    opcode: opcode::J::JAL,
                }),
                0,
                4,
            ),
        );

        cmp_instrs(
            &[
                "%0 = 0",
                "%1 = add %0, 4",
                "x4 = %1",
                "%2 = add 4096, %0",
                "ret 0, %2",
            ],
            &instrs,
        );
    }

    #[test]
    fn sys_break() {
        let ctx = Context::new(0);

        let instrs = super::lower_terminal(
            ctx,
            instruction::Info::new(
                instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
                0,
                4,
            ),
        );

        cmp_instrs(&["%0 = 0", "%1 = add 4, %0", "ret 1, %1"], &instrs);
    }

    #[test]
    fn addi_nop() {
        let mut ctx = Context::new(0);
        lower_non_terminal(
            &mut ctx,
            instruction::Info::new(
                instruction::Instruction::I(instruction::I::new(0, None, None, opcode::I::ADDI)),
                0,
                4,
            ),
        );

        let instrs = super::lower_terminal(
            ctx,
            instruction::Info::new(
                instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
                4,
                4,
            ),
        );

        cmp_instrs(
            &[
                "%0 = 0",
                "%1 = 0",
                "%2 = add %1, 0",
                "%3 = add 4, %0",
                "%4 = add 4, %3",
                "ret 1, %4",
            ],
            &instrs,
        );
    }

    #[test]
    fn addi_no_dest() {
        let mut ctx = Context::new(0);
        lower_non_terminal(
            &mut ctx,
            instruction::Info::new(
                instruction::Instruction::I(instruction::I::new(
                    50,
                    Some(register::RiscV::X1),
                    None,
                    opcode::I::ADDI,
                )),
                0,
                4,
            ),
        );

        let instrs = super::lower_terminal(
            ctx,
            instruction::Info::new(
                instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
                4,
                4,
            ),
        );

        cmp_instrs(
            &[
                "%0 = 0",
                "%1 = x1",
                "%2 = add %1, 50",
                "%3 = add 4, %0",
                "%4 = add 4, %3",
                "ret 1, %4",
            ],
            &instrs,
        );
    }

    #[test]
    fn addi_no_src() {
        let mut ctx = Context::new(0);
        lower_non_terminal(
            &mut ctx,
            instruction::Info::new(
                instruction::Instruction::I(instruction::I::new(
                    50,
                    None,
                    Some(register::RiscV::X2),
                    opcode::I::ADDI,
                )),
                0,
                4,
            ),
        );

        let instrs = super::lower_terminal(
            ctx,
            instruction::Info::new(
                instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
                4,
                4,
            ),
        );

        cmp_instrs(
            &[
                "%0 = 0",
                "%1 = 0",
                "%2 = add %1, 50",
                "x2 = %2",
                "%3 = add 4, %0",
                "%4 = add 4, %3",
                "ret 1, %4",
            ],
            &instrs,
        );
    }
}
