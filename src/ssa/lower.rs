use std::mem;

use super::{CmpKind, Id, Instruction, InstructionId, Source};

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
        let pc = mem::replace(&mut self.pc, id);

        self.instructions.push(Instruction::Add {
            dest: id,
            src1: src,
            src2: Source::Id(pc),
        });

        pc
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

    pub fn read_register(&mut self, reg: register::RiscV) -> Id {
        let id = self.new_id();

        self.instructions
            .push(Instruction::ReadReg { dest: id, src: reg });

        id
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

    pub fn and(&mut self, src1: Source, src2: Source) -> Id {
        self.instr_with_id(|dest| Instruction::And { dest, src1, src2 })
    }

    pub fn add(&mut self, src1: Source, src2: Source) -> Id {
        self.instr_with_id(|dest| Instruction::Add { dest, src1, src2 })
    }

    pub fn cmp(&mut self, src1: Source, src2: Source, mode: CmpKind) -> Id {
        self.instr_with_id(|dest| Instruction::Cmp {
            dest,
            src1,
            src2,
            kind: mode,
        })
    }

    pub fn select(&mut self, cond: Source, val1: Source, val2: Source) -> Id {
        self.instr_with_id(|dest| Instruction::Select {
            dest,
            val1,
            val2,
            cond,
        })
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

        instruction::Instruction::I(instruction::I {
            opcode: opcode::I::JALR,
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
            let src1 = if let Some(src) = rs1 {
                ctx.read_register(src)
            } else {
                ctx.load_const(0)
            };

            // todo: dedup this

            let src2 = if let Some(src) = rs2 {
                ctx.read_register(src)
            } else {
                ctx.load_const(0)
            };

            let cmp = ctx.cmp(
                Source::Id(src1),
                Source::Id(src2),
                match cmp_mode {
                    opcode::Cmp::Eq => CmpKind::Eq,
                    opcode::Cmp::Ne => CmpKind::Ne,
                    opcode::Cmp::Lt => CmpKind::Sl,
                    opcode::Cmp::Ltu => CmpKind::Ul,
                    opcode::Cmp::Ge => CmpKind::Sge,
                    opcode::Cmp::Geu => CmpKind::Uge,
                },
            );

            let current_pc = Source::Id(ctx.pc);

            let continue_pc = ctx.add(current_pc, Source::Val(len));

            let jump_pc = ctx.add(current_pc, Source::Val(imm as i16 as u32));

            let next_pc = ctx.select(Source::Id(cmp), Source::Id(jump_pc), Source::Id(continue_pc));

            ctx.pc = next_pc;

            ctx.ret()
        }

        _ => panic!("Instruction wasn't a terminal"),
    }
}

#[cfg(test)]
mod test {
    use super::Context;
    use crate::{instruction, opcode, register};

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

        for instr in instrs {
            println!("{}", instr);
        }

        panic!()
    }
}
