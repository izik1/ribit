#[cfg(test)]
mod tests;

use ribit_core::{ReturnCode, Width, instruction, opcode, register};

use super::{AnySource, CmpKind, Id, Instruction, ShiftOp};
use crate::instruction::{CmpArgs, CommutativeBinArgs, ExtInt, Select};
use crate::reference::Reference;
use crate::ty::{self, ConstTy, Constant};
use crate::{Arg, Block, CommutativeBinOp, IdAllocator, Ref, Source, SourcePair, Terminator, eval};

pub struct Context {
    id_allocator: IdAllocator,
    pub pc: Source<ty::I32>,
    registers: [Option<AnySource>; 32],
    register_arg: Source<ty::I32>,
    memory_arg: Source<ty::I32>,
    registers_written: u32,
    instructions: Vec<Instruction>,
    memory_size: u32,
}

/// try to use associativity.
///
/// this optimizes things like the following:
/// ```text
/// %2 = %1 + 1
/// %3 = %2 + 1
/// %4 = %3 + 0xffff_fffd
/// ```
/// to
/// ```text
/// %4 = %1 + 0
/// ```
/// which we can then munch away via identity.
fn try_associate(
    instructions: &[Instruction],
    op: CommutativeBinOp,
    src1: Reference,
    src2: Constant,
) -> (Reference, Constant) {
    if !op.is_associative() {
        return (src1, src2);
    }

    let instr = instructions.iter().rfind(|it| it.id() == Some(src1.id)).unwrap();
    let inner = match instr {
        Instruction::CommutativeBinOp { dest: _, args } if args.op == op => (args.src1, args.src2),
        _ => return (src1, src2),
    };

    let src2 = match inner.1 {
        AnySource::Const(inner_src2) => eval::commutative_binop(inner_src2, src2, op),
        _ => return (src1, src2),
    };

    let src1 = inner.0;

    (src1, src2)
}

impl Context {
    #[must_use]
    pub fn new(start_pc: u32, memory_size: u32) -> Self {
        const REG_ID: Id = Id(0);
        const MEM_ID: Id = Id(REG_ID.0 + 1);

        assert!(memory_size.is_power_of_two());

        Self {
            id_allocator: const { IdAllocator::with_start(Id(MEM_ID.0 + 1)) },
            pc: Source::Const(start_pc),
            registers: [None; 32],
            register_arg: Source::Ref(REG_ID),
            memory_arg: Source::Ref(MEM_ID),
            registers_written: 0x0000_0000,
            instructions: Vec::from([
                Instruction::Arg { dest: REG_ID, src: Arg::Register },
                Instruction::Arg { dest: MEM_ID, src: Arg::Memory },
            ]),
            memory_size,
        }
    }

    pub fn add_pc(&mut self, src: AnySource) -> Source<ty::I32> {
        self.pc = self.add(src, self.pc.upcast()).downcast().unwrap();
        self.pc
    }

    pub fn override_pc(&mut self, src: Source<ty::I32>) {
        self.pc = src;
    }

    fn instruction<F: FnOnce(Id) -> Instruction>(&mut self, f: F) -> AnySource {
        let id = self.id_allocator.allocate();

        let instr = f(id);
        let ty = instr.ty();

        self.instructions.push(instr);
        AnySource::Ref(Reference { ty, id })
    }

    fn typed_instruction<T: ConstTy, F: FnOnce(Id) -> Instruction>(&mut self, f: F) -> Source<T> {
        let id = self.id_allocator.allocate();

        let instr = f(id);
        assert_eq!(instr.ty(), T::TY);

        self.instructions.push(instr);
        Source::Ref(id)
    }

    pub fn commutative_binop(
        &mut self,
        op: CommutativeBinOp,
        src1: AnySource,
        src2: AnySource,
    ) -> AnySource {
        let args = CommutativeBinArgs::new_assoc(op, src1, src2, |op, r, c| {
            try_associate(&self.instructions, op, r, c)
        });

        match args {
            Ok(args) => self.instruction(|dest| Instruction::CommutativeBinOp { dest, args }),
            Err(res) => res,
        }
    }

    pub fn shift(&mut self, op: ShiftOp, src1: AnySource, src2: AnySource) -> AnySource {
        assert_eq!(src1.ty(), src2.ty());

        // shift identity.
        if matches!(src2, AnySource::Const(Constant::Int(i)) if i.unsigned() == 0) {
            return src1;
        }

        // todo: some kind of shift folding, up until the total folded shift turns into an absorb.
        // doesn't work for sra without knowing the MSB, but it does work for sll, srl.

        let consts: (Constant, Constant) = match SourcePair::try_from((src1, src2)) {
            Ok(src) => return self.instruction(|dest| Instruction::ShiftOp { dest, src, op }),
            Err(consts) => consts,
        };

        AnySource::Const(eval::shift(consts.0, consts.1, op))
    }

    pub fn read_register(&mut self, reg: register::RiscV) -> AnySource {
        self.registers[reg.get() as usize].unwrap_or_else(|| {
            let id = self.id_allocator.allocate();
            let instr = Instruction::ReadReg { dest: id, base: self.register_arg, src: reg };

            let ty = instr.ty();

            self.instructions.push(instr);

            let r = AnySource::Ref(Reference { ty, id });

            self.registers[reg.get() as usize] = Some(r);

            r
        })
    }

    pub fn load_register(&mut self, reg: Option<register::RiscV>) -> AnySource {
        reg.map_or(AnySource::Const(Constant::i32(0)), |reg| self.read_register(reg))
    }

    pub fn read_memory(&mut self, src: AnySource, width: Width, sign_extend: bool) -> AnySource {
        let base = self.memory_arg;
        self.instruction(|dest| Instruction::ReadMem { dest, src, base, width, sign_extend })
    }

    pub fn write_register(&mut self, reg: register::RiscV, val: AnySource) {
        let stored = &mut self.registers[reg.get() as usize];

        // don't write a value that's already there.
        // this can happen due to non cannonical NOPs (aka, certain HINTs),
        // or due to writing the same value twice, doesn't really matter how it happens.
        // not sure how _common_ it is, unfortunately.
        if stored.as_ref() == Some(&val) {
            return;
        }

        *stored = Some(val);
        self.registers_written |= 1 << reg.get();
    }

    pub fn write_memory(&mut self, addr: Source<ty::I32>, val: AnySource, width: Width) {
        self.instructions.push(Instruction::WriteMem {
            addr,
            base: self.memory_arg,
            src: val,
            width,
        });
    }

    pub fn or(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.commutative_binop(CommutativeBinOp::Or, src1, src2)
    }

    pub fn sll(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.shift(ShiftOp::Sll, src1, src2)
    }

    pub fn srl(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.shift(ShiftOp::Srl, src1, src2)
    }

    pub fn sra(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.shift(ShiftOp::Sra, src1, src2)
    }

    pub fn xor(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.commutative_binop(CommutativeBinOp::Xor, src1, src2)
    }

    pub fn and(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.commutative_binop(CommutativeBinOp::And, src1, src2)
    }

    pub fn add(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        self.commutative_binop(CommutativeBinOp::Add, src1, src2)
    }

    pub fn sub(&mut self, src1: AnySource, src2: AnySource) -> AnySource {
        let src2 = match src2 {
            AnySource::Const(c) => return self.add(src1, AnySource::Const(eval::neg(c))),
            AnySource::Ref(src2) => src2,
        };

        self.instruction(|dest| Instruction::Sub { dest, src1, src2 })
    }

    pub fn cmp(&mut self, src1: AnySource, src2: AnySource, kind: CmpKind) -> Source<ty::Bool> {
        assert_eq!(src1.ty(), src2.ty());

        match CmpArgs::new(src1, src2, kind) {
            Ok(args) => self.typed_instruction(|dest| Instruction::Cmp { dest, args }),
            Err(c) => Source::Const(c),
        }
    }

    pub fn int_extend(&mut self, src: AnySource, width: Width, signed: bool) -> AnySource {
        match src {
            AnySource::Const(src) => {
                AnySource::Const(Constant::Int(eval::extend_int(width, src, signed)))
            }
            AnySource::Ref(src) => {
                self.instruction(|dest| Instruction::ExtInt(ExtInt { dest, width, src, signed }))
            }
        }
    }

    pub fn select(
        &mut self,
        cond: Source<ty::Bool>,
        if_true: AnySource,
        if_false: AnySource,
    ) -> AnySource {
        match cond {
            Source::Const(false) => if_false,
            Source::Const(true) => if_true,
            Source::Ref(id) => self.instruction(|dest| {
                Instruction::Select(Select { dest, if_true, if_false, cond: Ref::new(id) })
            }),
        }
    }

    pub fn mem_mask(&mut self, address: AnySource) -> AnySource {
        let memory_size = AnySource::Const(Constant::i32(self.memory_size - 1));

        self.and(address, memory_size)
    }

    pub fn fence(&mut self) {
        self.instructions.push(Instruction::Fence);
    }

    #[must_use]
    pub fn ret_with_code(mut self, addr: Source<ty::I32>, code: ReturnCode) -> Block {
        for (idx, src) in self.registers.iter().enumerate().skip(1) {
            let dest = register::RiscV::with_u8(idx as u8).unwrap();

            if let Some(src) = *src
                && (self.registers_written >> dest.get()) & 1 == 1
            {
                self.instructions.push(Instruction::WriteReg {
                    dest,
                    base: self.register_arg,
                    src,
                });
            }
        }

        Block {
            allocator: self.id_allocator,
            instructions: self.instructions,
            terminator: Terminator::Ret { addr, code },
        }
    }

    #[must_use]
    pub fn ret_with_addr(self, addr: Source<ty::I32>) -> Block {
        self.ret_with_code(addr, ReturnCode::Normal)
    }

    #[must_use]
    pub fn ret(self) -> Block {
        let pc = self.pc;
        self.ret_with_addr(pc)
    }
}

#[allow(clippy::needless_pass_by_value)]
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
                    let imm = AnySource::Const(Constant::i32(imm));

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
                    let imm = AnySource::Const(Constant::i32(imm));

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

        // ignore nops to save the need to eliminate them.
        instruction::Instruction::I(instruction::I { rd: None, .. }) => {}
        instruction::Instruction::I(instruction::I { opcode, imm, rs1, rd: Some(rd) }) => {
            let imm = imm as i16 as u32;
            let src = ctx.load_register(rs1);
            let imm = AnySource::Const(Constant::i32(imm));

            let res = match opcode {
                opcode::I::ADDI => ctx.add(src, imm),
                opcode::I::XORI => ctx.xor(src, imm),
                opcode::I::ANDI => ctx.and(src, imm),
                opcode::I::SLLI => ctx.sll(src, imm),
                opcode::I::SRLI => ctx.srl(src, imm),
                opcode::I::SRAI => ctx.sra(src, imm),
                opcode::I::ORI => ctx.or(src, imm),
                opcode::I::SICond(cmp) => {
                    let tmp = ctx.cmp(src, imm, cmp.into()).upcast();
                    ctx.int_extend(tmp, Width::DWord, false)
                }
            };

            ctx.write_register(rd, res);
        }

        instruction::Instruction::R(instruction::R {
            opcode:
                opcode::R::MUL
                | opcode::R::MULH
                | opcode::R::MULHSU
                | opcode::R::MULHU
                | opcode::R::DIV
                | opcode::R::DIVU
                | opcode::R::REM
                | opcode::R::REMU,
            ..
        }) => {
            todo!("M-extension instructions have no JIT support")
        }
        // same as with I-type, ignore nops to avoid needing to get rid of them.
        instruction::Instruction::R(instruction::R { rd: None, .. }) => {}
        instruction::Instruction::R(instruction::R { rs1, rs2, rd: Some(rd), opcode }) => {
            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            let res = match opcode {
                opcode::R::ADD => ctx.add(src1, src2),
                opcode::R::SUB => ctx.sub(src1, src2),
                opcode::R::SLL => ctx.sll(src1, src2),
                opcode::R::SCond(cmp) => {
                    let tmp = ctx.cmp(src1, src2, cmp.into()).upcast();
                    ctx.int_extend(tmp, Width::DWord, false)
                }
                opcode::R::XOR => ctx.xor(src1, src2),
                opcode::R::SRL => ctx.srl(src1, src2),
                opcode::R::SRA => ctx.sra(src1, src2),
                opcode::R::OR => ctx.or(src1, src2),
                opcode::R::AND => ctx.and(src1, src2),
                opcode::R::MUL
                | opcode::R::MULH
                | opcode::R::MULHSU
                | opcode::R::MULHU
                | opcode::R::DIV
                | opcode::R::DIVU
                | opcode::R::REM
                | opcode::R::REMU => unreachable!(),
            };

            ctx.write_register(rd, res);
        }
        instruction::Instruction::S(instruction::S { width, rs1, rs2, imm }) => {
            let imm = imm as i16 as u32;
            let imm = AnySource::Const(Constant::i32(imm));

            let src1 = ctx.load_register(rs1);
            let addr = ctx.add(src1, imm);
            let addr = ctx.mem_mask(addr);

            let src2 = ctx.load_register(rs2);
            ctx.write_memory(addr.downcast().unwrap(), src2, width);
        }
        instruction::Instruction::U(instruction::U { opcode, imm, rd }) => {
            // ensure that the immediate only uses the upper 20 bits.
            let imm = imm & 0xffff_f000;
            let imm = AnySource::Const(Constant::i32(imm));

            let res = match opcode {
                opcode::U::LUI => imm,
                opcode::U::AUIPC => ctx.add(ctx.pc.upcast(), imm),
            };

            if let Some(rd) = rd {
                ctx.write_register(rd, res);
            }
        }
    }

    ctx.add_pc(AnySource::Const(Constant::i32(len)));
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn terminal(mut ctx: Context, instruction: instruction::Instruction, len: u32) -> Block {
    let len = AnySource::Const(Constant::i32(len));

    match instruction {
        instruction::Instruction::J(instruction::J { opcode: opcode::J::JAL, rd, imm }) => {
            // load pc + len into rd (the link register)
            if let Some(rd) = rd {
                // we specifically need to avoid modifying `pc`, since we need the
                // old value for adding the immediate.

                let next_pc = ctx.add(ctx.pc.upcast(), len);

                ctx.write_register(rd, next_pc);
            }

            let imm = AnySource::Const(Constant::i32(imm));

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

                ctx.write_register(rd, next_pc.upcast());
            }

            // adding 0 is the same as not adding, so don't bother with a match here.
            if let Some(mut src) = src {
                if imm != 0 {
                    let imm = AnySource::Const(Constant::i32(imm));
                    src = ctx.add(src, imm);
                }

                src = ctx.and(src, AnySource::Const(Constant::i32(!1)));
                ctx.override_pc(src.downcast().unwrap());
            } else {
                let imm = imm & !1;
                ctx.override_pc(Source::Const(imm));
            }

            ctx.ret()
        }

        instruction::Instruction::Sys(instruction::Sys { opcode }) => {
            let return_code = match opcode {
                opcode::RSys::EBREAK => ReturnCode::EBreak,
                opcode::RSys::ECALL => ReturnCode::ECall,
            };

            let pc = ctx.add_pc(len);

            ctx.ret_with_code(pc, return_code)
        }

        instruction::Instruction::B(instruction::B { imm, rs1, rs2, cmp_mode }) => {
            let imm = AnySource::Const(Constant::i32(imm as i16 as u32));

            let src1 = ctx.load_register(rs1);
            let src2 = ctx.load_register(rs2);

            let cmp = ctx.cmp(src1, src2, cmp_mode.into());

            let continue_pc = ctx.add(ctx.pc.upcast(), len);

            let jump_pc = ctx.add(ctx.pc.upcast(), imm);

            let addr = ctx.select(cmp, jump_pc, continue_pc);

            ctx.ret_with_addr(addr.downcast().unwrap())
        }

        _ => panic!("Instruction wasn't a terminal"),
    }
}
