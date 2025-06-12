use std::collections::HashMap;
use std::ops::Range;

use ribit_ssa::{AnySource, Bitness, Constant, Id, eval, ty};

use super::common;

pub struct Interpreter {
    blocks: Vec<Block>,
    ranges: Vec<Range<u32>>,
}

pub struct Block(ribit_ssa::Block);

fn lookup_source(ctx: &HashMap<Id, Constant>, src: AnySource) -> Constant {
    match src {
        AnySource::Const(it) => it,
        AnySource::Ref(it) => ctx[&it.id],
    }
}

fn unwrap_u32(c: Constant) -> u32 {
    match c {
        Constant::Int(ty::Int(Bitness::B32, v)) => v,
        _ => panic!("value was not a 32-bit integer"),
    }
}

fn unwrap_bool(c: Constant) -> bool {
    match c {
        Constant::Bool(b) => b,
        _ => panic!("value was not a boolean"),
    }
}

impl Block {
    fn execute(
        &self,
        registers: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) -> (u32, ribit_core::ReturnCode) {
        let mut evaluated: HashMap<Id, Constant> = HashMap::new();
        let mut stack = vec![None; 256];

        for instruction in &self.0.instructions {
            match instruction {
                ribit_ssa::Instruction::Arg { .. } => {
                    // nop; we instead directly use the inputs.
                }

                &ribit_ssa::Instruction::WriteStack { dest, src } => {
                    stack[dest.0 as usize] = Some(evaluated[&src.id]);
                }

                &ribit_ssa::Instruction::ReadStack { dest, src } => {
                    evaluated.insert(dest, stack[src.0 as usize].unwrap());
                }

                &ribit_ssa::Instruction::ReadReg { dest, base: _, src } => {
                    evaluated.insert(dest, Constant::i32(registers[src.get() as usize]));
                }

                &ribit_ssa::Instruction::WriteReg { dest, base: _, src } => {
                    let src = unwrap_u32(lookup_source(&evaluated, src));
                    registers[dest.get() as usize] = src;
                }

                &ribit_ssa::Instruction::ReadMem { dest, src, base: _, width, sign_extend } => {
                    let src = unwrap_u32(lookup_source(&evaluated, src));

                    let len = match width {
                        ribit_core::Width::Byte => 1,
                        ribit_core::Width::Word => 2,
                        ribit_core::Width::DWord => 4,
                    };

                    let mut val = [0; 4];

                    val[..len].copy_from_slice(&memory[(src as usize)..][..len]);

                    let val = u32::from_le_bytes(val);
                    let val = Constant::Int(ty::Int(width.into(), val));
                    let res = Constant::Int(eval::extend_int(width, val, sign_extend));

                    evaluated.insert(dest, res);
                }

                &ribit_ssa::Instruction::WriteMem { addr, src, base: _, width } => {
                    let addr = unwrap_u32(lookup_source(&evaluated, addr.upcast()));
                    let src = unwrap_u32(lookup_source(&evaluated, src));

                    let src = src.to_le_bytes();

                    // todo: handle address space wraps
                    let len = match width {
                        ribit_core::Width::Byte => 1,
                        ribit_core::Width::Word => 2,
                        ribit_core::Width::DWord => 4,
                    };

                    memory[(addr as usize)..][..len].copy_from_slice(&src[..len]);
                }

                &ribit_ssa::Instruction::ShiftOp { dest, src, op } => {
                    let src1 = lookup_source(&evaluated, src.lhs());
                    let src2 = lookup_source(&evaluated, src.rhs());
                    evaluated.insert(dest, eval::shift(src1, src2, op));
                }

                &ribit_ssa::Instruction::Cmp { dest, args } => {
                    let src1 = evaluated[&args.src1.id];
                    let src2 = lookup_source(&evaluated, args.src2);
                    let res = eval::icmp(src1, src2, args.op);

                    evaluated.insert(dest, Constant::Bool(res));
                }

                &ribit_ssa::Instruction::CommutativeBinOp { dest, args } => {
                    let src1 = evaluated[&args.src1.id];
                    let src2 = lookup_source(&evaluated, args.src2);
                    let output = eval::commutative_binop(src1, src2, args.op);
                    evaluated.insert(dest, output);
                }

                &ribit_ssa::Instruction::Sub { dest, src1, src2 } => {
                    let src1 = lookup_source(&evaluated, src1);
                    let src2 = evaluated[&src2.id];
                    let output = eval::sub(src1, src2);
                    evaluated.insert(dest, output);
                }

                ribit_ssa::Instruction::Select(it) => {
                    let cond = unwrap_bool(evaluated[&it.cond.id]);

                    let src = match cond {
                        true => it.if_true,
                        false => it.if_false,
                    };

                    let src = lookup_source(&evaluated, src);

                    evaluated.insert(it.dest, src);
                }

                ribit_ssa::Instruction::ExtInt(it) => {
                    let src = evaluated[&it.src.id];
                    let res = Constant::Int(eval::extend_int(it.width, src, it.signed));
                    evaluated.insert(it.dest, res);
                }

                ribit_ssa::Instruction::Fence => {}
            }
        }

        match &self.0.terminator {
            &ribit_ssa::Terminator::Ret { addr, code } => {
                let addr = unwrap_u32(lookup_source(&evaluated, addr.upcast()));

                (addr, code)
            }
        }
    }
}

impl crate::rt::Target for Interpreter {
    type Block = Block;

    fn generate_block(&mut self, block: ribit_ssa::Block, start_pc: u32, end_pc: u32) {
        let insert_idx =
            self.ranges.binary_search_by_key(&start_pc, |range| range.start).unwrap_or_else(|e| e);

        self.blocks.insert(insert_idx, Block(block));
        self.ranges.insert(insert_idx, start_pc..end_pc);
    }

    fn lookup_block(&self, start_address: u32) -> Option<&Self::Block> {
        common::lookup_block(&self.ranges, &self.blocks, start_address)
    }

    fn execute_block(
        &mut self,
        pc: u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) -> (u32, ribit_core::ReturnCode) {
        self.lookup_block(pc).unwrap().execute(regs, memory)
    }
}
