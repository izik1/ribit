use std::collections::HashMap;

use ribit_ssa::{eval, AnySource, Bitness, Constant, Id, Int};

pub struct Interpreter;

pub struct Block(ribit_ssa::Block);

fn lookup_source(ctx: &HashMap<Id, Constant>, src: AnySource) -> Constant {
    match src {
        AnySource::Const(it) => it,
        AnySource::Ref(it) => ctx[&it.id],
    }
}

fn unwrap_u32(c: Constant) -> u32 {
    match c {
        Constant::Int(Int(Bitness::B32, v)) => v,
        _ => panic!("value was not a 32-bit integer"),
    }
}

fn unwrap_bool(c: Constant) -> bool {
    match c {
        Constant::Bool(b) => b,
        _ => panic!("value was not a boolean"),
    }
}

impl crate::rt::Block for Block {
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
                    let val = Constant::Int(Int(width.into(), val));
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

                &ribit_ssa::Instruction::BinOp { dest, src, op } => {
                    let src1 = lookup_source(&evaluated, src.lhs());
                    let src2 = lookup_source(&evaluated, src.rhs());
                    evaluated.insert(dest, eval::binop(src1, src2, op));
                }

                &ribit_ssa::Instruction::Cmp { dest, src, kind } => {
                    let src1 = unwrap_u32(lookup_source(&evaluated, src.lhs()));
                    let src2 = unwrap_u32(lookup_source(&evaluated, src.rhs()));

                    let res = eval::cmp(src1, src2, kind);

                    evaluated.insert(dest, Constant::Bool(res));
                }

                &ribit_ssa::Instruction::CommutativeBinOp { dest, src1, src2, op } => {
                    let src1 = evaluated[&src1.id];
                    let src2 = lookup_source(&evaluated, src2);
                    let output = eval::commutative_binop(src1, src2, op);
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

unsafe impl crate::rt::Target for Interpreter {
    type Buffer = ();

    type Block = Block;

    fn generate_block(_buffer: &mut Self::Buffer, block: ribit_ssa::Block) -> Self::Block {
        Block(block)
    }
}
