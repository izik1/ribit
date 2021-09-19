use std::collections::HashMap;

use crate::ty::Constant;
use crate::{eval, Block, Id, Instruction, Source, Terminator};

fn const_id_lookup(consts: &HashMap<Id, Constant>, src: Source) -> Option<Constant> {
    src.reference().and_then(|it| consts.get(&it.id).copied())
}

fn const_prop(consts: &HashMap<Id, Constant>, src: &mut Source) -> Option<Constant> {
    if let Some(v) = const_id_lookup(consts, *src) {
        *src = Source::Const(v);
        Some(v)
    } else if let Source::Const(v) = src {
        Some(*v)
    } else {
        None
    }
}

// todo: `pub fn complex_const_prop(graph: &mut [Instruction])`

pub fn run(block: &mut Block) {
    let mut consts = HashMap::new();
    for instruction in block.instructions.iter_mut() {
        let (dest, val) = match instruction {
            Instruction::LoadConst { dest, src } => (*dest, *src),

            Instruction::Fence
            | Instruction::Arg { .. }
            | Instruction::ReadStack { .. }
            | Instruction::WriteStack { .. } => continue,
            Instruction::ReadReg { base, .. } => {
                const_prop(&consts, base);
                continue;
            }

            Instruction::WriteReg { src, base, .. }
            | Instruction::ReadMem { src, base, .. }
            | Instruction::WriteMem { src, base, .. } => {
                const_prop(&consts, src);
                const_prop(&consts, base);

                continue;
            }

            Instruction::BinOp { dest, src1, src2, op } => {
                let src1 = const_prop(&consts, src1);
                let src2 = const_prop(&consts, src2);

                let (lhs, rhs) = match (src1, src2) {
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                    _ => continue,
                };

                let res = match (lhs, rhs) {
                    (Constant::Int(lhs), Constant::Int(rhs))
                        if lhs.bits() == rhs.bits() && lhs.bits() == 32 =>
                    {
                        Constant::i32(eval::binop(lhs.1, rhs.1, *op))
                    }

                    (Constant::Int(lhs), Constant::Int(rhs)) => {
                        panic!("mismatched integral bitness: ({} != {})", lhs.bits(), rhs.bits())
                    }
                };

                (*dest, res)
            }

            Instruction::Cmp { dest, src1, src2, kind } => {
                let src1 = const_prop(&consts, src1);
                let src2 = const_prop(&consts, src2);

                // note: this explicitly doesn't simplify things like
                // `cmp eq %0, %0`, that's for a different pass
                // todo: write pass for the above.

                let (lhs, rhs) = match (src1, src2) {
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                    _ => continue,
                };

                let res = match (lhs, rhs) {
                    (Constant::Int(lhs), Constant::Int(rhs)) if lhs.bits() == rhs.bits() => {
                        Constant::Int(eval::cmp_int(lhs, rhs, *kind))
                    }

                    (Constant::Int(lhs), Constant::Int(rhs)) => {
                        panic!("mismatched integral bitness: ({} != {})", lhs.bits(), rhs.bits())
                    }
                };

                (*dest, res)
            }

            Instruction::Select { dest, cond, if_true, if_false } => {
                let cond = const_prop(&consts, cond);
                let if_true = const_prop(&consts, if_true);
                let if_false = const_prop(&consts, if_false);

                let cond = match cond {
                    Some(cond) => cond,
                    None => continue,
                };

                let cond = match cond {
                    Constant::Int(i) => i,
                };

                if let Some(res) = eval::partial_select_int(cond, if_true, if_false) {
                    (*dest, res)
                } else {
                    continue;
                }
            }
        };

        *instruction = Instruction::LoadConst { dest, src: val };

        consts.insert(dest, val);
    }

    match &mut block.terminator {
        Terminator::Ret { addr, code } => {
            const_prop(&consts, addr);
            const_prop(&consts, code);
        }
    }
}
