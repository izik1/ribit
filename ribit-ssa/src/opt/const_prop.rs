use std::collections::HashMap;

use crate::ty::{ConstTy, Constant};
use crate::{eval, AnySource, Block, Id, Instruction, Terminator, TypedRef};

fn typed_const_ref_lookup<T: ConstTy>(
    consts: &HashMap<Id, Constant>,
    src: TypedRef<T>,
) -> Option<T::Const> {
    consts.get(&src.id).copied().and_then(T::downcast)
}

fn const_id_lookup(consts: &HashMap<Id, Constant>, src: AnySource) -> Option<Constant> {
    src.reference().and_then(|it| consts.get(&it.id).copied())
}

fn const_prop(consts: &HashMap<Id, Constant>, src: &mut AnySource) {
    if let Some(v) = const_id_lookup(consts, *src) {
        *src = AnySource::Const(v);
    }
}

// todo: `pub fn complex_const_prop(graph: &mut [Instruction])`

fn run_instruction(
    consts: &mut HashMap<Id, Constant>,
    instruction: &mut Instruction,
) -> Option<(Id, Constant)> {
    match instruction {
        Instruction::Fence
        | Instruction::Arg { .. }
        | Instruction::ReadStack { .. }
        | Instruction::WriteStack { .. } => None,
        Instruction::ReadReg { base, .. } => {
            const_prop(consts, base);
            None
        }

        Instruction::WriteReg { src, base, .. }
        | Instruction::ReadMem { src, base, .. }
        | Instruction::WriteMem { src, base, .. } => {
            const_prop(consts, src);
            const_prop(consts, base);

            None
        }

        Instruction::CommutativeBinOp { dest, src1, src2, op } => {
            const_prop(consts, src2);

            let lhs = const_id_lookup(consts, AnySource::Ref(*src1))?;

            // have to indiana jones this stuff around...
            // potentially confusing note: if we return early,
            // this change happens, but if we don't, we ignore it.
            let old_src2 = std::mem::replace(src2, AnySource::Const(lhs));

            let rhs = match old_src2 {
                AnySource::Ref(r) => {
                    *src1 = r;
                    return None;
                }
                AnySource::Const(konst) => konst,
            };

            let res = match (lhs, rhs) {
                (Constant::Int(lhs), Constant::Int(rhs))
                    if lhs.bits() == rhs.bits() && lhs.bits() == 32 =>
                {
                    Constant::i32(eval::commutative_binop(lhs.1, rhs.1, *op))
                }

                (Constant::Int(lhs), Constant::Int(rhs)) => {
                    panic!("mismatched integral bitness: ({} != {})", lhs.bits(), rhs.bits())
                }
                (lhs, rhs) => {
                    panic!("can't compare types of `{}` and `{}`", lhs.ty(), rhs.ty())
                }
            };

            Some((*dest, res))
        }

        Instruction::BinOp { dest, src1, src2, op } => {
            const_prop(consts, src1);
            const_prop(consts, src2);

            let (lhs, rhs) = (src1.constant()?, src2.constant()?);

            let res = match (lhs, rhs) {
                (Constant::Int(lhs), Constant::Int(rhs))
                    if lhs.bits() == rhs.bits() && lhs.bits() == 32 =>
                {
                    Constant::i32(eval::binop(lhs.1, rhs.1, *op))
                }

                (Constant::Int(lhs), Constant::Int(rhs)) => {
                    panic!("mismatched integral bitness: ({} != {})", lhs.bits(), rhs.bits())
                }
                (lhs, rhs) => {
                    panic!("can't compare types of `{}` and `{}`", lhs.ty(), rhs.ty())
                }
            };

            Some((*dest, res))
        }

        Instruction::Cmp { dest, src1, src2, kind } => {
            const_prop(consts, src1);
            const_prop(consts, src2);

            // note: this explicitly doesn't simplify things like
            // `cmp eq %0, %0`, that's for a different pass
            // todo: write pass for the above.

            let (lhs, rhs) = (src1.constant()?, src2.constant()?);

            let result = match (lhs, rhs) {
                (Constant::Int(lhs), Constant::Int(rhs)) if lhs.bits() == rhs.bits() => {
                    Constant::Bool(eval::cmp_int(lhs, rhs, *kind))
                }

                (Constant::Int(lhs), Constant::Int(rhs)) => {
                    panic!("mismatched integral bitness: ({} != {})", lhs.bits(), rhs.bits())
                }
                (Constant::Bool(_lhs), Constant::Bool(_rhs)) => {
                    todo!("cmp between bools?")
                }
                (lhs, rhs) => panic!("mismatched types: ({} != {})", lhs.ty(), rhs.ty()),
            };

            Some((*dest, result))
        }

        Instruction::Select { dest, cond, if_true, if_false } => {
            const_prop(consts, if_true);
            const_prop(consts, if_false);

            let konst = typed_const_ref_lookup(consts, *cond)?;

            let taken = match konst {
                true => *if_true,
                false => *if_false,
            };

            taken.constant().map(|c| (*dest, c))
        }

        Instruction::ExtInt { dest, width, src, signed } => {
            let src = const_id_lookup(consts, AnySource::Ref(*src))?;

            Some((*dest, Constant::Int(eval::extend_int(*width, src, *signed))))
        }
    }
}

pub fn run(block: &mut Block) {
    let mut consts = HashMap::new();
    for instruction in &mut block.instructions {
        if let Some((dest, val)) = run_instruction(&mut consts, instruction) {
            consts.insert(dest, val);
        }
    }

    match &mut block.terminator {
        Terminator::Ret { addr, code } => {
            const_prop(&consts, addr);
            const_prop(&consts, code);
        }
    }
}
