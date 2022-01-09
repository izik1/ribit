use std::collections::HashMap;

use super::{lookup, typed_const_ref_lookup};
use crate::{
    eval, instruction, AnySource, Block, Constant, Id, Instruction, SourcePair, Terminator,
};

fn run_instruction(
    consts: &HashMap<Id, Constant>,
    instruction: &mut Instruction,
) -> Option<(Id, Constant)> {
    match instruction {
        Instruction::Fence
        | Instruction::Arg { .. }
        | Instruction::ReadStack { .. }
        | Instruction::WriteStack { .. } => None,

        Instruction::ReadReg { base, .. } => {
            *base = lookup(consts, base.upcast()).downcast().unwrap();
            None
        }

        Instruction::WriteReg { src, base, .. } | Instruction::ReadMem { src, base, .. } => {
            *src = lookup(consts, *src);
            *base = lookup(consts, base.upcast()).downcast().unwrap();

            None
        }

        Instruction::WriteMem { src, base, addr, .. } => {
            *src = lookup(consts, *src);
            *base = lookup(consts, base.upcast()).downcast().unwrap();
            *addr = lookup(consts, addr.upcast()).downcast().unwrap();

            None
        }

        Instruction::CommutativeBinOp { dest, src1, src2, op } => {
            *src2 = lookup(consts, *src2);

            let lhs = lookup(consts, AnySource::Ref(*src1)).constant()?;
            let rhs = {
                // have to indiana jones this stuff around...
                // potentially confusing note: if we return early,
                // this change happens, but if we don't, we ignore it.
                let src2 = std::mem::replace(src2, AnySource::Const(lhs));

                match src2 {
                    AnySource::Ref(r) => {
                        *src1 = r;
                        return None;
                    }
                    AnySource::Const(konst) => konst,
                }
            };

            let res = eval::commutative_binop(lhs, rhs, *op);

            Some((*dest, res))
        }

        Instruction::BinOp { dest, src, op } => {
            let lhs = lookup(consts, src.lhs());
            let rhs = lookup(consts, src.rhs());

            let (lhs, rhs) = match SourcePair::try_from((lhs, rhs)) {
                Ok(it) => {
                    *src = it;
                    return None;
                }

                Err(it) => it,
            };

            let res = eval::binop(lhs, rhs, *op);

            Some((*dest, res))
        }

        Instruction::Cmp { dest, src, kind } => {
            // note: this explicitly doesn't simplify things like
            // `cmp eq %0, %0`, that's for a different pass
            // todo: write pass for the above.

            let lhs = lookup(consts, src.lhs());
            let rhs = lookup(consts, src.rhs());

            let (lhs, rhs) = match SourcePair::try_from((lhs, rhs)) {
                Ok(it) => {
                    *src = it;
                    return None;
                }

                Err(it) => it,
            };

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

        Instruction::Select(it) => select(consts, it),
        Instruction::ExtInt(it) => ext_int(consts, it),
    }
}

fn ext_int(consts: &HashMap<Id, Constant>, it: &mut instruction::ExtInt) -> Option<(Id, Constant)> {
    let src = lookup(consts, AnySource::Ref(it.src)).constant()?;
    Some((it.dest, Constant::Int(eval::extend_int(it.width, src, it.signed))))
}

fn select(consts: &HashMap<Id, Constant>, it: &mut instruction::Select) -> Option<(Id, Constant)> {
    it.if_true = lookup(consts, it.if_true);
    it.if_false = lookup(consts, it.if_false);

    let konst = typed_const_ref_lookup(consts, it.cond)?;

    let taken = match konst {
        true => it.if_true,
        false => it.if_false,
    };

    taken.constant().map(|c| (it.id(), c))
}

pub fn run(block: &mut Block) {
    let mut consts = HashMap::new();
    for instruction in &mut block.instructions {
        if let Some((dest, val)) = run_instruction(&consts, instruction) {
            consts.insert(dest, val);
        }
    }

    match &mut block.terminator {
        Terminator::Ret { addr, .. } => {
            *addr = lookup(&consts, addr.upcast()).downcast().unwrap();
        }
    }
}
