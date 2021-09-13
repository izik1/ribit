use std::collections::HashMap;
use std::mem;

use rasen::params::Register;
use ribit_ssa::{BinOp, Id, Instruction};

/// Counts the clobbers required to execute the instruction (optimally)
///
/// In some cases, an instruction can require writing to more than just the output register, such as when  
#[must_use]
pub fn count_clobbers_for(instr: &Instruction, allocs: &HashMap<Id, Register>) -> usize {
    match instr {
        Instruction::Arg { .. }
        | Instruction::WriteStack { .. }
        | Instruction::ReadStack { .. }
        | Instruction::ReadReg { .. }
        | Instruction::WriteReg { .. }
        | Instruction::ReadMem { .. }
        | Instruction::WriteMem { .. }
        | Instruction::LoadConst { .. }
        | Instruction::Fence
        | Instruction::BinOp {.. } |
        // note: although slightly complicated, this can indeed be done without any branching
        // in even the worst cases, by doing the following:
        // <comparision>
        // set<cc> dest
        // and dest, 0x0000_0001
        // However, there are frequently more efficent code paths than that.
        Instruction::Cmp { .. } => 0,
        Instruction::Select {
            dest,
            cond: _,
            if_true,
            if_false,
        } => {
            match (if_true.id(), if_false.id()) {
                // both sources are consts, so we need a clobber register (otherwise we have no way of loading the number without branching)
                (None, None) => 1,
                // one of the sources is the same as the dest, so we need a clobber to store `dest` as a temporary
                (Some(id), None) | (None, Some(id)) if allocs[&id] == allocs[dest] => 1,
                // no source is the same as dest, so we can avoid a clobber by loading the const into dest _first_.
                (Some(_), None) | (None, Some(_)) => 0,
                // both registers are already allocated, so we definitely _don't_ need a clobber.
                (Some(_), Some(_)) => 0,
            }
        }

        Instruction::Ret { addr, code } => {
            // this instruction "needs" at least two registers- unless addr and cond are both `val`, in which case, just one- one of which must be Zax.

            let register_count = (addr.id().is_some() as usize) + (code.id().is_some() as usize);

            let zax_used = addr.id().or_else(|| code.id()).map_or(false, |id| allocs[&id] == Register::Zax);

            if register_count == 1 && zax_used {
                2
            } else {
                0
            }
        }

    }
}

#[allow(clippy::match_same_arms)]
pub fn legalise(graph: &mut [Instruction], allocs: &HashMap<Id, Register>) {
    for instruction in graph.iter_mut() {
        match instruction {
            Instruction::BinOp { dest, src1, src2, op } => {
                let src_reg_1 = src1.id().and_then(|it| allocs.get(&it));
                let src_reg_2 = src2.id().and_then(|it| allocs.get(&it));
                let dest_reg = allocs.get(dest);

                if src_reg_1 != dest_reg && src_reg_2 == dest_reg && commutative(*op) {
                    mem::swap(src1, src2);
                }
            }

            Instruction::Arg { dest: _, src: _ } => {}
            Instruction::WriteStack { dest: _, src: _ } => {}
            Instruction::ReadStack { dest: _, src: _ } => {}
            Instruction::ReadReg { dest: _, base: _, src: _ } => {}
            Instruction::WriteReg { dest: _, base: _, src: _ } => {}
            Instruction::ReadMem { dest: _, src: _, base: _, width: _, sign_extend: _ } => {}
            Instruction::WriteMem { addr: _, src: _, base: _, width: _ } => {}
            Instruction::LoadConst { dest: _, src: _ } => {}
            Instruction::Cmp { dest: _, src1: _, src2: _, kind: _ } => {}
            Instruction::Select { dest: _, cond: _, if_true: _, if_false: _ } => {}
            Instruction::Ret { addr: _, code: _ } => {}
            Instruction::Fence => {}
        }
    }
}

fn commutative(op: BinOp) -> bool {
    match op {
        BinOp::Add | BinOp::And | BinOp::Or | BinOp::Xor => true,
        BinOp::Sub | BinOp::Sll | BinOp::Sra | BinOp::Srl => false,
    }
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;
    use ribit_ssa::{analysis, opt};

    use crate::register_alloc;
    use crate::test::max_fn;

    #[test]
    fn legalise_max() {
        let (mut instrs, mut id_alloc) = max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let mut instrs = opt::register_writeback_shrinking(&instrs);

        let (allocs, _clobbers) = loop {
            match register_alloc::allocate_registers(&instrs) {
                Ok(allocs) => break allocs,
                Err(spill) => register_alloc::spill(&mut instrs, &mut id_alloc, spill),
            }
        };

        super::legalise(&mut instrs, &allocs);

        let lifetimes = analysis::lifetimes(&instrs);
        let mut lifetimes = format!("{}", analysis::ShowLifetimes::new(&lifetimes, &instrs));
        for (id, register) in &allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
        assert_snapshot!(lifetimes);
    }
}