use std::collections::HashMap;
use std::mem;

use rasen::params::Register;
use ribit_ssa::{BinOp, Block, Id, Instruction, Terminator};

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
            match (if_true.reference(), if_false.reference()) {
                // both sources are consts, so we need a clobber register (otherwise we have no way of loading the number without branching)
                (None, None) => 1,
                // one of the sources is the same as the dest, so we need a clobber to store `dest` as a temporary
                (Some(r), None) | (None, Some(r)) if allocs[&r.id] == allocs[dest] => 1,
                // no source is the same as dest, so we can avoid a clobber by loading the const into dest _first_.
                (Some(_), None) | (None, Some(_)) => 0,
                // both registers are already allocated, so we definitely _don't_ need a clobber.
                (Some(_), Some(_)) => 0,
            }
        }
    }
}

/// Counts the clobbers required to execute the terminator (optimally)
#[must_use]
pub fn count_clobbers_for_terminal(
    terminator: &Terminator,
    allocs: &HashMap<Id, Register>,
) -> usize {
    match terminator {
        Terminator::Ret { addr, code } => {
            // this terminator "needs" at least two registers- unless addr and cond are both `val`,
            // in which case, just one- one of which must be Zax.

            let register_count =
                (addr.reference().is_some() as usize) + (code.reference().is_some() as usize);

            let zax_used = addr
                .reference()
                .or_else(|| code.reference())
                .map_or(false, |r| allocs[&r.id] == Register::Zax);

            if register_count == 1 && zax_used { 2 } else { 0 }
        }
    }
}

#[allow(clippy::match_same_arms)]
pub fn legalise(block: &mut Block, allocs: &HashMap<Id, Register>) {
    for instruction in block.instructions.iter_mut() {
        match instruction {
            Instruction::BinOp { dest, src1, src2, op } => {
                let src_reg_1 = src1.reference().and_then(|it| allocs.get(&it.id));
                let src_reg_2 = src2.reference().and_then(|it| allocs.get(&it.id));
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
            Instruction::Cmp { dest: _, src1: _, src2: _, kind: _ } => {}
            Instruction::Select { dest: _, cond: _, if_true: _, if_false: _ } => {}
            Instruction::Fence => {}
        }
    }

    match &mut block.terminator {
        ribit_ssa::Terminator::Ret { addr: _, code: _ } => {}
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
    use ribit_ssa::{analysis, opt};

    use crate::test::max_fn;
    use crate::x86_64::register_alloc;

    #[test]
    fn legalise_max() {
        let mut block = max_fn();

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let (allocs, _clobbers) = loop {
            match register_alloc::allocate_registers(&block) {
                Ok(allocs) => break allocs,
                Err(spill) => register_alloc::spill(&mut block, spill),
            }
        };

        super::legalise(&mut block, &allocs);

        let lifetimes = analysis::lifetimes(&block);
        let mut lifetimes = format!(
            "{}",
            analysis::ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator)
        );

        for (id, register) in &allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)

        expect_test::expect![[r#"
            [10, -, -, -, -, -, -, -, -] zdi = args[0]
            [ 9, 7, -, -, -, -, -, -, -] zax = x(zdi)10
            [ 8, 6, 1, -, -, -, -, -, -] zcx = x(zdi)11
            [ 7, 5, 0, 3, -, -, -, -, -] zcx = add zcx, zax
            [ 6, 4, -, 2, 2, -, -, -, -] zdx = srl zcx, 0000001f
            [ 5, 3, -, 1, 1, -, -, -, -] x(zdi)12 = zdx
            [ 4, 2, -, 0, 0, 2, -, -, -] zcx = and zcx, zdx
            [ 3, 1, -, -, -, 1, -, -, -] x(zdi)11 = zcx
            [ 2, 0, -, -, -, 0, 1, -, -] zax = add zax, zcx
            [ 1, -, -, -, -, -, 0, -, -] x(zdi)10 = zax
            [ 0, -, -, -, -, -, -, 1, -] zax = x(zdi)1
            [--, -, -, -, -, -, -, 0, 1] zax = and zax, fffffffe
            [--, -, -, -, -, -, -, -, 0] ret 00000000, zax
        "#]]
        .assert_eq(&lifetimes);
    }
}
