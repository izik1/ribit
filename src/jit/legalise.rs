use crate::ssa::Id;
use crate::ssa::{BinOp, Instruction};
use rasen::params::Register;
use std::collections::HashMap;
use std::mem;

pub fn legalise(graph: &mut [Instruction], allocs: &HashMap<Id, Register>) {
    for instruction in graph.iter_mut() {
        match instruction {
            Instruction::BinOp {
                dest,
                src1,
                src2,
                op,
            } => {
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
            Instruction::ReadReg {
                dest: _,
                base: _,
                src: _,
            } => {}
            Instruction::WriteReg {
                dest: _,
                base: _,
                src: _,
            } => {}
            Instruction::ReadMem {
                dest: _,
                src: _,
                base: _,
                width: _,
                sign_extend: _,
            } => {}
            Instruction::WriteMem {
                addr: _,
                src: _,
                base: _,
                width: _,
            } => {}
            Instruction::LoadConst { dest: _, src: _ } => {}
            Instruction::Cmp {
                dest: _,
                src1: _,
                src2: _,
                kind: _,
            } => {}
            Instruction::Select {
                dest: _,
                cond: _,
                if_true: _,
                if_false: _,
            } => {}
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
    use crate::jit::register_alloc;
    use crate::ssa::{analysis, opt};
    use insta::assert_snapshot;

    #[test]
    fn legalise_max() {
        let (mut instrs, mut id_alloc) = crate::ssa::max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let mut instrs = opt::register_writeback_shrinking(&instrs);

        let allocs = loop {
            match register_alloc::allocate_registers(&instrs) {
                Ok(allocs) => break allocs,
                Err(spill) => register_alloc::spill(&mut instrs, &mut id_alloc, spill),
            }
        };

        super::legalise(&mut instrs, &allocs);

        let lifetimes = analysis::lifetimes(&instrs);
        let mut lifetimes = format!("{}", analysis::ShowLifetimes::new(&lifetimes, &instrs));
        for (id, register) in &allocs {
            lifetimes = lifetimes.replace(
                &id.to_string(),
                &crate::test::FmtRegister(*register).to_string(),
            );
        }

        // the main snapshot (long form, allows human readability)
        assert_snapshot!(lifetimes);
    }
}
