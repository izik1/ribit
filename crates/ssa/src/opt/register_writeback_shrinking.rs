use crate::{AnySource, Block, Instruction};

/// Moves register writes to be as close to their computations as possible.
pub fn run(block: &mut Block) {
    let mut stores = vec![];

    block.instructions.retain(|instr| match instr {
        Instruction::WriteReg { dest, src: AnySource::Ref(r), base } => {
            stores.push((*dest, *r, *base));
            false
        }
        _ => true,
    });

    for idx in (0..block.instructions.len()).rev() {
        let instr = &block.instructions[idx];

        let Some(id) = instr.id() else {
            continue;
        };

        let read_reg = match instr {
            Instruction::ReadReg { dest: _, base: _, src } => Some(*src),
            _ => None,
        };

        for store in stores.extract_if(.., |(_, r, _)| r.id == id) {
            // if we're writing to a reg an ID that's read from it... drop the write (because there's no write).
            // put another way, optimize this pattern:
            // ```
            // %4 = x(%2)26
            // x(%2)26 = %4
            // ```
            // the read can then be removed (via DIE) if this was its only use.
            if read_reg == Some(store.0) {
                continue;
            }

            // insert _after_ the instruction we just looked at
            block.instructions.insert(
                idx + 1,
                Instruction::WriteReg {
                    dest: store.0,
                    src: AnySource::Ref(store.1),
                    base: store.2,
                },
            );
        }
    }

    // the only way this debug assert would be able to fail without bugs is if a write gets placed with a fake ID, and that's not worth checking constantly for.
    debug_assert_eq!(stores, Vec::new());
}
