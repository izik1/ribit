use crate::{Block, Instruction, Source};

/// Moves register writes to be as close to their computations as possible.
pub fn run(block: &mut Block) {
    let mut stores = vec![];
    let mut instrs = Vec::with_capacity(block.instructions.len());

    for instr in &block.instructions {
        match instr {
            Instruction::WriteReg { dest, src: Source::Ref(r), base } => {
                stores.push((*dest, *r, *base));
            }

            _ => instrs.push(instr.clone()),
        }
    }

    for idx in (0..instrs.len()).rev() {
        if let Some(store_idx) =
            instrs[idx].id().and_then(|id| stores.iter().copied().position(|(_, r, _)| r.id == id))
        {
            let store = stores.remove(store_idx);
            // insert _after_ the instruction we just looked at
            instrs.insert(
                idx + 1,
                Instruction::WriteReg { dest: store.0, src: Source::Ref(store.1), base: store.2 },
            );
        }
    }

    block.instructions = instrs;
}
