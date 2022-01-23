use crate::{Block, Source, Terminator};

pub fn run(block: &mut Block) {
    let mut live_ids = [false; 0x1_0000];

    match &block.terminator {
        Terminator::Ret { addr, .. } => {
            if let Source::Ref(id) = *addr {
                live_ids[id.0 as usize] = true;
            }
        }
    }

    let mut all_live = true;

    for instruction in block.instructions.iter().rev() {
        let live = instruction.id().map_or(true, |id| live_ids[id.0 as usize]);
        if !live {
            all_live = false;
            continue;
        }

        // todo: note wrt dead reads: currently they can be removed, but, later maybe not.
        instruction.visit_arg_ids(|it| {
            live_ids[it.0 as usize] = true;
        });
    }

    if all_live {
        return;
    }

    block.instructions.retain(|it| it.id().map_or(true, |id| live_ids[id.0 as usize]));
}
