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

    let mut live_instruction_count = 0;

    for instruction in block.instructions.iter().rev() {
        if instruction.id().map_or(false, |it| !live_ids[it.0 as usize]) {
            continue;
        }

        live_instruction_count += 1;

        // todo: note wrt dead reads: currently they can be removed, but, later maybe not.

        instruction.visit_arg_ids(|it| {
            live_ids[it.0 as usize] = true;
        });
    }

    let mut instructions = Vec::with_capacity(live_instruction_count);

    for instr in
        block.instructions.iter().filter(|it| it.id().map_or(true, |id| live_ids[id.0 as usize]))
    {
        instructions.push(instr.clone());
    }

    block.instructions = instructions;
}
