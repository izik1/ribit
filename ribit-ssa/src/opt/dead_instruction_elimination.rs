use crate::{AnySource, Block, Id, Terminator};

fn mark_live(live_instructions: &mut [bool; 0x1_0000], src: AnySource) {
    if let AnySource::Ref(r) = src {
        mark_id_live(live_instructions, r.id);
    }
}

fn mark_id_live(live_instructions: &mut [bool; 0x1_0000], id: Id) {
    live_instructions[id.0 as usize] = true;
}

pub fn run(block: &mut Block) {
    let mut live_ids = [false; 0x1_0000];

    match &block.terminator {
        Terminator::Ret { addr, code } => {
            mark_live(&mut live_ids, *addr);
            mark_live(&mut live_ids, *code);
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
