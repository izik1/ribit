use crate::{Block, Id, Source, Terminator};

struct LiveIds([u32; Self::LEN]);

impl LiveIds {
    const LEN: usize = 0x1_0000 / (u32::BITS as usize);

    const fn new() -> Self {
        Self([0; Self::LEN])
    }

    #[inline(always)]
    fn get(&self, id: Id) -> bool {
        let idx = usize::from(id.0) / (u32::BITS as usize);
        let bit = id.0 % (u32::BITS as u16);

        (self.0[idx] >> bit & 1) != 0
    }

    #[inline(always)]
    fn set(&mut self, id: Id) {
        let idx = usize::from(id.0) / (u32::BITS as usize);
        let bit = id.0 % (u32::BITS as u16);

        self.0[idx] |= 1 << bit;
    }
}

pub fn run(block: &mut Block) {
    let mut live_ids = LiveIds::new();

    match &block.terminator {
        Terminator::Ret { addr, .. } => {
            if let Source::Ref(id) = *addr {
                live_ids.set(id);
            }
        }
    }

    let mut all_live = true;

    for instruction in block.instructions.iter().rev() {
        let live = instruction.id().is_none_or(|id| live_ids.get(id));
        if !live {
            all_live = false;
            continue;
        }

        // todo: note wrt dead reads: currently they can be removed, but, later maybe not.
        instruction.visit_arg_ids(|id| live_ids.set(id));
    }

    if all_live {
        return;
    }

    block.instructions.retain(|it| it.id().is_none_or(|id| live_ids.get(id)));
}
