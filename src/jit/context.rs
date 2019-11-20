use std::ops::Range;

use assembler::ExecutableAnonymousMemoryMap;

use crate::instruction;
pub struct Runtime {
    buffer: ExecutableAnonymousMemoryMap,
    blocks: Vec<super::BasicBlock>,
    ranges: Vec<Range<u32>>,
}

impl Runtime {
    // todo: clean
    pub fn execute_basic_block(
        &mut self,
        pc: &mut u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) {
        // assert 16 MiB of memory for now
        assert_eq!(memory.len(), 1024 * 1024 * 16);

        if let Some(block_num) = self.ranges.iter().position(|range| range.start == *pc) {
            let block = &self.blocks[block_num];
            *pc = unsafe { block(regs.as_mut_ptr(), self, memory.as_mut_ptr()) }
        } else {
            todo!("put an error here")
        }
    }

    #[must_use]
    pub fn new() -> Self {
        Self {
            buffer: ExecutableAnonymousMemoryMap::new(4096 * 16, false, true).unwrap(),
            blocks: vec![],
            ranges: vec![],
        }
    }

    // todo: signature
    pub fn generate_basic_block(
        &mut self,
        block_instrs: Vec<instruction::Info>,
        branch: instruction::Info,
    ) {
        let start_pc = block_instrs
            .first()
            .unwrap_or_else(|| &branch)
            .start_address;

        let end_pc = branch.end_address();

        let stream = self
            .buffer
            .instruction_stream(&assembler::InstructionStreamHints::default());

        let mut build_context = super::BlockBuilder::start(stream, check_ranges);

        for instruction in block_instrs {
            build_context.make_instruction(instruction)
        }

        let funct = build_context.complete(branch);

        let insert_idx = self
            .ranges
            .binary_search_by_key(&start_pc, |range| range.start)
            .unwrap_or_else(|e| e);

        self.blocks.insert(insert_idx, funct);
        self.ranges.insert(insert_idx, start_pc..end_pc);
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

extern "sysv64" fn check_ranges(pc: u32, ctx: &mut Runtime, address: u32) -> bool {
    let mut early_exit = false;

    let mut range_start: Option<usize> = None;
    let mut range_end: Option<usize> = None;

    for (idx, range) in ctx.ranges.iter().enumerate() {
        if range.contains(&address) {
            early_exit |= range.contains(&pc) && pc <= address;
            range_start = range_start.or(Some(idx));
        }

        if !range.contains(&address) && range_start != None {
            range_end = Some(idx);
            break;
        }
    }

    if let Some(range_start) = range_start {
        let range_end = range_end.unwrap_or_else(|| ctx.ranges.len());

        if range_start > range_end {
            unsafe { std::hint::unreachable_unchecked() }
        }

        if range_end > ctx.ranges.len() {
            unsafe { std::hint::unreachable_unchecked() }
        }

        ctx.ranges
            .splice(range_start..range_end, std::iter::empty());

        if range_end > ctx.blocks.len() {
            unsafe { std::hint::unreachable_unchecked() }
        }

        ctx.blocks
            .splice(range_start..range_end, std::iter::empty());
    }

    early_exit
}
