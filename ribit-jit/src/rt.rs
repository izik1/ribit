use std::ops::Range;

use ribit_core::{instruction, ReturnCode};

pub struct Runtime<Rt: Target> {
    buffer: Rt::Buffer,
    blocks: Vec<Rt::Block>,
    ranges: Vec<Range<u32>>,
}

/// # Safety:
/// Block generation can't invalidate *any* blocks backed by the buffer.
pub unsafe trait Target {
    type Buffer: Default;
    type Block: Block;

    fn generate_block(
        buffer: &mut Self::Buffer,
        instructions: Vec<ribit_ssa::Instruction>,
        id_allocator: ribit_ssa::IdAllocator,
    ) -> Self::Block;
}

pub trait Block {
    fn execute(&self, registers: &mut [u32; crate::XLEN], memory: &mut [u8]) -> (u32, ReturnCode);
}

impl<Rt: Target> Runtime<Rt> {
    pub fn execute_basic_block(
        &mut self,
        pc: &mut u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) {
        // assert memory size for now
        assert_eq!(memory.len(), crate::MEMORY_SIZE as usize);

        if let Some(block_num) = self.ranges.iter().position(|range| range.start == *pc) {
            let block = &self.blocks[block_num];

            let (address, return_code) = block.execute(regs, memory);

            *pc = address;

            match return_code {
                ReturnCode::Normal => {}
                ReturnCode::EBreak => todo!("EBREAK"),
                ReturnCode::ECall => crate::sbi::call(regs),
            }
        } else {
            todo!("failed to find block -- put an error here")
        }
    }

    #[must_use]
    pub fn new() -> Self {
        Self { buffer: Rt::Buffer::default(), blocks: vec![], ranges: vec![] }
    }

    #[must_use]
    pub fn lookup_block(&self, start_address: u32) -> bool {
        self.ranges.binary_search_by_key(&start_address, |range| range.start).is_ok()
    }

    pub fn generate_basic_block(
        &mut self,
        block_instrs: Vec<instruction::Info>,
        branch: instruction::Info,
        start_pc: u32,
        end_pc: u32,
    ) {
        let mut lower_context = ribit_ssa::lower::Context::new(start_pc, crate::MEMORY_SIZE);

        for instr in block_instrs {
            ribit_ssa::lower::non_terminal(&mut lower_context, instr.instruction, instr.len);
        }

        let (mut instrs, id_alloc) =
            ribit_ssa::lower::terminal(lower_context, branch.instruction, branch.len);

        ribit_ssa::opt::fold_and_prop_consts(&mut instrs);
        let instrs = ribit_ssa::opt::dead_instruction_elimination(&instrs);
        let instrs = ribit_ssa::opt::register_writeback_shrinking(&instrs);

        let block = Rt::generate_block(&mut self.buffer, instrs, id_alloc);

        let insert_idx =
            self.ranges.binary_search_by_key(&start_pc, |range| range.start).unwrap_or_else(|e| e);

        self.blocks.insert(insert_idx, block);
        self.ranges.insert(insert_idx, start_pc..end_pc);
    }
}

impl<Rt: Target> Default for Runtime<Rt> {
    fn default() -> Self {
        Self::new()
    }
}
