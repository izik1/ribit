use std::ops::Range;

use ribit_core::{instruction, ReturnCode};
use ribit_ssa::opt;
use ribit_ssa::opt::pass_manager::InplacePass;

pub struct Runtime<Rt: Target> {
    buffer: Rt::Buffer,
    blocks: Vec<Rt::Block>,
    ranges: Vec<Range<u32>>,
    opt_pass_manager: opt::PassManager,
}

/// # Safety:
/// Block generation can't invalidate *any* blocks backed by the buffer.
pub unsafe trait Target {
    type Buffer: Default;
    type Block: Block;

    fn generate_block(buffer: &mut Self::Buffer, block: ribit_ssa::Block) -> Self::Block;
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
        Self {
            buffer: Rt::Buffer::default(),
            blocks: vec![],
            ranges: vec![],
            opt_pass_manager: opt::PassManager::optimized(),
        }
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

        let mut block = ribit_ssa::lower::terminal(lower_context, branch.instruction, branch.len);

        self.opt_pass_manager.run(&mut block);

        let block = Rt::generate_block(&mut self.buffer, block);

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
