use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use std::mem;
use std::ops::Range;

use rasen::params::Register;

use super::generator::BlockBuilder;
use crate::rt::common;
use crate::rt::x86_64::{legalise, register_alloc, BasicBlock, ReturnCode};

#[derive(Default)]
pub struct X86_64 {
    // contract: blocks *must* be valid into the buffer.
    blocks: Vec<Block>,
    buffer: Buffer,
    ranges: Vec<Range<u32>>,
}

impl X86_64 {
    fn make_fn(
        buffer: &mut Buffer,
        block: &ribit_ssa::Block,
        allocs: &HashMap<ribit_ssa::Id, Register>,
        clobbers: &HashMap<usize, Vec<Register>>,
    ) -> BasicBlock {
        log::debug!("{}", block.display_instructions());

        let raw_buffer = buffer.raw.take().expect("Failed to take buffer");
        let mut raw_buffer = raw_buffer.make_mut().expect("Failed to make buffer mutable");
        let funct_ptr = unsafe { raw_buffer.as_mut_ptr().add(buffer.write_offset as usize) };

        let mut writer = Cursor::new(raw_buffer.as_mut());
        writer.set_position(buffer.write_offset);

        let stream = rasen::Assembler::new(&mut writer).unwrap();

        let mut build_context = BlockBuilder::start(stream);

        build_context
            .make_block(&block.instructions, allocs, clobbers)
            .expect("todo: handle block creation error");

        let final_clobbers = {
            clobbers
                .get(&(block.instructions.len()))
                .map_or_else(|| Cow::Owned(Vec::new()), Cow::Borrowed)
        };

        build_context
            .complete(&block.terminator, allocs, &*final_clobbers)
            .expect("todo: handle block creation error");

        let start = mem::replace(&mut buffer.write_offset, writer.position());

        let native_buffer = &raw_buffer[(start as usize)..(buffer.write_offset as usize)];

        let byte_str = {
            const SEPARATOR: &str = ", ";
            let len = native_buffer.len();

            // len = 2 bytes per char, eg, `a9`, and an additional SEPARATOR bytes per separator
            // there's 1 separator between every two chars, hence, so, `(len - 1) * SEPARATOR.len()`
            // When there are no characters, there are *still* no separators, so, `len.saturating_sub(1)`
            // ie, `0 - 1` should still give zero.
            let mut byte_str =
                String::with_capacity(len * 2 + len.saturating_sub(1) * SEPARATOR.len());
            let mut bytes = native_buffer.iter();

            if let Some(byte) = bytes.next() {
                byte_str.push_str(&format!("{byte:02x}"));
            }

            for byte in bytes {
                byte_str.push_str(SEPARATOR);

                byte_str.push_str(&format!("{byte:02x}"));
            }

            byte_str
        };

        log::debug!("Native block: [{byte_str}]");

        buffer.raw = Some(raw_buffer.make_exec().unwrap());

        unsafe { std::mem::transmute(funct_ptr) }
    }
}

impl crate::rt::Target for X86_64 {
    type Block = Block;

    fn generate_block(&mut self, mut block: ribit_ssa::Block, start_pc: u32, end_pc: u32) {
        let (allocs, clobbers) = loop {
            match register_alloc::allocate_registers(&block) {
                Ok(allocs) => break allocs,
                Err(spill) => register_alloc::spill(&mut block, spill),
            }
        };

        legalise::legalise(&mut block, &allocs);

        let funct = Self::make_fn(&mut self.buffer, &block, &allocs, &clobbers);

        let insert_idx =
            self.ranges.binary_search_by_key(&start_pc, |range| range.start).unwrap_or_else(|e| e);

        self.blocks.insert(insert_idx, Block(funct));
        self.ranges.insert(insert_idx, start_pc..end_pc);
    }

    fn lookup_block(&self, start_address: u32) -> Option<&Self::Block> {
        common::lookup_block(&self.ranges, &self.blocks, start_address)
    }

    fn execute_block(
        &mut self,
        pc: u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) -> (u32, ReturnCode) {
        self.lookup_block(pc).unwrap().execute(regs, memory)
    }
}

pub struct Block(super::BasicBlock);

impl Block {
    fn execute(&self, registers: &mut [u32; crate::XLEN], memory: &mut [u8]) -> (u32, ReturnCode) {
        // safety: contract of Runtime is that blocks *must* stay valid for as long as they exist.
        unsafe { self.0(registers.as_mut_ptr(), memory.as_mut_ptr()) }.into_parts()
    }
}

pub struct Buffer {
    raw: Option<memmap2::Mmap>,
    write_offset: u64,
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            raw: Some(
                memmap2::MmapOptions::new().len(4096 * 32).map_anon().unwrap().make_exec().unwrap(),
            ),
            write_offset: 0,
        }
    }
}

/*
todo: regression test for:
%0 = args[0]
%2 = x(%0)10
%3 = cmp NE %2, 0
%4 = select %3, 65932, 65964
ret 0, %4
->
 [8b, 47, 28, 85, c0, c7, c0, 00, 00, 00, 00, 0f, 95, c0, 85, c0, c7, c0, ac, 01, 01, 00, c7, c1, 8c, 01, 01, 00, 0f, 45, c1, 0b, c0, c3]
*/
