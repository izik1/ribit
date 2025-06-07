use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use std::mem;
use std::ops::Range;

use rasen::params::Register;

use super::generator::BlockBuilder;
use crate::rt::common;
use crate::rt::x86_64::{BasicBlockFunc, Block, ReturnCode, legalise, register_alloc};

#[derive(Default)]
pub struct X86_64 {
    // contract: blocks *must* be valid into the buffer.
    blocks: Vec<Block>,
    buffer: Buffer,
    ranges: Vec<Range<u32>>,
}

fn print_block(buffer: &[u8], block: &Block) {
    use core::fmt::Write;
    let buffer = &buffer[block.offset..][..block.length];
    let byte_str = {
        const SEPARATOR: &str = ", ";
        let len = buffer.len();

        // len = 2 bytes per char, eg, `a9`, and an additional SEPARATOR bytes per separator
        // there's 1 separator between every two chars, hence, so, `(len - 1) * SEPARATOR.len()`
        // When there are no characters, there are *still* no separators, so, `len.saturating_sub(1)`
        // ie, `0 - 1` should still give zero.
        let mut byte_str = String::with_capacity(len * 2 + len.saturating_sub(1) * SEPARATOR.len());
        let mut bytes = buffer.iter();

        if let Some(byte) = bytes.next() {
            write!(byte_str, "{byte:02x}").unwrap();
        }

        for byte in bytes {
            byte_str.push_str(SEPARATOR);

            write!(byte_str, "{byte:02x}").unwrap();
        }

        byte_str
    };

    log::debug!("Native block: [{byte_str}]");
}

impl X86_64 {
    fn make_fn(
        buffer: &mut Buffer,
        block: &ribit_ssa::Block,
        allocs: &HashMap<ribit_ssa::Id, Register>,
        clobbers: &HashMap<usize, Vec<Register>>,
    ) -> Block {
        log::debug!("{}", block.display_instructions());

        let raw_buffer = buffer.raw.take().expect("Failed to take buffer");
        let mut raw_buffer = raw_buffer.make_mut().expect("Failed to make buffer mutable");

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
            .complete(&block.terminator, allocs, &final_clobbers)
            .expect("todo: handle block creation error");

        let start = mem::replace(&mut buffer.write_offset, writer.position()) as usize;

        let len = (buffer.write_offset as usize) - (start as usize);

        let block = Block { offset: start, length: len };

        print_block(&raw_buffer, &block);

        buffer.raw = Some(raw_buffer.make_exec().unwrap());

        block
    }
}

impl crate::rt::Target for X86_64 {
    type Block = Block;

    fn generate_block(&mut self, mut block: ribit_ssa::Block, start_pc: u32, end_pc: u32) {
        let register_alloc::AllocMap { allocs, clobbers } = register_alloc::alloc(&mut block);

        legalise::legalise(&mut block, &allocs);

        let block = Self::make_fn(&mut self.buffer, &block, &allocs, &clobbers);

        let insert_idx =
            self.ranges.binary_search_by_key(&start_pc, |range| range.start).unwrap_or_else(|e| e);

        self.blocks.insert(insert_idx, block);
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
        let block = self.lookup_block(pc).unwrap();
        // safety: blocks *must* point to a valid offset into the buffer.
        let ptr = unsafe { self.buffer.raw.as_ref().unwrap().as_ptr().add(block.offset) };

        // safety: blocks in `self.blocks` must be valid BasicBlockFuncs.
        let func: BasicBlockFunc = unsafe { std::mem::transmute(ptr) };

        // safety: the code we generated promises to not invoke UB, which is the best you can do with dynamic code.
        let ret = unsafe { func(regs.as_mut_ptr(), memory.as_mut_ptr()) };

        ret.into_parts()
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
