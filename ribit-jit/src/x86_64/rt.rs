use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use std::mem;

use rasen::params::Register;

use super::generator::BlockBuilder;
use super::{legalise, register_alloc, ReturnCode};
use crate::x86_64::BasicBlock;

pub struct X86_64;

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
            .make_block(&block.instructions, &allocs, &clobbers)
            .expect("todo: handle block creation error");

        let final_clobbers = {
            clobbers
                .get(&(block.instructions.len()))
                .map_or_else(|| Cow::Owned(Vec::new()), Cow::Borrowed)
        };

        build_context
            .complete(&block.terminator, &allocs, &*final_clobbers)
            .expect("todo: handle block creation error");

        let start = mem::replace(&mut buffer.write_offset, writer.position());

        let native_buffer = &raw_buffer[(start as usize)..(buffer.write_offset as usize)];

        let mut byte_str = String::new();
        {
            let mut bytes = native_buffer.iter();

            if let Some(byte) = bytes.next() {
                byte_str.push_str(&format!("{:02x}", byte));
            }

            for byte in bytes {
                byte_str.push_str(", ");
                byte_str.push_str(&format!("{:02x}", byte));
            }
        }

        log::debug!("Native block: [{}]", byte_str);

        buffer.raw = Some(raw_buffer.make_exec().unwrap());

        unsafe { std::mem::transmute(funct_ptr) }
    }
}

unsafe impl crate::rt::Target for X86_64 {
    type Buffer = Buffer;

    type Block = Block;

    fn generate_block(buffer: &mut Self::Buffer, mut block: ribit_ssa::Block) -> Self::Block {
        let (allocs, clobbers) = loop {
            match register_alloc::allocate_registers(&block) {
                Ok(allocs) => break allocs,
                Err(spill) => register_alloc::spill(&mut block, spill),
            }
        };

        legalise::legalise(&mut block, &allocs);

        let funct = Self::make_fn(buffer, &block, &allocs, &clobbers);
        Block(funct)
    }
}

pub struct Block(super::BasicBlock);

impl crate::rt::Block for Block {
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
