use std::ops::Range;

use super::{generator::BlockBuilder, ReturnCode};
use crate::instruction;
use std::io::Cursor;
use crate::jit::BasicBlock;
use std::mem;

mod sbi {
    #[repr(u32)]
    enum StatusCode {
        Success = 0,
        ErrFailure = -1i32 as u32,
        ErrNotSupported = -2i32 as u32,
        ErrInvalidParam = -3i32 as u32,
        ErrDenied = -4i32 as u32,
        ErrInvalidAddress = -51i32 as u32,
    }

    pub fn call(regs: &mut [u32; crate::XLEN]) {
        let extension_id = regs[17]; // x17 -> a7
        let extension_funct = regs[16]; // x16 -> a6

        let (code, value) = match (extension_id, extension_funct) {
            (0x01, _) => console_putchar(regs[12]),
            (0x02, _) => console_getchar(),
            (0x10, 0) => get_sbi_spec_version(),
            (0x10, 1) => get_sbi_impl_id(),
            (0x10, 2) => get_sbi_impl_version(),
            (0x10, 3) => probe_extension(regs[12]),
            (0x10, 4) => get_mvendorid(),
            (0x10, 5) => get_marchid(),
            (0x10, 6) => get_mimpid(),
            _ => {
                log::warn!("Unsupported!");
                unsupported()
            },
        };

        regs[10] = code as u32; // a0
        regs[11] = value; // a1
    }

    const fn unsupported() -> (StatusCode, u32) {
        (StatusCode::ErrNotSupported, 0)
    }

    const fn get_sbi_spec_version() -> (StatusCode, u32) {
        // no error, minor version 2
        (StatusCode::Success, 2)
    }

    const fn get_sbi_impl_id() -> (StatusCode, u32) {
        // no error, id = 0xfabadaba
        (StatusCode::Success, 0xfabadaba)
    }

    const fn get_sbi_impl_version() -> (StatusCode, u32) {
        // no error, we're v0.1, encoding TBD
        (StatusCode::Success, 1)
    }

    fn probe_extension(extension_id: u32) -> (StatusCode, u32) {
        // no error
        let id = match extension_id {
            1 | 2 => extension_id,
            _ => 0, // None
        };

        (StatusCode::Success, id)
    }

    fn get_mvendorid() -> (StatusCode, u32) {
        (StatusCode::Success, 0)
    }

    fn get_marchid() -> (StatusCode, u32) {
        (StatusCode::Success, 0)
    }

    fn get_mimpid() -> (StatusCode, u32) {
        (StatusCode::Success, 0)
    }

    fn console_getchar() -> (StatusCode, u32) {
        use std::io::Read;

        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let mut buf = [0];
        let code = match stdin.read_exact(&mut buf) {
            Ok(_) => StatusCode::Success,
            Err(_) => StatusCode::ErrFailure,
        };

        (code, buf[0] as u32)
    }

    fn console_putchar(ch: u32) -> (StatusCode, u32) {
        println!("{:08x}", ch);
        print!("{}", char::from(ch as u8));
        (StatusCode::Success, 0)
    }
}
pub struct Runtime {
    buffer: Option<memmap::Mmap>,
    buffer_write_offset: u64,
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
        // assert memory size for now
        assert_eq!(memory.len(), crate::MEMORY_SIZE as usize);

        if let Some(block_num) = self.ranges.iter().position(|range| range.start == *pc) {
            let block = &self.blocks[block_num];

            let (address, return_code) =
                unsafe { block(regs.as_mut_ptr(), self, memory.as_mut_ptr()) }.into_parts();

            *pc = address;

            match return_code {
                ReturnCode::Normal => {}
                ReturnCode::EBreak => todo!("EBREAK"),
                ReturnCode::ECall => sbi::call(regs),
            }
        } else {
            todo!("failed to find block -- put an error here")
        }
    }

    pub fn lookup_block(&self, start_address: u32) -> bool {
        self.ranges
            .binary_search_by_key(&start_address, |range| range.start)
            .is_ok()
    }

    #[must_use]
    pub fn new() -> Self {
        Self {
            buffer: Some(memmap::MmapOptions::new().len(4096 * 16).stack().map_anon().unwrap().make_exec().unwrap()),
            buffer_write_offset: 0,
            blocks: vec![],
            ranges: vec![],
        }
    }

    fn make_fn(&mut self, block_instrs: Vec<instruction::Info>, branch: instruction::Info) -> BasicBlock {
        let buffer = self.buffer.take().expect("Failed to take buffer");
        let mut buffer = buffer.make_mut().unwrap();
        let funct_ptr = unsafe { buffer.as_mut_ptr().add(self.buffer_write_offset as usize) };

        let mut writer = Cursor::new(buffer.as_mut());
        writer.set_position(self.buffer_write_offset);

        let stream = rasen::Assembler::new(&mut writer).unwrap();

        let mut build_context = BlockBuilder::start(stream, check_ranges);

        for instruction in block_instrs {
            build_context.make_instruction(instruction)
        }

        build_context.complete(branch);

        let start = mem::replace(&mut self.buffer_write_offset, writer.position());
        let instrs = &buffer[(start as usize)..(self.buffer_write_offset as usize)];

        let mut byte_str = String::new();
        {
            let mut bytes = instrs.iter();

            if let Some(byte) = bytes.next() {
                byte_str.push_str(&format!("{:02x}", byte));
            }

            for byte in bytes {
                byte_str.push_str(", ");
                byte_str.push_str(&format!("{:02x}", byte));
            }
        }

        log::debug!("Native block: [{}]", byte_str);

        self.buffer = Some(buffer.make_exec().unwrap());

        unsafe { std::mem::transmute(funct_ptr) }
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

        let funct = self.make_fn(block_instrs, branch);

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
