#![forbid(unsafe_code)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate)]

use xmas_elf::header::Data;
use xmas_elf::sections::SectionData;
use xmas_elf::symbol_table::{Binding, Entry};
use xmas_elf::ElfFile;

struct TestAddrs {
    _from_host: u32,
    to_host: u32,
    begin_signature: u32,
    end_signature: u32,
}

#[derive(Default)]
struct TestAddrBuilder {
    to_host: Option<u32>,
    begin_signature: Option<u32>,
    end_signature: Option<u32>,
}

fn sym_iter<S: Entry>(
    ctx: &mut TestAddrBuilder,
    file: &ElfFile,
    syms: &[S],
) -> Result<(), &'static str> {
    for sym in syms {
        if !matches!(sym.get_binding()?, Binding::Global) {
            continue;
        }

        let name = sym.get_name(file)?;

        log::info!("sym: {}", name);

        let value = sym.value();

        if name == "tohost" {
            log::info!("found tohost: {:08x}", value);
            ctx.to_host = Some(value as u32);
        }

        if name == "begin_signature" {
            log::info!("found begin_signature: {:08x}", value);

            ctx.begin_signature = Some(value as u32);
        }

        if name == "end_signature" {
            log::info!("found end_signature: {:08x}", value);

            ctx.end_signature = Some(value as u32);
        }
    }

    Ok(())
}

impl TestAddrs {
    fn from_elf(file: &ElfFile) -> Option<Self> {
        let mut ctx = TestAddrBuilder::default();

        for section in file.section_iter() {
            match section.get_data(file).ok()? {
                SectionData::SymbolTable32(tabs) => sym_iter(&mut ctx, file, tabs).ok()?,
                SectionData::SymbolTable64(tabs) => sym_iter(&mut ctx, file, tabs).ok()?,
                _ => {}
            }
        }

        Some(Self {
            _from_host: 0,
            to_host: ctx.to_host?,
            begin_signature: ctx.begin_signature?,
            end_signature: ctx.end_signature?,
        })
    }
}

pub struct ExecutionEngine {
    // xregs[0] is fixed to 0
    xregs: [u32; ribit_jit::XLEN],
    pc: u32,
    memory: Box<[u8]>,
    jit: ribit_jit::DefaultRuntime,
    test_ctx: Option<TestAddrs>,
}

impl ExecutionEngine {
    #[must_use]
    pub fn pc(&self) -> u32 {
        self.pc
    }

    #[must_use]
    pub fn new(program: &[u8]) -> Self {
        assert!(ribit_jit::MEMORY_SIZE as usize >= program.len());
        let mut memory = vec![0; ribit_jit::MEMORY_SIZE as usize].into_boxed_slice();
        memory[0x10000..][..program.len()].copy_from_slice(program);

        let xregs = [0; ribit_jit::XLEN];
        let pc = 0x10000;
        let jit = ribit_jit::DefaultRuntime::new();

        Self { xregs, pc, memory, jit, test_ctx: None }
    }

    #[must_use]
    pub fn new_elf(program: &[u8]) -> Self {
        let elf = xmas_elf::ElfFile::new(program).unwrap();
        assert!(matches!(elf.header.pt1.data(), Data::LittleEndian));
        let entry = match elf.header.pt2 {
            xmas_elf::header::HeaderPt2::Header32(it) => it.entry_point,
            xmas_elf::header::HeaderPt2::Header64(_) => panic!(),
        };

        assert_eq!(entry, 0x10000);

        let mut memory = vec![0; ribit_jit::MEMORY_SIZE as usize].into_boxed_slice();

        for header in elf.program_iter() {
            if header.get_type().unwrap() == xmas_elf::program::Type::Load {
                memory[(header.physical_addr() as usize)..][..(header.mem_size() as usize)]
                    .copy_from_slice(
                        &program[(header.physical_addr() as usize)..]
                            [..(header.mem_size() as usize)],
                    );
            }
        }

        let xregs = [0; ribit_jit::XLEN];
        let pc = 0x10000;
        let jit = ribit_jit::DefaultRuntime::new();

        Self { xregs, pc, memory, jit, test_ctx: TestAddrs::from_elf(&elf) }
    }

    fn parse_compressed(
        &mut self,
    ) -> Result<ribit_core::instruction::Info, ribit_decode::DecodeError> {
        if self.pc as usize + 1 >= self.memory.len() {
            return Err(ribit_decode::DecodeError::Other);
        }

        let instr = u16::from_le_bytes(
            self.memory[(self.pc as usize)..][..2]
                .try_into()
                .expect("bad slice size expected 2???"),
        );

        log::debug!("instruction: {:016b}", instr);

        let instr = ribit_decode::compressed::decode_instruction(instr)?;
        let info = ribit_core::instruction::Info::new(instr, 2);
        self.pc += 2;
        Ok(info)
    }

    fn parse_instruction(
        &mut self,
    ) -> Result<ribit_core::instruction::Info, ribit_decode::DecodeError> {
        if self.pc as usize + 3 >= self.memory.len() {
            return Err(ribit_decode::DecodeError::Other);
        }

        let instr = u32::from_le_bytes(
            self.memory[(self.pc as usize)..][..4]
                .try_into()
                .expect("bad slice size expected 4???"),
        );

        // log::debug!("instruction bytes: {:08x}", instr.to_le());

        if instr & 0b11 == 0b11 {
            log::debug!("instruction: {:032b}", instr);
            let instr = ribit_decode::instruction(instr)?;
            let info = ribit_core::instruction::Info::new(instr, 4);
            self.pc += 4;
            Ok(info)
        } else {
            self.parse_compressed()
        }
    }

    fn create_block(&mut self) -> Result<(), ribit_decode::DecodeError> {
        let pc = self.pc;
        let mut block_instrs = Vec::new();

        let terminator;
        loop {
            log::debug!("PC: ${:04x}", self.pc);
            let inst_info = self.parse_instruction()?;

            log::debug!(
                "instr: {}",
                ribit_core::disassemble::FmtInstruction::from_info(&inst_info)
            );

            if inst_info.instruction.is_terminator() {
                terminator = inst_info;
                break;
            }

            block_instrs.push(inst_info);
        }

        self.pc = pc;
        self.jit.generate_basic_block(block_instrs, terminator, pc, self.pc);

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), ribit_decode::DecodeError> {
        if !self.jit.lookup_block(self.pc) {
            self.create_block()?;
        }

        self.jit.execute_basic_block(&mut self.pc, &mut self.xregs, &mut self.memory);

        Ok(())
    }

    #[must_use]
    pub fn to_host(&self) -> bool {
        self.test_ctx.as_ref().map_or(false, |it| {
            u32::from_le_bytes(self.memory[it.to_host as usize..][..4].try_into().unwrap()) != 0
        })
    }

    #[must_use]
    pub fn signature(&self) -> Option<&[u8]> {
        self.test_ctx
            .as_ref()
            .map(|it| &self.memory[it.begin_signature as usize..it.end_signature as usize])
    }
}

const fn __calc_misa() -> u32 {
    let mxl = 1;

    let extensions = [b'I', b'M', b'C'];

    let mut sum = mxl << (32 - 1);

    let mut i = 0;
    while i < extensions.len() {
        sum |= 1 << (extensions[i] - b'A');
        i += 1;
    }

    sum
}

const _MISA: u32 = __calc_misa();
const _MVENDORID: u32 = 0;
const _MARCHID: u32 = 0;
const _MIMPID: u32 = 0;
