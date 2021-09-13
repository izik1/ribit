#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_lossless,
    clippy::match_bool
)]

use std::convert::TryInto;

use goblin::elf::program_header::PT_LOAD;
use goblin::elf::Symtab;
use goblin::elf32;
use goblin::elf64::sym::STB_GLOBAL;
use goblin::strtab::Strtab;

struct TestAddrs {
    _from_host: u32,
    to_host: u32,
    begin_signature: u32,
    end_signature: u32,
}

impl TestAddrs {
    fn from_syms(syms: &Symtab, strs: &Strtab) -> Option<Self> {
        let mut to_host_addr: Option<u32> = None;
        let mut begin_signature_addr: Option<u32> = None;
        let mut end_signature_addr: Option<u32> = None;

        for sym in syms.iter() {
            if sym.st_bind() != STB_GLOBAL {
                continue;
            }

            if let Some(name) = strs.get_unsafe(sym.st_name) {
                log::info!("sym: {}", name);

                if name == "tohost" {
                    log::info!("found tohost: {:08x}", sym.st_value);
                    to_host_addr = Some(sym.st_value as u32);
                }

                if name == "begin_signature" {
                    log::info!("found begin_signature: {:08x}", sym.st_value);

                    begin_signature_addr = Some(sym.st_value as u32);
                }

                if name == "end_signature" {
                    log::info!("found end_signature: {:08x}", sym.st_value);

                    end_signature_addr = Some(sym.st_value as u32);
                }
            }
        }

        Some(Self {
            _from_host: 0,
            to_host: to_host_addr?,
            begin_signature: begin_signature_addr?,
            end_signature: end_signature_addr?,
        })
    }
}

pub struct ExecutionEngine {
    // xregs[0] is fixed to 0
    xregs: [u32; ribit_jit::XLEN],
    pc: u32,
    memory: Box<[u8]>,
    jit: ribit_jit::context::Runtime,
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
        let jit = ribit_jit::context::Runtime::new();

        Self { xregs, pc, memory, jit, test_ctx: None }
    }

    #[must_use]
    pub fn new_elf(program: &[u8]) -> Self {
        let elf = goblin::elf::Elf::parse(program).unwrap();
        assert!(!elf.is_64);
        assert!(elf.little_endian);
        assert!(elf.entry == 0x10000);

        let mut memory = vec![0; ribit_jit::MEMORY_SIZE as usize].into_boxed_slice();

        for header in &elf.program_headers {
            if header.p_type == PT_LOAD {
                let header = elf32::program_header::ProgramHeader::from(header.clone());
                memory[(header.p_paddr as usize)..][..(header.p_memsz as usize)].copy_from_slice(
                    &program[(header.p_offset as usize)..][..(header.p_memsz as usize)],
                );
            }
        }

        // elf.syms

        let xregs = [0; ribit_jit::XLEN];
        let pc = 0x10000;
        let jit = ribit_jit::context::Runtime::new();

        Self { xregs, pc, memory, jit, test_ctx: TestAddrs::from_syms(&elf.syms, &elf.strtab) }
    }

    fn parse_compressed(
        &mut self,
    ) -> Result<ribit_core::instruction::Info, ribit_core::DecodeError> {
        if self.pc as usize + 1 >= self.memory.len() {
            return Err(ribit_core::DecodeError::Other);
        }

        let instr = u16::from_le_bytes(
            self.memory[(self.pc as usize)..][..2]
                .try_into()
                .expect("bad slice size expected 2???"),
        );

        let instr = ribit_core::decode::compressed::decode_instruction(instr)?;
        let info = ribit_core::instruction::Info::new(instr, 2);
        self.pc += 2;
        Ok(info)
    }

    fn parse_instruction(
        &mut self,
    ) -> Result<ribit_core::instruction::Info, ribit_core::DecodeError> {
        if self.pc as usize + 3 >= self.memory.len() {
            return Err(ribit_core::DecodeError::Other);
        }

        let instr = u32::from_le_bytes(
            self.memory[(self.pc as usize)..][..4]
                .try_into()
                .expect("bad slice size expected 4???"),
        );

        // log::debug!("instruction bytes: {:08x}", instr.to_le());

        if instr & 0b11 == 0b11 {
            let instr = ribit_core::decode::instruction(instr)?;
            let info = ribit_core::instruction::Info::new(instr, 4);
            self.pc += 4;
            Ok(info)
        } else {
            self.parse_compressed()
        }
    }

    fn create_block(&mut self) -> Result<(), ribit_core::DecodeError> {
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
            } else {
                block_instrs.push(inst_info);
            }
        }

        self.pc = pc;
        self.jit.generate_basic_block(block_instrs, terminator, pc, self.pc);

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), ribit_core::DecodeError> {
        if !self.jit.lookup_block(self.pc) {
            self.create_block()?;
        }

        self.jit.execute_basic_block(&mut self.pc, &mut self.xregs, &mut self.memory);

        Ok(())
    }

    pub fn to_host(&self) -> bool {
        self.test_ctx.as_ref().map_or(false, |it| {
            u32::from_le_bytes(self.memory[it.to_host as usize..][..4].try_into().unwrap()) != 0
        })
    }

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
