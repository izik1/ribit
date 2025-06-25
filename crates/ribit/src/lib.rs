#![forbid(unsafe_code)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![warn(clippy::must_use_candidate)]

use core::fmt;
use std::ops::ControlFlow;

use ribit_core::instruction::{self, NonTerminal, Terminal};
use ribit_core::register;
use xmas_elf::ElfFile;
use xmas_elf::header::Data;
use xmas_elf::sections::SectionData;
use xmas_elf::symbol_table::{Binding, Entry};

#[derive(Eq, PartialEq)]
struct LowerHex<T: fmt::LowerHex>(T);

impl<T: fmt::LowerHex> fmt::Display for LowerHex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#0width$x}", self.0, width = const { std::mem::size_of::<T>() * 2 + 2 })
    }
}

impl<T: fmt::LowerHex> fmt::Debug for LowerHex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

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

        tracing::info!(sym = name);

        let value = sym.value();

        match name {
            "tohost" => {
                tracing::info!(to_host=%LowerHex(value), "found tohost");
                ctx.to_host = Some(value as u32);
            }

            "begin_signature" => {
                tracing::info!(begin_signature=%LowerHex(value), "found begin_signaturev");
                ctx.begin_signature = Some(value as u32);
            }

            "end_signature" => {
                tracing::info!(end_signature=%LowerHex(value), "found end_signaturev");
                ctx.end_signature = Some(value as u32);
            }

            _ => {}
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

fn parse_compressed(
    pc: &mut u32,
    memory: &[u8],
) -> Result<instruction::Info, ribit_decode::DecodeError> {
    if *pc as usize + 1 >= memory.len() {
        return Err(ribit_decode::DecodeError::Other);
    }

    let instr = u16::from_le_bytes(
        memory[(*pc as usize)..][..2].try_into().expect("bad slice size expected 2???"),
    );

    tracing::debug!("instruction: {instr:016b}");

    let instr = ribit_decode::compressed::decode_instruction(instr)?;
    let info = instruction::Info::new(instr, 2);
    *pc += 2;
    Ok(info)
}

fn parse_instruction(
    pc: &mut u32,
    memory: &[u8],
) -> Result<instruction::Info, ribit_decode::DecodeError> {
    if *pc as usize + 3 >= memory.len() {
        return Err(ribit_decode::DecodeError::Other);
    }

    let instr = u32::from_le_bytes(
        memory[(*pc as usize)..][..4].try_into().expect("bad slice size expected 4???"),
    );

    // tracing::debug!("instruction bytes: {:08x}", instr.to_le());

    if instr & 0b11 == 0b11 {
        tracing::debug!("instruction: {instr:032b}");
        let instr = ribit_decode::instruction(instr)?;
        let info = instruction::Info::new(instr, 4);
        *pc += 4;
        Ok(info)
    } else {
        parse_compressed(pc, memory)
    }
}

fn decode_block(
    start_pc: u32,
    memory: &[u8],
) -> Result<
    (Vec<instruction::Info<NonTerminal>>, instruction::Info<Terminal>, u32),
    ribit_decode::DecodeError,
> {
    let mut current_pc = start_pc;

    let mut instructions = Vec::new();
    let terminator = loop {
        let _span =
            tracing::debug_span!("decode_instruction", pc = %LowerHex(current_pc)).entered();
        let info = parse_instruction(&mut current_pc, memory)?;

        tracing::debug!(
            instruction = %ribit_core::disassemble::FmtInstruction::from_info(&info)
        );

        instructions.push(match info.into_controlflow() {
            ControlFlow::Break(info) => break info,
            ControlFlow::Continue(info) => info,
        });
    };

    let end_pc = current_pc;

    Ok((instructions, terminator, end_pc))
}

pub struct ExecutionEngine {
    xregs: register::File<u32>,
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

        let xregs = register::File([0; 31]);
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

        // fill with `AA`s instead of zeros to hopefully failings to zero memory red-handed.
        let mut memory = vec![0xaa; ribit_jit::MEMORY_SIZE as usize].into_boxed_slice();
        for header in elf.program_iter() {
            if header.get_type().unwrap() == xmas_elf::program::Type::Load {
                // the memory segment we're loading could be bigger than is stored in the program,
                let memory_segment = &mut memory[(header.physical_addr() as usize)..]
                    [..(header.mem_size() as usize)];

                let (load, zeroize) = memory_segment.split_at_mut(header.file_size() as usize);

                load.copy_from_slice(
                    &program[(header.offset() as usize)..][..(header.file_size() as usize)],
                );

                zeroize.fill(0);
            }
        }

        let xregs = register::File([0; 31]);
        let pc = entry;
        let jit = ribit_jit::DefaultRuntime::new();

        Self { xregs, pc, memory, jit, test_ctx: TestAddrs::from_elf(&elf) }
    }

    pub fn run(&mut self) -> Result<(), ribit_decode::DecodeError> {
        self.jit.execute_basic_block(&mut self.pc, &mut self.xregs, &mut self.memory, decode_block)
    }

    #[must_use]
    pub fn to_host(&self) -> bool {
        self.test_ctx.as_ref().is_some_and(|it| {
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
