#![no_main]

use std::hint::black_box;
use std::mem;
use std::sync::OnceLock;

use libfuzzer_sys::arbitrary::{self, Arbitrary};
use libfuzzer_sys::{fuzz_mutator, fuzz_target};
use rand::distr::{Bernoulli, Distribution};
use rand::{Rng, SeedableRng};
use ribit_core::instruction::{self, Instruction};
use ribit_core::{opcode, register};
use ribit_ssa::opt::PassManager;
use ribit_ssa::opt::pass_manager::{InplacePass, Pass};

static RATIO_1_8: OnceLock<Bernoulli> = OnceLock::new();
static RATIO_1_4: OnceLock<Bernoulli> = OnceLock::new();

const NOP: Instruction =
    Instruction::I(instruction::I { imm: 0, rs1: None, rd: None, opcode: opcode::I::ADDI });

const EBREAK: Instruction = Instruction::Sys(instruction::Sys { opcode: opcode::RSys::EBREAK });

fn parse_strict(mut data: &[u8]) -> Result<Vec<instruction::Info>, ()> {
    let mut instructions = Vec::with_capacity(data.len() / 2);

    while let Some((small, rest)) = data.split_at_checked(2) {
        let info = if small[0] & 0b11 == 0b11 {
            let Some((big, rest)) = data.split_at_checked(4) else {
                break;
            };

            data = rest;

            let raw = u32::from_le_bytes(big.try_into().unwrap());
            let instruction = ribit_decode::instruction(raw).map_err(drop)?;
            instruction::Info { instruction, len: 4 }
        } else {
            data = rest;

            let raw = u16::from_le_bytes(small.try_into().unwrap());
            let instruction = ribit_decode::compressed::decode_instruction(raw).map_err(drop)?;
            instruction::Info { instruction, len: 2 }
        };
        instructions.push(info);
    }

    Ok(instructions)
}

fn parse(mut data: &[u8]) -> (Vec<instruction::Info>, instruction::Info) {
    let mut instructions = Vec::with_capacity(data.len() / 2);

    while let Some((small, rest)) = data.split_at_checked(2) {
        let info = if small[0] & 0b11 == 0b11 {
            let Some((big, rest)) = data.split_at_checked(4) else {
                break;
            };

            data = rest;

            let raw = u32::from_le_bytes(big.try_into().unwrap());
            let instruction = ribit_decode::instruction(raw).unwrap_or_else(|_| NOP);
            instruction::Info { instruction, len: 4 }
        } else {
            data = rest;

            let raw = u16::from_le_bytes(small.try_into().unwrap());
            let instruction =
                ribit_decode::compressed::decode_instruction(raw).unwrap_or_else(|_| NOP);
            instruction::Info { instruction, len: 2 }
        };

        if info.instruction.is_terminator() {
            return (instructions, info);
        }

        instructions.push(info);
        //
    }

    (instructions, instruction::Info { instruction: EBREAK, len: 4 })
}

#[derive(Debug)]
struct Passes(Vec<Pass>);

impl Passes {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut passes = Vec::with_capacity(bytes.len());

        for byte in bytes {
            match byte {
                0x00 => break,
                0x01 => passes.push(Pass::ConstProp),
                0x02 => passes.push(Pass::DeadInstructionElimination),
                0x03 => passes.push(Pass::RegisterWritebackShrinking),
                _ => {}
            }
        }

        Self(passes)
    }
}

impl<'a> Arbitrary<'a> for Passes {
    fn arbitrary(
        u: &mut libfuzzer_sys::arbitrary::Unstructured<'a>,
    ) -> libfuzzer_sys::arbitrary::Result<Self> {
        let size = match u.arbitrary_len::<u8>() {
            Ok(size) => size,
            Err(arbitrary::Error::NotEnoughData) => 0,
            Err(e) => return Err(e),
        };

        u.bytes(size).map(Self::from_bytes)
    }

    fn arbitrary_take_rest(u: arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self::from_bytes(u.take_rest()))
    }
}

#[derive(Debug)]
struct FuzzData<'a> {
    instructions: &'a [u8],
    passes: &'a [u8],
}

impl<'a> From<&'a [u8]> for FuzzData<'a> {
    fn from(value: &'a [u8]) -> Self {
        if value.len() < 2 {
            return Self { instructions: &[], passes: &[] };
        }

        let (instructions, value) = 'split_point: {
            let mut data = value;
            while let Some((small, rest)) = data.split_at_checked(2) {
                if small[0] & 0b11 == 0b11 {
                    let Some((big, rest)) = data.split_at_checked(4) else {
                        break;
                    };

                    let raw = u32::from_le_bytes(big.try_into().unwrap());

                    if raw == 0 || raw == u32::MAX {
                        let len = value.len() - rest.len() - big.len();

                        break 'split_point (&value[..len], rest);
                    }

                    data = rest;
                } else {
                    let raw = u16::from_le_bytes(small.try_into().unwrap());

                    if raw == 0 || raw == u16::MAX {
                        let len = value.len() - rest.len() - small.len();

                        break 'split_point (&value[..len], rest);
                    }

                    data = rest;
                };
            }

            (value, &[])
        };

        let (passes, _value) = {
            let point = value.iter().position(|it| *it == 0);

            point.map(|it| value.split_at(it)).unwrap_or((value, &[]))
        };

        Self { instructions, passes }
    }
}

impl<'a> Arbitrary<'a> for FuzzData<'a> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let len = u.arbitrary_len::<u8>()?;

        Ok(Self::from(u.bytes(len)?))
    }

    fn arbitrary_take_rest(u: arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self::from(u.take_rest()))
    }

    #[inline]
    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        (0, None)
    }
}

fuzz_target!(
    init: {
        RATIO_1_8.set(Bernoulli::from_ratio(1, 8).unwrap()).unwrap();
        RATIO_1_4.set(Bernoulli::from_ratio(1, 4).unwrap()).unwrap();
    },
    |data: FuzzData<'_>| {
    let FuzzData { instructions, passes } = data;
    let (instrutctions, term) = parse(instructions);
    let passes = Passes::from_bytes(passes);

    let mut block = black_box(lower(instrutctions, term));

    PassManager::with_passes(passes.0).run(&mut block);
    let _ = black_box(&block);
});

fn lower(instructions: Vec<instruction::Info>, term: instruction::Info) -> ribit_ssa::Block {
    let mut ctx = ribit_ssa::lower::Context::new(1024, 2_u32.pow(20));

    for instruction::Info { instruction, len } in instructions {
        assert!(!instruction.is_terminator());

        match instruction {
            Instruction::R(instruction::R {
                rs1: _,
                rs2: _,
                rd: _,
                opcode:
                    opcode::R::MUL
                    | opcode::R::MULH
                    | opcode::R::MULHSU
                    | opcode::R::MULHU
                    | opcode::R::DIV
                    | opcode::R::DIVU
                    | opcode::R::REM
                    | opcode::R::REMU,
            }) => continue,
            _ => {}
        }

        ribit_ssa::lower::non_terminal(&mut ctx, instruction, len);
    }

    assert!(term.instruction.is_terminator());

    ribit_ssa::lower::terminal(ctx, term.instruction, term.len)
}

fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
    mutator(data, size, max_size, seed)
});

fn mutator(data: &mut [u8], size: usize, max_size: usize, seed: u32) -> usize {
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed as u64);

    let ratio_1_4 = RATIO_1_4.get().unwrap();
    let ratio_1_8 = RATIO_1_8.get().unwrap();

    if ratio_1_4.sample(&mut rng) {
        return libfuzzer_sys::fuzzer_mutate(data, size, max_size);
    }

    let tmp = FuzzData::from(&data[..size]);
    let mut passes = Passes::from_bytes(tmp.passes);
    let instructions = {
        let instructions_len = tmp.instructions.len();
        &mut data[..instructions_len]
    };

    let Ok(mut instructions) = parse_strict(instructions) else {
        return libfuzzer_sys::fuzzer_mutate(data, size, max_size);
    };

    match rng.random_range::<usize, _>(..7) {
        _ if instructions.is_empty() => {}
        // randomly delete an instruction (we have the power of being able to do it on aligned boundaries)
        0 => {
            let idx = rng.random_range(..instructions.len());

            instructions.remove(idx);
        }

        // compress or decompress a random instruction (try up to 5 times, but only change 1)
        1 => {
            for _ in 0..5 {
                let idx = rng.random_range(..instructions.len());

                let info = &mut instructions[idx];

                let new_len = match info.len {
                    2 if ribit_encode::instruction(&info.instruction).is_ok() => 4,
                    4 if ribit_encode::compressed::instruction(&info.instruction).is_ok() => 2,
                    _ => continue,
                };

                info.len = new_len;

                if new_len == 2 && ratio_1_8.sample(&mut rng) {
                    // randomly either pre-insert or post insert the nop, potentially do interesting things with the alignment.
                    let after = rng.random::<bool>() as usize;

                    instructions
                        .insert(idx + after, instruction::Info { instruction: NOP, len: 2 });
                }

                break;
            }
        }

        // randomly nop an instruction (get rid of it without changing the PC downstream)
        2 => {
            // ADDI X0, X0, 0 is the standard nop in rv32 instructions, but RVC has a special nop... that's basically the same thing, but technically is `C.nop`.

            let idx = rng.random_range(..instructions.len());

            let info = &mut instructions[idx];

            info.instruction = NOP;
        }

        // randomly swap two instructions
        // note that unlike compressing/decompressing, this will try infinite times, after all, it's statistically guaranteed to work very quickly.
        3 if instructions.len() >= 2 => {
            let (idx1, idx2) = loop {
                let idx1 = rng.random_range(..instructions.len());
                let idx2 = rng.random_range(..instructions.len());

                if idx1 != idx2 {
                    break (idx1, idx2);
                }
            };

            instructions.swap(idx1, idx2);
        }

        // generate new instructions (attempt to add new interesting behaviors)
        4 if size < max_size => {
            instruction_neogenesis(&mut instructions, &mut rng);
        }

        // remove all instructions post a terminal one (reduce)
        5 => {
            if let Some(idx) = instructions.iter().position(|it| it.instruction.is_terminator()) {
                instructions.truncate(idx + 1);
            }
        }

        // Nothing (Just let clean up)
        _ => {}
    }

    match rng.random_range(..=20_usize) {
        // pick a pass, add it somewhere.
        0 => {
            let pass = match rng.random_range(1..=3) {
                1 => Pass::ConstProp,
                2 => Pass::DeadInstructionElimination,
                3 => Pass::RegisterWritebackShrinking,
                _ => unreachable!(),
            };

            let idx = rng.random_range(..=passes.0.len());

            passes.0.insert(idx, pass);
        }

        // pick a pass, remove it
        1 if !passes.0.is_empty() => {
            let index = rng.random_range(..passes.0.len());

            passes.0.remove(index);
        }

        // set the passes to exactly `PassManager::optimized`'s passes
        2 | 3 => {
            passes.0 = vec![
                Pass::ConstProp,
                Pass::DeadInstructionElimination,
                Pass::RegisterWritebackShrinking,
            ];
        }

        // clear all passes (make unoptimized)
        4 | 5 => {
            passes.0.clear();
        }

        // nothing
        _ => {}
    }

    let new_size = {
        let data = &mut data[..max_size];
        let data = write_instructions(data, instructions);

        let data = &mut *data;

        if passes.0.is_empty() {
            return max_size - data.len();
        }

        let mut data = {
            let Some((start, rest)) = data.split_at_mut_checked(2) else {
                return max_size - data.len();
            };

            start.copy_from_slice(0_u16.to_le_bytes().as_slice());
            rest
        };

        for pass in passes.0 {
            const LEN: usize = std::mem::size_of::<u8>();
            let Some(start) = data.get_mut(..LEN) else {
                break;
            };

            let value: u8 = match pass {
                Pass::Inplace(_) => unreachable!(),
                Pass::ConstProp => 0x01,
                Pass::DeadInstructionElimination => 0x02,
                Pass::RegisterWritebackShrinking => 0x03,
            };

            start.copy_from_slice(&value.to_be_bytes());
            data = &mut data[LEN..];
        }

        max_size - data.len()
    };

    if ratio_1_8.sample(&mut rng) {
        libfuzzer_sys::fuzzer_mutate(data, new_size, max_size)
    } else {
        new_size
    }
}

fn write_instructions<'a>(
    mut data: &'a mut [u8],
    instructions: Vec<instruction::Info>,
) -> &'a mut [u8] {
    for info in instructions {
        let len = info.len as usize;
        let Some(start) = data.get_mut(..len) else {
            break;
        };

        match info.len {
            2 => {
                let bytes =
                    ribit_encode::compressed::instruction(&info.instruction).unwrap().to_le_bytes();
                start.copy_from_slice(&bytes);
            }

            4 => {
                let bytes = ribit_encode::instruction(&info.instruction).unwrap().to_le_bytes();
                start.copy_from_slice(&bytes);
            }
            _ => unreachable!(),
        }

        data = &mut data[len..];
    }

    data
}

fn instruction_neogenesis<R: rand::Rng>(instructions: &mut Vec<instruction::Info>, rng: &mut R) {
    #[must_use]
    const fn sign_extend_32<const BITS: u8>(value: u32) -> u32 {
        let mask = u32::BITS - BITS as u32;
        (((value << mask) as i32) >> mask) as u32
    }

    let mut idx = rng.random_range(..=instructions.len());

    match rng.random_range(..3_u8) {
        // put a constant in a register
        0 => {
            let value: u32 = rng.random();

            let rd = rng.random_range(1..=31);
            let rd = register::RiscV::with_u8(rd).unwrap();

            if value == 0 {
                // if the value is zero we just `ADDI, rd, x0(0)`
                let instruction = Instruction::I(instruction::I {
                    imm: 0,
                    rs1: None,
                    rd: Some(rd),
                    opcode: opcode::I::ADDI,
                });

                instructions.insert(idx, instruction::Info { instruction, len: 4 });

                return;
            }

            // this is more annoying than it sounds like
            // we have a lower immediate (12 bits) and an upper immediate (20 bits)
            // but the lower immediate is sign extended, not zero extended
            // so we need to `LUI` the upper immediate then `ADDI` the lower immediate, but the upper immediate's value actually needs to be different depending on the high bit of the lower immediate's
            // first we need to split out the lower immediate
            let lower = value & 0xfff;
            // reinterpret both values as as signed (by sign-extending out the lower immediate)
            let lower = sign_extend_32::<12>(lower).cast_signed();
            let value = value.cast_signed();
            // and getting the upper immediate by subtracting the lower immediate (with wrapping)
            let upper = value.wrapping_sub(lower).cast_unsigned() & !0xfff;

            // if upper is zero we can just emit the ADDI (we already know lower and upper can't both be zero), otherwise we'll need the LUI.
            let rs1 = match upper == 0 {
                true => None,
                false => Some(rd),
            };

            // now, we need to the LUI, the ADDI, or both, depending on the values.

            if upper != 0 {
                let new_idx = idx + 1;
                instructions.insert(
                    mem::replace(&mut idx, new_idx),
                    instruction::Info {
                        instruction: Instruction::U(instruction::U {
                            imm: upper,
                            rd: Some(rd),
                            opcode: opcode::U::LUI,
                        }),
                        len: 4,
                    },
                );
            }

            if lower != 0 {
                instructions.insert(
                    idx,
                    instruction::Info {
                        instruction: Instruction::I(instruction::I {
                            imm: lower as i16 as u16,
                            rs1,
                            rd: Some(rd),
                            opcode: opcode::I::ADDI,
                        }),
                        len: 4,
                    },
                )
            }
        }
        // move a value from one register to another
        1 => {
            let rd = rng.random_range(1..=31);
            let rs2 = rng.random_range(0..=32);

            let rd = register::RiscV::with_u8(rd).unwrap();
            let rs2 = register::RiscV::with_u8(rs2);

            instructions.insert(
                idx,
                instruction::Info {
                    instruction: Instruction::R(instruction::R {
                        rs1: None,
                        rs2,
                        rd: Some(rd),
                        opcode: opcode::R::ADD,
                    }),
                    len: 2,
                },
            );
        }
        2 => {
            instructions.insert(idx, instruction::Info { instruction: EBREAK, len: 2 });
        }
        _ => unreachable!(),
    };
}
