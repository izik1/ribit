#![no_main]

use std::hint::black_box;
use std::mem;
use std::ops::ControlFlow;
use std::sync::OnceLock;

use libfuzzer_sys::arbitrary::{self, Arbitrary};
use libfuzzer_sys::{fuzz_mutator, fuzz_target};
use rand::distr::{Bernoulli, Distribution, StandardUniform};
use rand::seq::{IndexedMutRandom, SliceRandom};
use rand::{Rng, SeedableRng};
use ribit_core::instruction::{self, Instruction};
use ribit_core::{ReturnCode, Width, opcode, register};
use ribit_ssa::Block;
use ribit_ssa::opt::PassManager;
use ribit_ssa::opt::pass_manager::{InplacePass, Pass};

#[must_use]
const fn sign_extend<const BITS: u8>(value: u16) -> u16 {
    let mask = u16::BITS as u8 - BITS;
    ((value << mask).cast_signed() >> mask).cast_unsigned()
}

#[must_use]
const fn sign_extend_32<const BITS: u8>(value: u32) -> u32 {
    let mask = u32::BITS as u8 - BITS;
    ((value << mask).cast_signed() >> mask).cast_unsigned()
}

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

fn parse(mut data: &[u8]) -> Block {
    let mut ctx = ribit_ssa::lower::Context::new(1024, 2_u32.pow(20));

    while let Some((small, rest)) = data.split_at_checked(2) {
        let info = if small[0] & 0b11 == 0b11 {
            let Some((big, rest)) = data.split_at_checked(4) else {
                break;
            };

            data = rest;

            let raw = u32::from_le_bytes(big.try_into().unwrap());
            let instruction = ribit_decode::instruction(raw).unwrap_or(NOP);
            instruction::Info { instruction, len: 4 }
        } else {
            data = rest;

            let raw = u16::from_le_bytes(small.try_into().unwrap());
            let instruction = ribit_decode::compressed::decode_instruction(raw).unwrap_or(NOP);
            instruction::Info { instruction, len: 2 }
        };

        let info = match info.into_controlflow() {
            ControlFlow::Continue(info) => info,
            ControlFlow::Break(info) => {
                return ribit_ssa::lower::terminal(ctx, info.instruction, info.len);
            }
        };

        if let Instruction::R(instruction::R { opcode, .. }) = info.instruction
            && opcode.is_m_extension()
        {
            continue;
        }

        ribit_ssa::lower::non_terminal(&mut ctx, info.instruction, info.len);
    }

    let addr = ctx.add_pc_u32(4);
    ctx.ret_with_code(addr, ReturnCode::EBreak)
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
                0x04 => passes.push(Pass::LocalValueNumbering),
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
                }
            }

            (value, &[])
        };

        let (passes, _value) = {
            let point = value.iter().position(|it| *it == 0);

            point.map_or((value, [].as_slice()), |it| value.split_at(it))
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
    let passes = Passes::from_bytes(passes);
    let mut block = black_box(parse(instructions));

    PassManager::with_passes(passes.0).run(&mut block);

    ribit_ssa::assert_well_formed(&block);
});

fn trial_encode(instruction: &Instruction, len: u32) -> Result<(), ()> {
    match len {
        2 if ribit_encode::compressed::instruction(instruction).is_ok() => Ok(()),
        4 if ribit_encode::instruction(instruction).is_ok() => Ok(()),
        _ => Err(()),
    }
}

fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
    mutator(data, size, max_size, seed)
});

fn mutator(data: &mut [u8], size: usize, max_size: usize, seed: u32) -> usize {
    let mut rng = rand::rngs::StdRng::seed_from_u64(u64::from(seed));

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

    match rng.random_range::<usize, _>(..8) {
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
                    2 => 4,
                    4 => 2,
                    _ => unreachable!(),
                };

                let Ok(()) = trial_encode(&info.instruction, new_len) else {
                    continue;
                };

                info.len = new_len;

                if new_len == 2 && ratio_1_8.sample(&mut rng) {
                    // randomly either pre-insert or post insert the nop, potentially do interesting things with the alignment.
                    let after = usize::from(rng.random::<bool>());

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

        // mutate an instruction randomly.
        // - replace an immediate with some other immediate.
        // - replace a register with some other register.
        // - swap an opcode with one of the same class.
        // - swap arguments.
        // and so on.
        6 => {
            let max_replacements = rng.random_range(1..4_u8);
            let mut replacements = 0;
            for _ in 0..5 {
                let idx = rng.random_range(..instructions.len());
                let info = &mut instructions[idx];
                match mutate_instruction(info, &mut rng) {
                    ControlFlow::Continue(()) => continue,
                    ControlFlow::Break(new) => {
                        *info = new;

                        replacements += 1;
                        if replacements >= max_replacements {
                            break;
                        }
                    }
                }
            }
        }

        // Nothing (Just let clean up)
        _ => {}
    }

    match rng.random_range(..=20_usize) {
        // pick a pass, add it somewhere.
        0 => {
            let pass = match rng.random_range(1..=3_u8) {
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
                Pass::LocalValueNumbering,
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
                Pass::LocalValueNumbering => 0x04,
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

fn write_instructions(mut data: &mut [u8], instructions: Vec<instruction::Info>) -> &mut [u8] {
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
                            imm: (lower as i16).cast_unsigned(),
                            rs1,
                            rd: Some(rd),
                            opcode: opcode::I::ADDI,
                        }),
                        len: 4,
                    },
                );
            }
        }
        // move a value from one register to another
        1 => {
            let rd = rng.random_range(1..=31);

            let rd = register::RiscV::with_u8(rd).unwrap();
            let rs2 = rng.random::<Register>().0;

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
    }
}

struct Register(Option<register::RiscV>);

impl Distribution<Register> for StandardUniform {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Register {
        let register = rng.next_u32() as u8 & 0b1_1111;

        Register(register::RiscV::with_u8(register))
    }
}

fn mutate_instruction<R: rand::Rng + ?Sized>(
    info: &instruction::Info,
    rng: &mut R,
) -> ControlFlow<instruction::Info> {
    // returns a `I-type` instruction for `I` and `I` subtype instructions (loads and jumps)
    fn random_i_instruction_op<R: rand::Rng + ?Sized>(
        rd: Option<register::RiscV>,
        rs1: Option<register::RiscV>,
        imm: u16,
        rng: &mut R,
    ) -> Instruction {
        match rng.random_range(..15_u8) {
            0 => instruction::I::new(imm, rs1, rd, opcode::I::ADDI).into(),
            1 => instruction::I::new(imm, rs1, rd, opcode::I::ANDI).into(),
            2 => instruction::I::new(imm, rs1, rd, opcode::I::ORI).into(),
            3 => instruction::I::new(imm, rs1, rd, opcode::I::SLLI).into(),
            4 => instruction::I::new(imm, rs1, rd, opcode::I::SRLI).into(),
            5 => instruction::I::new(imm, rs1, rd, opcode::I::SRAI).into(),
            6 => instruction::I::new(imm, rs1, rd, opcode::I::SICond(opcode::SCmp::Lt)).into(),
            7 => instruction::I::new(imm, rs1, rd, opcode::I::SICond(opcode::SCmp::Ltu)).into(),
            8 => instruction::IJump::new(imm, rs1, rd, opcode::IJump::JALR).into(),
            9 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::FENCE).into(),
            10 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::LD(Width::Byte)).into(),
            11 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::LD(Width::Word)).into(),
            12 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::LD(Width::DWord)).into(),
            13 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::LDU(Width::Byte)).into(),
            14 => instruction::IMem::new(imm, rs1, rd, opcode::IMem::LDU(Width::Word)).into(),
            _ => unreachable!(),
        }
    }

    let encode = |new| match trial_encode(&new, 2) {
        Ok(()) => ControlFlow::Break(instruction::Info { instruction: new, len: 2 }),
        // try expanding just in case.
        Err(()) => match trial_encode(&new, 4) {
            Ok(()) => ControlFlow::Break(instruction::Info { instruction: new, len: 4 }),
            Err(()) => ControlFlow::Continue(()),
        },
    };

    match &info.instruction {
        Instruction::R(r) => {
            // 4 real options here.
            let new = match rng.random_range(..4_u8) {
                // swap opcodes.
                0 => {
                    let new_op = match rng.random_range(..10_u8) {
                        0 => opcode::R::ADD,
                        1 => opcode::R::AND,
                        2 => opcode::R::OR,
                        3 => opcode::R::SLL,
                        4 => opcode::R::SRL,
                        5 => opcode::R::SRA,
                        6 => opcode::R::SUB,
                        7 => opcode::R::XOR,
                        8 => opcode::R::SCond(opcode::SCmp::Lt),
                        9 => opcode::R::SCond(opcode::SCmp::Ltu),
                        _ => unreachable!(),
                    };

                    instruction::R { opcode: new_op, ..*r }
                }

                // shuffle registers.
                1 => {
                    let mut registers = [r.rd, r.rs1, r.rs2];

                    registers.shuffle(rng);

                    let [rd, rs1, rs2] = registers;

                    instruction::R { rd, rs1, rs2, ..*r }
                }
                // replace one register with another.
                2 => {
                    let mut registers = [r.rd, r.rs1, r.rs2];
                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rd, rs1, rs2] = registers;

                    instruction::R { rd, rs1, rs2, ..*r }
                }
                // replace all registers with others.
                3 => {
                    let [rd, rs1, rs2] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::R { rd, rs1, rs2, ..*r }
                }
                _ => unreachable!(),
            };

            encode(new.into())
        }
        Instruction::I(i) => {
            let new = match rng.random_range(..5_u8) {
                // swap opcode (including across I sub-types)
                0 => random_i_instruction_op(i.rd, i.rs1, i.imm, rng),
                // replace imm
                1 => instruction::I { imm: sign_extend::<12>(rng.random_range(..(1 << 12))), ..*i }
                    .into(),

                // swap rd and rs1
                2 => instruction::I { rd: i.rs1, rs1: i.rd, ..*i }.into(),
                // replace rd or rs1
                3 => {
                    let mut registers = [i.rd, i.rs1];
                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rd, rs1] = registers;

                    instruction::I { rd, rs1, ..*i }.into()
                }

                // replace both rd and rs1
                4 => {
                    let [rd, rs1] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::I { rd, rs1, ..*i }.into()
                }

                _ => unreachable!(),
            };

            encode(new)
        }
        Instruction::IJump(i) => {
            let new = match 0_u8 {
                // swap opcode (including across I sub-types)
                0 => random_i_instruction_op(i.rd, i.rs1, i.imm, rng),
                // replace imm
                1 => instruction::IJump {
                    imm: sign_extend::<12>(rng.random_range(..(1 << 12))),
                    ..*i
                }
                .into(),

                // swap rd and rs1
                2 => instruction::IJump { rd: i.rs1, rs1: i.rd, ..*i }.into(),
                // replace rd or rs1
                3 => {
                    let mut registers = [i.rd, i.rs1];
                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rd, rs1] = registers;

                    instruction::IJump { rd, rs1, ..*i }.into()
                }

                // replace both rd and rs1
                4 => {
                    let [rd, rs1] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::IJump { rd, rs1, ..*i }.into()
                }

                _ => unreachable!(),
            };

            encode(new)
        }
        Instruction::IMem(i) => {
            let new = match 0_u8 {
                // swap opcode (including across I sub-types)
                0 => random_i_instruction_op(i.rd, i.rs1, i.imm, rng),
                // replace imm
                1 => instruction::IMem {
                    imm: sign_extend::<12>(rng.random_range(..(1 << 12))),
                    ..*i
                }
                .into(),

                // swap rd and rs1
                2 => instruction::IMem { rd: i.rs1, rs1: i.rd, ..*i }.into(),
                // replace rd or rs1
                3 => {
                    let mut registers = [i.rd, i.rs1];
                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rd, rs1] = registers;

                    instruction::IMem { rd, rs1, ..*i }.into()
                }

                // replace both rd and rs1
                4 => {
                    let [rd, rs1] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::IMem { rd, rs1, ..*i }.into()
                }

                _ => unreachable!(),
            };

            encode(new)
        }
        Instruction::S(s) => {
            let new = match rng.random_range(..5_u8) {
                // swap width
                0 => instruction::S {
                    width: match rng.random_range(..3_u8) {
                        0 => Width::Byte,
                        1 => Width::Word,
                        2 => Width::DWord,
                        _ => unreachable!(),
                    },
                    ..*s
                },
                // replace imm
                1 => instruction::S { imm: sign_extend::<12>(rng.random_range(..(1 << 12))), ..*s },
                // swap rs1 and rs2
                2 => instruction::S { rs1: s.rs2, rs2: s.rs1, ..*s },
                // replace rs1 or rs2
                3 => {
                    let mut registers = [s.rs1, s.rs2];
                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rs1, rs2] = registers;

                    instruction::S { rs1, rs2, ..*s }
                }

                // replace both rs1 and rs2
                4 => {
                    let [rs1, rs2] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::S { rs1, rs2, ..*s }
                }

                _ => unreachable!(),
            };

            encode(new.into())
        }
        Instruction::B(b) => {
            let new = match rng.random_range(..4_u8) {
                0 => instruction::B {
                    cmp_mode: match rng.random_range(..6_u8) {
                        0 => opcode::Cmp::Eq,
                        1 => opcode::Cmp::Ne,
                        2 => opcode::Cmp::Lt,
                        3 => opcode::Cmp::Ltu,
                        4 => opcode::Cmp::Ge,
                        5 => opcode::Cmp::Geu,
                        _ => unreachable!(),
                    },
                    ..*b
                },

                // replace imm
                1 => instruction::B {
                    imm: sign_extend::<12>(rng.random_range(..(1 << 12))) << 1,
                    ..*b
                },

                // swap rs1 and rs2
                2 => instruction::B { rs1: b.rs2, rs2: b.rs1, ..*b },

                // replace rs1 or rs2
                3 => {
                    let mut registers = [b.rs1, b.rs2];

                    *registers.choose_mut(rng).unwrap() = rng.random::<Register>().0;

                    let [rs1, rs2] = registers;

                    instruction::B { rs1, rs2, ..*b }
                }

                // replace both rs1 and rs2
                4 => {
                    let [rs1, rs2] = std::array::from_fn(|_| rng.random::<Register>().0);

                    instruction::B { rs1, rs2, ..*b }
                }

                _ => unreachable!(),
            };

            encode(new.into())
        }
        Instruction::U(u) => {
            let new = match rng.random_range(..3_u8) {
                // swap opcode
                0 => match u.opcode {
                    opcode::U::AUIPC => instruction::U { opcode: opcode::U::LUI, ..*u },
                    opcode::U::LUI => instruction::U { opcode: opcode::U::AUIPC, ..*u },
                },
                // replace imm
                1 => {
                    let imm = rng.next_u32() & !0xfff;
                    instruction::U { imm, ..*u }
                }

                // replace rd
                2 => {
                    let rd = rng.random::<Register>().0;

                    instruction::U { rd, ..*u }
                }
                _ => unreachable!(),
            };

            encode(new.into())
        }
        Instruction::J(j) => {
            // few options
            let j = match rng.random_range(..2_u8) {
                // change around dest register
                0 => instruction::J { rd: rng.random::<Register>().0, ..*j },
                // change around the immediate.
                1 => {
                    // even integers in the range +-1MiB
                    let imm = sign_extend_32::<20>(rng.random_range(..(1 << 20))) << 1;
                    instruction::J { imm, ..*j }
                }
                _ => unreachable!(),
            };

            encode(Instruction::J(j))
        }

        // yeah, not exactly rocket science here.
        Instruction::Sys(sys) => {
            let opcode = match sys.opcode {
                opcode::RSys::ECALL => opcode::RSys::EBREAK,
                opcode::RSys::EBREAK => opcode::RSys::ECALL,
            };

            encode(Instruction::Sys(instruction::Sys { opcode }))
        }
    }
}
