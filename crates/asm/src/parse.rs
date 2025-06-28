use ribit_core::instruction::Instruction;
use ribit_core::register;

use crate::parse::word::Word;

mod instruction;
mod word;

#[cfg(test)]
mod tests;

#[must_use]
const fn sign_extend(value: u16, data_bits: u8) -> u16 {
    let mask = u16::BITS - (data_bits as u32);
    ((value << mask).cast_signed() >> mask).cast_unsigned()
}

#[must_use]
const fn sign_extend_32(value: u32, data_bits: u8) -> u32 {
    let mask = u32::BITS - (data_bits as u32);
    ((value << mask).cast_signed() >> mask).cast_unsigned()
}

// v0.0.1 we don't support fancy things like labels.
// or error locations for that matter.
#[derive(Debug)]
struct ParseContext {
    instructions: Vec<ribit_core::instruction::Info>,
    errors: Vec<String>,
    supports_compressed: bool,
}

impl ParseContext {
    fn consume<T>(&mut self, res: Result<T, String>) -> Option<T> {
        match res {
            Ok(it) => Some(it),
            Err(e) => {
                self.errors.push(e);
                None
            }
        }
    }

    fn push32<I: Into<Instruction>>(&mut self, instruction: I) {
        self.instructions
            .push(ribit_core::instruction::Info { instruction: instruction.into(), len: 4 });
    }

    fn push16<I: Into<Instruction>>(&mut self, instruction: I) {
        self.instructions
            .push(ribit_core::instruction::Info { instruction: instruction.into(), len: 2 });
    }
}

#[derive(Debug)]
pub struct ParseOutput {
    pub instructions: Vec<ribit_core::instruction::Info>,
    pub errors: Vec<String>,
}

impl From<ParseContext> for ParseOutput {
    fn from(context: ParseContext) -> Self {
        Self { instructions: context.instructions, errors: context.errors }
    }
}

#[must_use]
pub fn tokenize(input: &str, enable_compressed: bool) -> ParseOutput {
    let mut output = ParseContext {
        instructions: Vec::new(),
        errors: Vec::new(),
        supports_compressed: enable_compressed,
    };

    for line in input.lines() {
        let line = line.split_once(';').map_or(line, |(line, _comment)| line);
        let line = line.trim();

        tokenize_instruction(&mut output, line);
    }

    output.into()
}

fn tokenize_instruction(context: &mut ParseContext, line: &str) {
    if line.is_empty() {
        return;
    }

    let (full_op, args) = line.split_once(' ').unwrap_or((line, ""));

    // any other prefixes as well
    let mut has_compressed = false;
    for part in full_op.split('.') {
        if part.eq_ignore_ascii_case("C") {
            has_compressed = true;
        }
    }

    let op = full_op.split('.').next_back().unwrap();

    let mut args: Vec<_> = args.split(',').map(str::trim).collect();

    if args.last() == Some(&"") {
        args.pop();
    }

    let args = args;

    let instruction_matched = 'check: {
        let Ok(op) = op.parse::<Word>() else {
            break 'check false;
        };

        match has_compressed {
            true => instruction::compressed(context, op, full_op, &args),
            false => parse_32(context, op, full_op, &args),
        }
    };

    if !instruction_matched {
        context.errors.push(format!("Unknown instruction `{full_op}`"));
    }
}

fn parse_32(context: &mut ParseContext, op: Word, full_op: &str, args: &[&str]) -> bool {
    match op {
        Word::Op(instruction) => match instruction {
            Instruction::R(op) => instruction::r_32(context, op, full_op, args),
            Instruction::I(op) => instruction::i_32(context, op, full_op, args),
            Instruction::IJump(op) => instruction::ijump_32(context, op, full_op, args),
            Instruction::IMem(op) => instruction::imem_32(context, op, full_op, args),
            Instruction::S(op) => instruction::s_32(context, op, full_op, args),
            Instruction::B(op) => instruction::b_32(context, op, full_op, args),
            Instruction::U(op) => instruction::u_32(context, op, full_op, args),
            Instruction::J(op) => instruction::j_32(context, op, full_op, args),
            Instruction::Sys(op) => instruction::sys_32(context, op, full_op, args),
        },
        Word::Ret => instruction::ret(context, full_op, args),
        Word::Nop => instruction::nop(context, full_op, args),
        Word::J | Word::Li => return false,
    }

    true
}

fn test_len(context: &mut ParseContext, op: &str, expected: usize, actual: usize) -> bool {
    if actual != expected {
        context
            .errors
            .push(format!("Expected {expected} argument(s) for `{op}`, found `{actual}`"));
        return true;
    }

    false
}

fn integer_register(register: &str) -> Result<Option<register::RiscV>, String> {
    if let Some(num) = register.strip_prefix('x') {
        let num = num.parse::<u8>().map_err(|_| format!("Invalid register number: `{num}`"))?;
        if num >= 32 {
            return Err(format!("register out of range: `{num}`"));
        }

        return Ok(register::RiscV::with_u8(num));
    }

    let reg = register.as_bytes();

    let reg = hashify2::tiny_map!(
        reg,
        "zero" => None,
        "ra" => Some(register::RiscV::X1),
        "sp" => Some(register::RiscV::X2),
        "gp" => Some(register::RiscV::X3),
        "tp" => Some(register::RiscV::X4),
        "t0" => Some(register::RiscV::X5),
        "t1" => Some(register::RiscV::X6),
        "t2" => Some(register::RiscV::X7),
        "s0" => Some(register::RiscV::X8),
        "fp" => Some(register::RiscV::X8),
        "s1" => Some(register::RiscV::X9),
        "a0" => Some(register::RiscV::X10),
        "a1" => Some(register::RiscV::X11),
        "a2" => Some(register::RiscV::X12),
        "a3" => Some(register::RiscV::X13),
        "a4" => Some(register::RiscV::X14),
        "a5" => Some(register::RiscV::X15),
        "a6" => Some(register::RiscV::X16),
        "a7" => Some(register::RiscV::X17),
        "s2" => Some(register::RiscV::X18),
        "s3" => Some(register::RiscV::X19),
        "s4" => Some(register::RiscV::X20),
        "s5" => Some(register::RiscV::X21),
        "s6" => Some(register::RiscV::X22),
        "s7" => Some(register::RiscV::X23),
        "s8" => Some(register::RiscV::X24),
        "s9" => Some(register::RiscV::X25),
        "s10" => Some(register::RiscV::X26),
        "s11" => Some(register::RiscV::X27),
        "t3" => Some(register::RiscV::X28),
        "t4" => Some(register::RiscV::X29),
        "t5" => Some(register::RiscV::X30),
        "t6" => Some(register::RiscV::X31),
    );

    reg.ok_or_else(|| format!("Unexpected register name `{register}`"))
}
fn compressed_integer_register(register: &str) -> Result<Option<register::RiscV>, String> {
    let res = integer_register(register)?;

    let register = res.map_or(0, register::RiscV::get);
    if !(8..16).contains(&register) {
        return Err(format!("register out of range: `{register}` (x8..x16)"));
    }

    Ok(res)
}

fn parse_immediate(src: &str, bits: u8) -> Result<u32, String> {
    fn parse_lit(src: &str) -> Result<u32, String> {
        let (src, base, base_name) = if let Some(src) = src.strip_suffix('h') {
            (src, 16, "hex")
        } else if let Some(src) = src.strip_prefix("0x") {
            (src, 16, "hex")
        } else if let Some(src) = src.strip_prefix("0o") {
            (src, 8, "octal")
        } else if let Some(src) = src.strip_suffix('b') {
            (src, 2, "binary")
        } else if let Some(src) = src.strip_prefix("0b") {
            (src, 2, "binary")
        } else {
            (src, 10, "decimal")
        };

        u32::from_str_radix(src, base).map_err(|_| format!("invalid {base_name} number: {src}"))
    }

    let (src, negative) = match src.strip_prefix('-') {
        Some(src) => (src, true),
        None => (src, false),
    };

    let unsigned = parse_lit(src)?;

    let signed_min = 1 << (bits - 1);
    let unsigned_max = (1 << bits) - 1;

    match negative {
        true if unsigned <= signed_min => {
            Ok(((-unsigned.cast_signed()).cast_unsigned()) & unsigned_max)
        }
        true => Err(format!("invalid {bits}bit literal (-{unsigned} < -{signed_min})")),
        false if unsigned <= unsigned_max => Ok(unsigned),
        false => Err(format!("invalid {bits}bit literal ({unsigned} > {unsigned_max})")),
    }
}

fn parse_imm_sx32(src: &str, bits: u8) -> Result<u32, String> {
    parse_immediate(src, bits).map(|it| sign_extend_32(it, bits))
}

fn parse_imm_sx(src: &str, bits: u8) -> Result<u16, String> {
    parse_immediate(src, bits).map(|it| sign_extend(it as u16, bits))
}
