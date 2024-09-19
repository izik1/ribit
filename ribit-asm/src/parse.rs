use ribit_core::register;

mod instruction;

#[cfg(test)]
mod tests;

#[must_use]
const fn sign_extend(value: u16, data_bits: u8) -> u16 {
    let mask = 16 - data_bits;
    (((value << mask) as i16) >> mask) as u16
}

#[must_use]
const fn sign_extend_32(value: u32, data_bits: u8) -> u32 {
    let mask = 32 - data_bits;
    (((value << mask) as i32) >> mask) as u32
}

// v0.0.1 we don't support fancy things like labels.
// or error locations for that matter.
#[derive(Debug)]
struct ParseContext {
    instructions: Vec<ribit_core::instruction::Info>,
    errors: Vec<String>,
    supports_compressed: bool,
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

pub fn tokenize(input: &str, enable_compressed: bool) -> ParseOutput {
    let mut output = ParseContext {
        instructions: Default::default(),
        errors: Default::default(),
        supports_compressed: enable_compressed,
    };

    for line in input.lines() {
        let line = line.trim();
        let line = line.split_once(';').map(|(line, _comment)| line).unwrap_or(line);

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

    let op = full_op.split('.').last().unwrap();

    let mut args: Vec<_> = args.split(',').map(str::trim).collect();

    if args.last() == Some(&"") {
        args.pop();
    }

    let args = args;

    let instruction_matched = match has_compressed {
        true => instruction::compressed(context, op, full_op, &args),
        false => parse_32(context, op, full_op, &args),
    };

    if !instruction_matched {
        context.errors.push(format!("Unknown instruction `{full_op}`"));
    }
}

fn parse_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    if instruction::r_32(context, op, full_op, args) {
        return true;
    }

    if instruction::i_32(context, op, full_op, args) {
        return true;
    }

    if instruction::ijump_32(context, op, full_op, args) {
        return true;
    }

    if instruction::imem_32(context, op, full_op, args) {
        return true;
    }

    if instruction::s_32(context, op, full_op, args) {
        return true;
    }

    if instruction::b_32(context, op, full_op, args) {
        return true;
    }

    if instruction::u_32(context, op, full_op, args) {
        return true;
    }

    if instruction::j_32(context, op, full_op, args) {
        return true;
    }

    if instruction::sys_32(context, op, full_op, args) {
        return true;
    }

    false
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

fn parse_general_purpose_register(register: &str) -> Result<Option<register::RiscV>, String> {
    if let Some(num) = register.strip_prefix('x') {
        let num = num.parse::<u8>().map_err(|_| format!("Invalid register number: `{num}`"))?;
        if num >= 32 {
            return Err(format!("register out of range: `{num}`"));
        }

        return Ok(register::RiscV::with_u8(num));
    }

    // todo: abi names.

    Err(format!("Unexpected register name `{register}`"))
}
fn parse_compressed_register(register: &str) -> Result<Option<register::RiscV>, String> {
    let res = parse_general_purpose_register(register)?;

    let register = res.map_or(0, |it| it.get());
    if register < 8 || register >= 16 {
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
        true if unsigned <= signed_min => Ok((-(unsigned as i32) as u32) & unsigned_max),
        true => Err(format!("invalid {bits}bit literal (-{unsigned} < -{signed_min})")),
        false if unsigned <= unsigned_max => Ok(unsigned),
        false => Err(format!("invalid {bits}bit literal ({unsigned} > {unsigned_max})")),
    }
}
