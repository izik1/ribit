use crate::{lower, Block};

pub const MEM_SIZE: u32 = 0x1000000;

pub(crate) fn assemble_block(block: &str) -> Block {
    let output = ribit_asm::tokenize(block, true);
    for error in &output.errors {
        eprintln!("error: {}", error);
    }

    if !output.errors.is_empty() {
        panic!("failing due to previous error(s)");
    }

    let mut instructions = output.instructions;

    let mut ctx = lower::Context::new(1024, MEM_SIZE);

    let last = instructions.remove(instructions.len() - 1);

    for instruction in instructions {
        lower::non_terminal(&mut ctx, instruction.instruction, instruction.len);
    }

    lower::terminal(ctx, last.instruction, last.len)
}

pub fn max_fn() -> Block {
    // todo: psudeos: `ret`
    assemble_block(
        r#"
                ADD x11, x10, x11
                SRLI x12, x11, 31
                AND x11, x11, x12
                ADD x10, x10, x11
                JALR x0, 0(x1)
            "#,
    )
}

pub fn min_fn() -> Block {
    // fn min(x: u32, y: u32) -> u32 {
    //     let tmp0 = (x < y) as u32;
    //     let tmp1 = (0 - tmp0 as i32) as u32;
    //     let tmp2 = x ^ y;
    //     let tmp3 = tmp2 & tmp1;
    //     y ^ tmp3
    // }

    // todo: psudeos: `neg`, `ret`
    assemble_block(
        r#"
                SLTU x12, x10, x11
                SUB x12, x0, x12
                XOR x10, x10, x11
                AND x10, x10, x12
                XOR x10, x10, x11
                JALR x0, 0(x1)
            "#,
    )
}
