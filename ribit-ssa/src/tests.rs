use expect_test::Expect;

use crate::opt::pass_manager::InplacePass;
use crate::opt::PassManager;
use crate::{lower, Block};

pub const MEM_SIZE: u32 = 0x1000000;

pub(crate) fn assemble_block_with_context(mut context: lower::Context, block: &str) -> Block {
    let output = ribit_asm::tokenize(block, true);
    for error in &output.errors {
        eprintln!("error: {}", error);
    }

    if !output.errors.is_empty() {
        panic!("failing due to previous error(s)");
    }

    let mut instructions = output.instructions;

    let last = instructions.remove(instructions.len() - 1);

    for instruction in instructions {
        lower::non_terminal(&mut context, instruction.instruction, instruction.len);
    }

    lower::terminal(context, last.instruction, last.len)
}

pub(crate) fn assemble_block(block: &str) -> Block {
    assemble_block_with_context(lower::Context::new(1024, MEM_SIZE), block)
}

#[track_caller]
pub(crate) fn expect_block(block: &str, expect: Expect) {
    expect_block_with_opts(PassManager::unoptimized(), block, expect)
}

#[track_caller]
pub(crate) fn expect_block_with_opts(pm: PassManager, block: &str, expect: Expect) {
    let mut block = assemble_block(block);
    pm.run(&mut block);
    expect.assert_eq(&block.display_instructions().to_string())
}

pub fn max_fn() -> Block {
    assemble_block(
        r#"
            add x11, x10, x11
            srli x12, x11, 31
            and x11, x11, x12
            add x10, x10, x11
            c.ret
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

    // todo: psudeos: `neg`
    assemble_block(
        r#"
                sltu x12, x10, x11
                sub x12, x0, x12
                xor x10, x10, x11
                and x10, x10, x12
                xor x10, x10, x11
                c.ret
            "#,
    )
}
