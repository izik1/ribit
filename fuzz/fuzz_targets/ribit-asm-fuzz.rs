#![no_main]

use std::hint::black_box;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    black_box(ribit_asm::tokenize(data, false));
    black_box(ribit_asm::tokenize(data, true));
    // fuzzed code goes here
});
