use core::ops::Range;
use std::io::{Read, Write};

use ribit_core::register;

#[repr(u32)]
#[derive(Copy, Clone, Debug)]
enum StatusCode {
    Success = 0_u32,
    #[allow(dead_code)]
    ErrFailure = -1_i32 as u32,
    #[allow(dead_code)]
    ErrNotSupported = -2_i32 as u32,
    #[allow(dead_code)]
    ErrInvalidParam = -3_i32 as u32,
    #[allow(dead_code)]
    ErrDenied = -4_i32 as u32,
    #[allow(dead_code)]
    ErrInvalidAddress = -5_i32 as u32,
}

const EXT_BASE: u32 = 0x10;
const EXT_CONSOLE: u32 = 0x4442_434e;

pub fn call(regs: &mut register::File<u32>, mem: &mut [u8]) {
    let extension_id = regs[register::RiscV::X17]; // x17 -> a7
    let extension_funct = regs[register::RiscV::X16]; // x16 -> a6

    let (code, value) = match (extension_id, extension_funct) {
        (EXT_BASE, 0) => get_sbi_spec_version(),
        (EXT_BASE, 1) => get_sbi_impl_id(),
        (EXT_BASE, 2) => get_sbi_impl_version(),
        (EXT_BASE, 3) => probe_extension(regs[register::RiscV::X10]),
        (EXT_BASE, 4) => get_mvendorid(),
        (EXT_BASE, 5) => get_marchid(),
        (EXT_BASE, 6) => get_mimpid(),
        (EXT_CONSOLE, 0) => debug_console_write(
            mem,
            regs[register::RiscV::X10],
            regs[register::RiscV::X11],
            regs[register::RiscV::X12],
        ),
        (EXT_CONSOLE, 1) => debug_console_read(
            mem,
            regs[register::RiscV::X10],
            regs[register::RiscV::X11],
            regs[register::RiscV::X12],
        ),
        (EXT_CONSOLE, 2) => debug_console_write_byte(regs[register::RiscV::X10]),
        _ => {
            log::warn!("Unsupported!");
            unsupported()
        }
    };

    regs[register::RiscV::X10] = code as u32; // a0
    regs[register::RiscV::X11] = value; // a1
}

const fn unsupported() -> (StatusCode, u32) {
    (StatusCode::ErrNotSupported, 0)
}

const fn get_sbi_spec_version() -> (StatusCode, u32) {
    // no error, minor version 2
    (StatusCode::Success, 2)
}

const fn get_sbi_impl_id() -> (StatusCode, u32) {
    // no error, id = 0xfabadaba
    (StatusCode::Success, 0xfaba_daba)
}

const fn get_sbi_impl_version() -> (StatusCode, u32) {
    // no error, we're v0.1, encoding TBD
    (StatusCode::Success, 1)
}

fn probe_extension(extension_id: u32) -> (StatusCode, u32) {
    // no error
    let id = match extension_id {
        1 | 2 => extension_id,
        _ => 0, // None
    };

    (StatusCode::Success, id)
}

fn get_mvendorid() -> (StatusCode, u32) {
    (StatusCode::Success, 0)
}

fn get_marchid() -> (StatusCode, u32) {
    (StatusCode::Success, 0)
}

fn get_mimpid() -> (StatusCode, u32) {
    (StatusCode::Success, 0)
}

fn addr_range(
    num_bytes: u32,
    base_addr_lo: u32,
    base_addr_hi: u32,
) -> Result<Range<usize>, StatusCode> {
    // if we're somehow running this on a 16 bit system (Please don't!) I don't want to see any complaints lol.
    let Ok(num_bytes) = usize::try_from(num_bytes) else {
        return Err(StatusCode::ErrFailure);
    };

    let base_addr = (u64::from(base_addr_hi) << u32::BITS) + u64::from(base_addr_lo);
    let Ok(base_addr) = usize::try_from(base_addr) else {
        return Err(StatusCode::ErrInvalidAddress);
    };

    let Some(end_addr) = base_addr.checked_add(num_bytes) else {
        return Err(StatusCode::ErrInvalidAddress);
    };

    Ok(base_addr..end_addr)
}

fn debug_console_write(
    mem: &[u8],
    num_bytes: u32,
    base_addr_lo: u32,
    base_addr_hi: u32,
) -> (StatusCode, u32) {
    let range = match addr_range(num_bytes, base_addr_lo, base_addr_hi) {
        Ok(it) => it,
        Err(code) => return (code, 0),
    };

    let Some(bytes) = mem.get(range) else {
        return (StatusCode::ErrInvalidAddress, 0);
    };

    match std::io::stdout().write(bytes) {
        // `num_bytes` comes from a u32, `written` is at most `num_bytes`, ergo, we're fine.
        Ok(written) => (StatusCode::Success, written as u32),
        Err(_) => (StatusCode::ErrFailure, 0),
    }
}

fn debug_console_read(
    mem: &mut [u8],
    num_bytes: u32,
    base_addr_lo: u32,
    base_addr_hi: u32,
) -> (StatusCode, u32) {
    let range = match addr_range(num_bytes, base_addr_lo, base_addr_hi) {
        Ok(it) => it,
        Err(code) => return (code, 0),
    };

    let Some(bytes) = mem.get_mut(range) else {
        return (StatusCode::ErrInvalidAddress, 0);
    };

    match std::io::stdin().read(bytes) {
        // `num_bytes` comes from a u32, `read` is at most `num_bytes`, ergo, we're fine.
        Ok(read) => (StatusCode::Success, read as u32),
        Err(_) => (StatusCode::ErrFailure, 0),
    }
}

fn debug_console_write_byte(byte: u32) -> (StatusCode, u32) {
    let byte = byte as u8;

    match std::io::stdout().lock().write(&[byte]) {
        Ok(_read) => (StatusCode::Success, 0),
        Err(_) => (StatusCode::ErrFailure, 0),
    }
}
