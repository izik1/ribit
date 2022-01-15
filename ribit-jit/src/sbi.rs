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

pub fn call(regs: &mut [u32; crate::XLEN]) {
    let extension_id = regs[17]; // x17 -> a7
    let extension_funct = regs[16]; // x16 -> a6

    let (code, value) = match (extension_id, extension_funct) {
        (0x01, _) => console_putchar(regs[12]),
        (0x02, _) => console_getchar(),
        (0x10, 0) => get_sbi_spec_version(),
        (0x10, 1) => get_sbi_impl_id(),
        (0x10, 2) => get_sbi_impl_version(),
        (0x10, 3) => probe_extension(regs[12]),
        (0x10, 4) => get_mvendorid(),
        (0x10, 5) => get_marchid(),
        (0x10, 6) => get_mimpid(),
        _ => {
            log::warn!("Unsupported!");
            unsupported()
        }
    };

    regs[10] = code as u32; // a0
    regs[11] = value; // a1
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

fn console_getchar() -> (StatusCode, u32) {
    use std::io::Read;

    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let mut buf = [0];
    let code = match stdin.read_exact(&mut buf) {
        Ok(_) => StatusCode::Success,
        Err(_) => StatusCode::ErrFailure,
    };

    log::debug!("{}", u32::from(buf[0]));
    log::debug!("{code:?}");

    (code, u32::from(buf[0]))
}

fn console_putchar(ch: u32) -> (StatusCode, u32) {
    println!("{:08x}", ch);
    print!("{}", char::from(ch as u8));
    (StatusCode::Success, 0)
}
