use super::{BlockBuilder, LoadProfile, Memory, StoreProfile};
use crate::register;

pub fn addi(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
) {
    match (rs, imm) {
        (None, _) => builder.write_register_imm(rd, imm, Some(StoreProfile::Allocate)),
        (Some(rs), 0) => builder.register_mov(rd, rs),
        (Some(rs), _) if rd != rs => {
            let (native_rd, rs) = builder.register_manager.alloc_2(
                (rd, rs),
                &[rd, rs],
                &mut builder.stream,
                (LoadProfile::Lazy, LoadProfile::Eager),
            );

            builder.register_manager.set_dirty(rd);

            builder.stream.lea_Register32Bit_Any32BitMemory(
                native_rd.as_asm_reg32(),
                Memory::base_64_displacement(rs.as_asm_reg64(), imm.into()),
            );
        }

        (Some(rs), _) => {
            let rs = builder.ez_alloc(rs);
            builder.register_manager.set_dirty(rd);
            builder
                .stream
                .add_Register32Bit_Immediate32Bit(rs.as_asm_reg32(), imm.into());
        }
    }
}
