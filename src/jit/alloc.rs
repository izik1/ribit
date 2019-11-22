use std::collections::VecDeque;

use assembler::InstructionStream;

use crate::jit::generator::{generate_register_read, generate_register_writeback};
use crate::register;

#[derive(Default, Copy, Clone)]
struct UsageMask(u32);

impl UsageMask {
    fn _new() -> Self {
        Self::default()
    }

    fn _with_mask(mask: u32) -> Self {
        Self(mask)
    }

    fn is_set(self, register: register::RiscV) -> bool {
        (self.0 >> register.get()) & 1 == 1
    }

    fn set(&mut self, register: register::RiscV) {
        self.0 |= 1 << register.get();
    }

    fn res(&mut self, register: register::RiscV) {
        self.0 &= !(1 << register.get());
    }

    fn is_clear(self) -> bool {
        self.0 == 0
    }
}

pub(super) struct RegisterManager {
    free_registers: Vec<register::Native>,
    used_registers: VecDeque<(register::Native, register::RiscV)>,
    to_load_map: UsageMask,
    to_store_map: UsageMask,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            free_registers: vec![
                // don't allocate RDX, RDX is an arg.
                // register::Native::RDX,
                register::Native::RCX,
                register::Native::R8,
                register::Native::R9,
                // register::Native::RAX,
            ],
            used_registers: VecDeque::new(),
            to_load_map: UsageMask::default(),
            to_store_map: UsageMask::default(),
        }
    }

    pub fn is_cleared(&self) -> bool {
        self.to_store_map.is_clear()
    }

    pub fn is_allocated(&self, register: register::RiscV) -> bool {
        self.find_native_register(register).is_some()
    }

    pub fn needs_store(&self, register: register::RiscV) -> bool {
        self.to_store_map.is_set(register)
    }

    pub fn needs_load(&self, register: register::RiscV) -> bool {
        self.to_load_map.is_set(register)
    }

    pub fn clobber(&mut self, register: register::Native, stream: &mut InstructionStream) {
        if let Some(rv32_reg) = self.find_rv32_register(register) {
            if self.to_store_map.is_set(rv32_reg) {
                generate_register_writeback(stream, register, rv32_reg);
                self.to_store_map.res(rv32_reg);
            }

            self.to_load_map.set(rv32_reg);
        }
    }

    pub fn load(
        &mut self,
        rv32_reg: register::RiscV,
        stream: &mut InstructionStream,
    ) -> Result<(), ()> {
        let native_reg = self.find_native_register(rv32_reg).ok_or(())?;

        if self.to_load_map.is_set(rv32_reg) {
            generate_register_read(stream, native_reg, rv32_reg);
            self.to_load_map.res(rv32_reg);
        }

        Ok(())
    }

    // hack:
    pub fn set_dirty(&mut self, rv32_reg: register::RiscV) -> Result<(), ()> {
        self.find_native_register(rv32_reg).ok_or(())?;

        self.to_store_map.set(rv32_reg);

        Ok(())
    }

    // todo: add a `move register` function to avoid having to write->read a register that's already in a Native register.

    pub fn try_alloc_specific(
        &mut self,
        native_reg: register::Native,
        rv_reg: register::RiscV,
        basic_block: &mut InstructionStream,
    ) -> Result<(), register::RiscV> {
        if let Some(rv_reg) = self.find_rv32_register(native_reg) {
            return Err(rv_reg);
        }

        self.alloc_specific(native_reg, rv_reg, basic_block);

        Ok(())
    }

    pub fn alloc_specific(
        &mut self,
        native_reg: register::Native,
        rv_reg: register::RiscV,
        stream: &mut InstructionStream,
    ) {
        // todo: proper error handling?
        self.free_registers
            .remove_item(&native_reg)
            .expect("Can't reserve a register that's in use!");

        // todo: current setup can't handle the same register being in multiple places at once.
        if self.find_native_register(rv_reg).is_some() {
            self.free(rv_reg, stream);
        }

        self.to_load_map.set(rv_reg);
        self.used_registers.push_back((native_reg, rv_reg));
    }

    fn is_free(&self, reg: register::Native) -> bool {
        self.free_registers.iter().any(|it| *it == reg)
    }

    fn find_rv32_register(&self, reg: register::Native) -> Option<register::RiscV> {
        self.used_registers
            .iter()
            .find_map(|(native_reg, alloced_reg)| {
                if *native_reg == reg {
                    Some(*alloced_reg)
                } else {
                    None
                }
            })
    }

    pub fn find_native_register(&self, reg: register::RiscV) -> Option<register::Native> {
        self.used_registers
            .iter()
            .find_map(|(native_reg, alloced_reg)| {
                if *alloced_reg == reg {
                    Some(*native_reg)
                } else {
                    None
                }
            })
    }

    fn try_alloc(&mut self, rv_reg: register::RiscV) -> Option<register::Native> {
        self.find_native_register(rv_reg).or_else(|| {
            let native_reg = self.free_registers.pop()?;
            self.to_load_map.set(rv_reg);
            self.used_registers.push_back((native_reg, rv_reg));
            Some(native_reg)
        })
    }

    pub fn alloc(
        &mut self,
        reg: register::RiscV,
        keep_regs: &[register::RiscV],
        basic_block: &mut InstructionStream,
    ) -> register::Native {
        if let Some(native_reg) = self.try_alloc(reg) {
            return native_reg;
        }

        // this should only be `None` if keep regs tries to keep as many virtual registers as there are native registers.
        let _ = self.free_first(keep_regs, basic_block)
            .expect("Tried to allocate a register without a free register, and all others needed to be kept.");

        self.try_alloc(reg).unwrap()
    }

    pub fn free(
        &mut self,
        rv32_reg: register::RiscV,
        basic_block: &mut InstructionStream,
    ) -> Option<register::Native> {
        let idx = self
            .used_registers
            .iter()
            .position(|(_, rv_reg)| *rv_reg == rv32_reg)?;

        let native_reg = self.used_registers.remove(idx).map(|it| it.0)?;

        if self.to_store_map.is_set(rv32_reg) {
            generate_register_writeback(basic_block, native_reg, rv32_reg);
            self.to_store_map.res(rv32_reg);
        }

        self.free_registers.push(native_reg);

        Some(native_reg)
    }

    pub fn free_first(
        &mut self,
        keep_regs: &[register::RiscV],
        basic_block: &mut InstructionStream,
    ) -> Option<register::Native> {
        let idx = self
            .used_registers
            .iter()
            .position(|(_, rv_reg)| !keep_regs.contains(rv_reg))?;

        // unreachable because if the index isn't `None`, it's in range.
        let (native_reg, rv_reg) = self
            .used_registers
            .remove(idx)
            .unwrap_or_else(|| unsafe { std::hint::unreachable_unchecked() });

        if self.to_store_map.is_set(rv_reg) {
            generate_register_writeback(basic_block, native_reg, rv_reg);
            self.to_store_map.res(rv_reg);
        }

        self.free_registers.push(native_reg);

        Some(native_reg)
    }

    pub fn free_all(&mut self, basic_block: &mut InstructionStream) {
        for (native_reg, rv_reg) in self.used_registers.drain(..) {
            if self.to_store_map.is_set(rv_reg) {
                generate_register_writeback(basic_block, native_reg, rv_reg);
                self.to_store_map.res(rv_reg);
            }

            self.free_registers.push(native_reg);
        }
    }
}
