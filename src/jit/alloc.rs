use std::collections::VecDeque;

use assembler::InstructionStream;

use crate::jit::generator::{generate_register_read, generate_register_writeback};
use crate::register;

pub(super) struct RegisterManager {
    free_registers: Vec<register::Native>,
    used_registers: VecDeque<(register::Native, register::RiscV)>,
    load_map: u32,
    store_map: u32,
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
            load_map: 0,
            store_map: 0,
        }
    }

    pub fn is_cleared(&self) -> bool {
        self.store_map == 0
    }

    pub fn is_allocated(&self, register: register::RiscV) -> bool {
        self.find_native_register(register).is_some()
    }

    pub fn needs_store(&self, register: register::RiscV) -> bool {
        (self.store_map >> register.get()) & 1 == 1
    }

    pub fn needs_load(&self, register: register::RiscV) -> bool {
        (self.load_map >> register.get()) & 1 == 1
    }

    pub fn load(
        &mut self,
        rv32_reg: register::RiscV,
        stream: &mut InstructionStream,
    ) -> Result<(), ()> {
        let native_reg = self.find_native_register(rv32_reg).ok_or(())?;

        if self.needs_load(rv32_reg) {
            generate_register_read(stream, native_reg, rv32_reg);
            self.load_map &= !(1 << rv32_reg.get())
        }

        Ok(())
    }

    // hack:
    pub fn set_dirty(&mut self, rv32_reg: register::RiscV) -> Result<(), ()> {
        self.find_native_register(rv32_reg).ok_or(())?;

        self.store_map |= 1 << rv32_reg.get();

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

        self.load_map |= 1 << rv_reg.get();
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

    fn try_alloc(
        &mut self,
        rv_reg: register::RiscV,
        basic_block: &mut InstructionStream,
    ) -> Option<register::Native> {
        self.find_native_register(rv_reg).or_else(|| {
            let native_reg = self.free_registers.pop()?;
            self.load_map |= 1 << rv_reg.get();
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
        if let Some(native_reg) = self.try_alloc(reg, basic_block) {
            return native_reg;
        }

        // this should only be `None` if keep regs tries to keep as many virtual registers as there are native registers.
        let native_reg = self.free_first(keep_regs, basic_block)
            .expect("Tried to allocate a register without a free register, and all others needed to be kept.");

        generate_register_read(basic_block, native_reg, reg);
        self.used_registers.push_back((native_reg, reg));
        native_reg
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

        if self.needs_store(rv32_reg) {
            generate_register_writeback(basic_block, native_reg, rv32_reg);
            self.store_map &= !(1 << rv32_reg.get())
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

        if self.needs_store(rv_reg) {
            generate_register_writeback(basic_block, native_reg, rv_reg);
            self.store_map &= !(1 << rv_reg.get())
        }

        self.free_registers.push(native_reg);

        Some(native_reg)
    }

    pub fn free_all(&mut self, basic_block: &mut InstructionStream) {
        for (native_reg, rv_reg) in self.used_registers.drain(..) {
            // hack: inline `Self::needs_store` to play nice with lifetimes
            if (self.store_map >> rv_reg.get()) & 1 == 1 {
                generate_register_writeback(basic_block, native_reg, rv_reg);
                self.store_map &= !(1 << rv_reg.get())
            }

            self.free_registers.push(native_reg);
        }
    }
}
