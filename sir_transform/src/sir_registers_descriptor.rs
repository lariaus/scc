use std::collections::HashMap;

use serde::Deserialize;

#[derive(Deserialize, Clone, Copy)]
pub enum RegisterKind {
    #[serde(rename = "general")]
    General,

    #[serde(rename = "stack_pointer")]
    StackPointer,
}

/// Class containing informations about a register.
#[derive(Deserialize)]
pub struct RegDesc {
    // Name of the register, set automatically from the key.
    #[serde(skip_deserializing)]
    name: String,

    // Kind of the register.
    kind: RegisterKind,

    // Size in bits.
    bitwidth: usize,

    // If true, this isn't a real register, and regalloc will take care of it.
    #[serde(default)]
    is_virtual: bool,
}

impl RegDesc {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn kind(&self) -> RegisterKind {
        self.kind
    }

    pub fn bitwidth(&self) -> usize {
        self.bitwidth
    }

    pub fn is_virtual(&self) -> bool {
        self.is_virtual
    }
}

/// Descriptor class containing all informations about the registers of a SIR backend.
#[derive(Deserialize)]
pub struct RegistersSetDescriptor {
    /// All registeres supported by the backend.
    all_regs: HashMap<String, RegDesc>,
}

impl RegistersSetDescriptor {
    /// Build the RegistersSetDescriptor from the raw source code.
    /// This should be called like `from_source(include_str!(...))`.
    pub fn from_source(s: &'static str) -> Self {
        let mut res: RegistersSetDescriptor = serde_json::from_str(s)
            .expect("Failed to parse source file for RegistersSetDescriptor");
        res._post_parse();
        res
    }

    /// Returns all available registers.
    pub fn registers(&self) -> impl Iterator<Item = &RegDesc> {
        self.all_regs.values()
    }

    /// Returns the descriptor of the rhe register named `reg_name`.
    /// Returns None if this register doesn't exist.
    pub fn get_reg_desc(&self, reg_name: &str) -> Option<&RegDesc> {
        self.all_regs.get(reg_name)
    }

    // Prepare the struct to be usable.
    fn _post_parse(&mut self) {
        // Automatically set the name of the registers.
        for (key, val) in self.all_regs.iter_mut() {
            val.name = key.clone();
        }
    }
}
