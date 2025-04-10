use sir_core::ir_context::IRContext;

use crate::sir_registers_descriptor::RegistersSetDescriptor;

/// Contains all informations needed to represent the backend and generate assembly code.
/// (instructions, registers, etc).
pub trait SIRBackendDescriptorImpl: 'static {
    /// Returns the name of the backend.
    fn get_name(&self) -> &str;

    /// Returns true if the backend can generate machine code running on this machine.
    fn can_target_host(&self) -> bool;

    /// Register all ops / transforms needed for the backend.
    fn setup_context(&self, ctx: &mut IRContext);

    /// Build the descriptor for the registers of the backend.
    fn build_registers_set_descriptor(&self) -> RegistersSetDescriptor;
}

/// Wrapper around `SIRBackendDescriptorImpl` to fetch the backend infos.
pub struct SIRBackendDescriptor {
    _impl: Box<dyn SIRBackendDescriptorImpl>,
    _registers_desc: Option<RegistersSetDescriptor>,
}

impl SIRBackendDescriptor {
    /// Build a new descriptor from an impl class.
    pub fn new<T: SIRBackendDescriptorImpl>(desc: T) -> Self {
        Self {
            _impl: Box::new(desc),
            _registers_desc: None,
        }
    }

    /// Returns the backend name.
    pub fn get_name(&self) -> &str {
        self._impl.get_name()
    }

    /// Returns if the current backend can target the host device.
    pub fn can_target_host(&self) -> bool {
        self._impl.can_target_host()
    }

    /// Initialize the backend descriptor object.
    pub fn initialize(&mut self, ctx: &mut IRContext) {
        self._impl.setup_context(ctx);
        self._registers_desc = Some(self._impl.build_registers_set_descriptor());
    }

    /// Returns the registers set descriptor.
    pub fn get_registers_desc(&self) -> &RegistersSetDescriptor {
        self._registers_desc
            .as_ref()
            .expect("Backend not initialized")
    }
}
