use sir_core::ir_context::IRContext;

use crate::{
    sir_backend::BackendsRegistry, sir_backend_descriptor::SIRBackendDescriptorImpl,
    sir_registers_descriptor::RegistersSetDescriptor,
};

pub(crate) struct UnknownBackendDesc;

impl SIRBackendDescriptorImpl for UnknownBackendDesc {
    fn get_name(&self) -> &str {
        "unknown"
    }

    fn can_target_host(&self) -> bool {
        false
    }

    fn setup_context(&self, _ctx: &mut IRContext) {}

    fn build_registers_set_descriptor(&self) -> RegistersSetDescriptor {
        RegistersSetDescriptor::from_source(REGISTERS_DEC_SRC)
    }
}

pub fn register_unknown_backend(registry: &mut BackendsRegistry) {
    registry.register_backend(UnknownBackendDesc);
}

const REGISTERS_DEC_SRC: &str = "{\"all_regs\": {}}";
