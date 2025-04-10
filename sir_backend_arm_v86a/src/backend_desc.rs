use sir_core::ir_context::IRContext;
use sir_transform::{
    sir_backend::BackendsRegistry, sir_backend_descriptor::SIRBackendDescriptorImpl,
    sir_registers_descriptor::RegistersSetDescriptor,
};

use crate::{armv86a_ops::register_armv86a_ops, isel_patterns::register_isel_patterns};

struct ArmV86ABackendDesc;

impl SIRBackendDescriptorImpl for ArmV86ABackendDesc {
    fn get_name(&self) -> &str {
        "Armv8.6-A"
    }

    fn can_target_host(&self) -> bool {
        true
    }

    fn setup_context(&self, ctx: &mut IRContext) {
        register_armv86a_ops(ctx);
        register_isel_patterns(ctx);
    }

    fn build_registers_set_descriptor(&self) -> RegistersSetDescriptor {
        RegistersSetDescriptor::from_source(include_str!("../descs/registers.json"))
    }
}

/// Register the Armv8.6-A backend.
pub fn register_armv86a_backend(registry: &mut BackendsRegistry) {
    registry.register_backend(ArmV86ABackendDesc);
}
