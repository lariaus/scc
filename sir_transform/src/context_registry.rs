use sir_core::{
    attributes::Attribute,
    ir_context::{IRContext, RawOpBuilder},
    operation_type::OperationTypeInfos,
};

use crate::{
    ir_transforms::TransformsList,
    pass::{DynamicPassOptions, PassRegistration},
    transforms_registry::TransformsRegistry,
};

/// Helper class to be able to register more easily ops / passes / transforms on the Context.
pub struct ContextRegistry<'a> {
    ctx: &'a mut IRContext,
}

impl<'a> ContextRegistry<'a> {
    /// Execute a registry function a single time.
    pub fn exec_register_fn<Fn: FnOnce(ContextRegistry) -> ()>(
        ctx: &'a mut IRContext,
        uid: &'static str,
        f: Fn,
    ) {
        ctx.run_ininitializer(uid, |ctx| {
            let registry = ContextRegistry { ctx };
            f(registry);
        });
    }

    /// Register a new operation.
    pub fn register_operation(&mut self, infos: OperationTypeInfos) {
        self.ctx.register_operation(infos);
    }

    /// Register a constant builder from a function.
    pub fn register_constant_builder<F: Fn(Attribute) -> Option<RawOpBuilder> + 'static>(
        &mut self,
        builder: F,
    ) {
        self.ctx.register_constant_builder(builder);
    }

    /// Register a function that extend the transformations for pass `pass_name`.
    pub fn register_extra_pass_transforms<F: Fn(&mut TransformsList) -> () + 'static>(
        &mut self,
        pass_name: &str,
        transforms_builder: F,
    ) {
        let registry = TransformsRegistry::get_mut(self.ctx);
        registry.register_extra_pass_transforms(pass_name, transforms_builder);
    }

    // Add a callback that setup the options for the pass named `pass_name`.
    pub fn register_pass_config_callback<
        T: DynamicPassOptions + 'static,
        F: Fn(&mut T) -> () + 'static,
    >(
        &mut self,
        pass_name: &str,
        callback_fn: F,
    ) {
        let registry = TransformsRegistry::get_mut(self.ctx);
        registry.register_pass_config_callback(pass_name, move |dyn_opts| {
            let opts = dyn_opts
                .as_any_mut()
                .downcast_mut::<T>()
                .expect("invalid options type");
            callback_fn(opts);
        });
    }

    /// Register a new pass with a builder function.
    pub fn register_pass<T: PassRegistration + 'static, F: Fn() -> T + 'static>(
        &mut self,
        pass_builder: &'static F,
    ) {
        let registry = TransformsRegistry::get_mut(self.ctx);
        registry.register_pass(pass_builder);
    }
}
