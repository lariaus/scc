use std::collections::HashMap;

use sir_core::{context_globals::ContextGlobal, ir_context::IRContext};

use crate::{
    ir_transforms::TransformsList,
    pass::{DynamicPassOptions, Pass, PassRegistration},
};

pub struct RegisteredPassInfos {
    pub name: &'static str,
    pub description: &'static str,
    pub builder: Box<dyn Fn() -> Box<dyn Pass>>,
}

/// Hold all registered custom passes / transforms.
pub struct TransformsRegistry {
    registered_extra_transforms: HashMap<String, Vec<Box<dyn Fn(&mut TransformsList) -> ()>>>,
    registered_passes_configs: HashMap<String, Vec<Box<dyn Fn(&mut dyn DynamicPassOptions) -> ()>>>,
    registered_passes_builders: HashMap<&'static str, RegisteredPassInfos>,
}

impl ContextGlobal for TransformsRegistry {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl TransformsRegistry {
    /// Get the singleton attachated to the context.
    /// Returns None if the singleton wasn't initialized.
    pub fn get<'a>(ctx: &'a IRContext) -> Option<&'a Self> {
        ctx.get_singleton::<Self>()
    }

    pub(crate) fn get_mut<'a>(ctx: &'a mut IRContext) -> &'a mut Self {
        if ctx.contains_singleton::<Self>() {
            ctx.get_singleton_mut::<Self>().unwrap()
        } else {
            ctx.insert_singleton(TransformsRegistry::new())
        }
    }

    // Add a function that build extra transformers for the pass named `pass_name`.
    pub(crate) fn register_extra_pass_transforms<F: Fn(&mut TransformsList) -> () + 'static>(
        &mut self,
        pass_name: &str,
        transforms_builder: F,
    ) {
        if !self.registered_extra_transforms.contains_key(pass_name) {
            self.registered_extra_transforms
                .insert(pass_name.to_owned(), Vec::new());
        }
        let builders = self.registered_extra_transforms.get_mut(pass_name).unwrap();
        builders.push(Box::new(transforms_builder));
    }

    // Add a callback that setup the options for the pass named `pass_name`.
    pub(crate) fn register_pass_config_callback<
        F: Fn(&mut dyn DynamicPassOptions) -> () + 'static,
    >(
        &mut self,
        pass_name: &str,
        callback_fn: F,
    ) {
        if !self.registered_passes_configs.contains_key(pass_name) {
            self.registered_passes_configs
                .insert(pass_name.to_owned(), Vec::new());
        }
        let builders = self.registered_passes_configs.get_mut(pass_name).unwrap();
        builders.push(Box::new(callback_fn));
    }

    // Register a new pass with a builder function.
    pub(crate) fn register_pass<T: PassRegistration + 'static, F: Fn() -> T + 'static>(
        &mut self,
        pass_builder: &'static F,
    ) {
        let name = T::get_pass_name();
        assert!(
            !self.registered_passes_builders.contains_key(name),
            "Pass {} already registered",
            name
        );
        let infos = RegisteredPassInfos {
            name: T::get_pass_name(),
            description: T::get_pass_description(),
            builder: Box::new(|| Box::new(pass_builder())),
        };
        self.registered_passes_builders.insert(name, infos);
    }

    pub(crate) fn get_registered_extra_transforms(
        &self,
    ) -> &HashMap<String, Vec<Box<dyn Fn(&mut TransformsList) -> ()>>> {
        &self.registered_extra_transforms
    }

    pub fn get_registered_passes_builders(&self) -> &HashMap<&'static str, RegisteredPassInfos> {
        &self.registered_passes_builders
    }

    pub(crate) fn get_registered_passes_configs(
        &self,
    ) -> &HashMap<String, Vec<Box<dyn Fn(&mut dyn DynamicPassOptions) -> ()>>> {
        &self.registered_passes_configs
    }

    fn new() -> Self {
        Self {
            registered_extra_transforms: HashMap::new(),
            registered_passes_configs: HashMap::new(),
            registered_passes_builders: HashMap::new(),
        }
    }
}
