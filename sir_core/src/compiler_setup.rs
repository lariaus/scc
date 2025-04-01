use std::collections::HashMap;

use crate::{
    ir_transforms::TransformsList,
    pass_manager::{Pass, PassRegistration},
};

pub(crate) struct RegisteredPassInfos {
    pub name: &'static str,
    pub description: &'static str,
    pub builder: Box<dyn Fn() -> Box<dyn Pass>>,
}

// Class to setup SIR compiler.
// This contains lots of settings / registration / setups.
// One difference with IRContext is that objects should only need this object for setup / initialization.
// After, they don't need to keep a reference to it.
pub struct CompilerSetup {
    registered_extra_transforms: HashMap<String, Vec<Box<dyn Fn(&mut TransformsList) -> ()>>>,
    registered_passes_builders: HashMap<&'static str, RegisteredPassInfos>,
}

impl CompilerSetup {
    // Create a new setup with all default options.
    pub fn new() -> Self {
        Self {
            registered_extra_transforms: HashMap::new(),
            registered_passes_builders: HashMap::new(),
        }
    }

    // Add a function that build extra transformers for the pass named `pass_name`.
    pub fn register_extra_pass_transforms<F: Fn(&mut TransformsList) -> () + 'static>(
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

    // Register a new pass with a builder function.
    pub fn register_pass<T: PassRegistration + 'static, F: Fn() -> T + 'static>(
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

    pub(crate) fn get_registered_passes_builders(
        &self,
    ) -> &HashMap<&'static str, RegisteredPassInfos> {
        &self.registered_passes_builders
    }
}
