use std::collections::HashMap;

use sir_core::{context_globals::ContextGlobal, ir_context::IRContext};

use crate::{
    sir_backend_descriptor::{SIRBackendDescriptor, SIRBackendDescriptorImpl},
    unknown_backend_desc::UnknownBackendDesc,
};

/// Options to configure the backend.
#[derive(Debug, Clone)]
pub struct SIRBackendOptions {
    // name of the selected backend.
    // If none, will use any backend compatible with the host device.
    pub backend_name: Option<String>,
}

impl SIRBackendOptions {
    /// Returns the default backend options.
    pub fn new() -> Self {
        Self { backend_name: None }
    }
}

/// Main class to configure the compiler backend.
pub struct SIRBackend {
    desc: SIRBackendDescriptor,
}

impl ContextGlobal for SIRBackend {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl SIRBackend {
    /// Build a backend object from the options.
    pub fn make(registry: BackendsRegistry, opts: SIRBackendOptions, ctx: &mut IRContext) -> Self {
        // Get the descriptor.
        let mut desc = match registry.take_backend(opts.backend_name.as_ref().map(|x| &x[..])) {
            Ok(desc) => desc,
            Err(err) => panic!("Failed to build the SIRBackend: {}", err),
        };

        // Initialize and build the backend.
        desc.initialize(ctx);
        Self { desc }
    }

    /// Build the unknown backend.
    pub fn make_unknown(ctx: &mut IRContext) -> Self {
        Self::make(BackendsRegistry::new(), SIRBackendOptions::new(), ctx)
    }

    /// Returns the backend descriptor.
    pub fn backend_desc(&self) -> &SIRBackendDescriptor {
        &self.desc
    }
}

/// Class used to register and select backends.
pub struct BackendsRegistry {
    backends: HashMap<String, SIRBackendDescriptor>,
}

impl BackendsRegistry {
    /// Create a new registry/
    pub fn new() -> Self {
        Self {
            backends: HashMap::new(),
        }
    }

    /// Register a new backend.
    pub fn register_backend<T: SIRBackendDescriptorImpl>(&mut self, desc: T) {
        let name = desc.get_name().to_owned();
        if self
            .backends
            .insert(name, SIRBackendDescriptor::new(desc))
            .is_some()
        {
            panic!("Backend already registered");
        }
    }

    /// Returns the backend registered with name `name`.
    pub fn find_backend(&self, name: &str) -> Option<&SIRBackendDescriptor> {
        self.backends.get(name)
    }

    /// Returns the name of the default backend.
    /// Returns None if none is supported.
    pub fn get_default_backend_name(&self) -> Option<&str> {
        self.backends
            .values()
            .find(|x| x.can_target_host())
            .map(|x| x.get_name())
    }

    /// Get all the registered backends.
    pub fn registered_backends(&self) -> impl Iterator<Item = &SIRBackendDescriptor> {
        self.backends.values()
    }

    // Returns a backend from its name.
    // Or None if not found
    fn take_backend(mut self, name: Option<&str>) -> Result<SIRBackendDescriptor, String> {
        if let Some(name) = name {
            return match self.backends.remove(name) {
                Some(desc) => Ok(desc),
                None => Err(format!("backend `{}` not registered", name)),
            };
        }

        // Use the default backend.
        if let Some(name) = self.get_default_backend_name() {
            return Ok(self.backends.remove(&name.to_owned()).unwrap());
        }

        // If no default backend is found, just use unknown.
        Ok(SIRBackendDescriptor::new(UnknownBackendDesc))
    }
}
