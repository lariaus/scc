pub mod interfaces;
pub mod legalize_pass;
pub mod low_level_types;
pub mod tags;

// Register all passes of sir_low_level.
pub fn register_low_level_passes(cs: &mut sir_core::compiler_setup::CompilerSetup) {
    cs.register_pass(&legalize_pass::LegalizeToLowLevelPass::new);
}
