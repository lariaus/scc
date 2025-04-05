use sir_core::{canonicalize_pass::CanonicalizePass, cse_pass::CSEPass, pass_manager::PassManager};
use sir_low_level::legalize_pass::LegalizeToLowLevelPass;

/// Create the full pipeline that takes any SIR module, optimize it and lower it to LIR.
pub fn create_sir_to_lir_pipeline(pm: &mut PassManager) {
    // Optimize the IR first.
    pm.add_pass(CanonicalizePass::new());
    pm.add_pass(CSEPass::new());

    // Then lower to LIR.
    pm.add_pass(LegalizeToLowLevelPass::new());

    // And then optimize the IR again.
    pm.add_pass(CanonicalizePass::new());
    pm.add_pass(CSEPass::new());
}
