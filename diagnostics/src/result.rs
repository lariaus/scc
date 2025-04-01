use std::io::stderr;

use crate::diagnostics::{CompilerDiagnostics, CompilerInputs};

// Result object using CompilerDiagnostics.
// We can't use the rust `Result` type because an object might have both diagnostics and a result.
pub struct CompilerResult<T> {
    diagnostics: Option<CompilerDiagnostics>,
    result: Option<T>,
}

impl<T> CompilerResult<T> {
    // Create a new Compiler result from the diagnostics and an optional result.
    pub fn make(diagnostics: CompilerDiagnostics, result: Option<T>) -> Self {
        let diagnostics = if diagnostics.has_any_errors() {
            Some(diagnostics)
        } else {
            None
        };
        Self {
            diagnostics,
            result,
        }
    }

    // Returns true if there is an available result.
    pub fn has_result(&self) -> bool {
        self.result.is_some()
    }

    // Returns true if there is an available diagnostic.
    pub fn has_diagnostics(&self) -> bool {
        self.diagnostics.is_some()
    }

    // Returns true if any of the diagnostics are error.
    pub fn has_errors(&self) -> bool {
        match &self.diagnostics {
            Some(diagnostics) => diagnostics.has_any_errors(),
            None => false,
        }
    }

    // Print all diagnostics to stderr.
    // Returns none if there was any errors.
    // Returns the result if there was no errors.
    pub fn resolve(self, inputs: CompilerInputs) -> Option<T> {
        self.resolve_with_stream(inputs, &mut stderr())
    }

    // Print all diagnostics to os.
    // Returns none if there was any errors.
    // Returns the result if there was no errors.
    pub fn resolve_with_stream<W: std::io::Write>(
        self,
        inputs: CompilerInputs,
        os: &mut W,
    ) -> Option<T> {
        let has_errs = if let Some(diagnostics) = self.diagnostics {
            diagnostics.logs_all_messages(inputs, os);
            diagnostics.has_any_errors()
        } else {
            false
        };

        if has_errs {
            return None;
        };

        if self.result.is_none() {
            panic!("No errors but no result available")
        }
        self.result
    }

    // Convert the diagnostics messages to a string.
    pub fn diagnostics_to_string(&self, inputs: CompilerInputs) -> String {
        match &self.diagnostics {
            Some(diagnostics) => diagnostics.logs_to_string(inputs),
            None => "".to_string(),
        }
    }

    // Take all generated diagnostics from this object.
    pub fn take_diagnostics(self) -> CompilerDiagnostics {
        match self.diagnostics {
            Some(res) => res,
            None => CompilerDiagnostics::new(),
        }
    }
}
