use sir_core::types::Type;

// Helper trait to convert types.
// Mostly used for tranforms / passes.
pub trait TypeConverter {
    // Try to convert `ty`.
    // Returns None if translation fails.
    fn convert_type(&self, ty: &Type) -> Option<Type>;
}
