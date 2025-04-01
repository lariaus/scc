use sir_core::{
    attributes::{Attribute, FloatAttr, IntegerAttr},
    type_converter::TypeConverter,
    types::{FloatType, FunctionType, IntegerType, Type},
};

pub fn is_valid_scalar_int_type(ty: &IntegerType) -> bool {
    // Only support signless types.
    if ty.signed().is_some() {
        return false;
    }

    // Only support bitwidth 8 / 16 / 32 / 64.
    let width = ty.bitwidth();
    width == 8 || width == 16 || width == 32 || width == 64
}

pub fn pred_valid_scalar_int_type(ty: &Type) -> bool {
    match ty {
        Type::Int(ty) => is_valid_scalar_int_type(ty),
        _ => false,
    }
}

pub fn is_valid_scalar_float_type(ty: &FloatType) -> bool {
    // Only supports F32 / F64 for now.
    match ty {
        FloatType::F32 => true,
        FloatType::F64 => true,
    }
}

pub fn pred_valid_scalar_float_type(ty: &Type) -> bool {
    match ty {
        Type::Float(ty) => is_valid_scalar_float_type(ty),
        _ => false,
    }
}

// Returns true if `ty` is a valid LowLevel scalar type.
pub fn is_valid_scalar_type(ty: &Type) -> bool {
    match ty {
        Type::Int(ty) => is_valid_scalar_int_type(ty),
        Type::Float(ty) => is_valid_scalar_float_type(ty),
        _ => false,
    }
}

// Returns true if `function_type` is a valid LowLevel function type.
pub fn is_valid_function_type(function_type: &FunctionType) -> bool {
    function_type.arguments().iter().all(|ty| is_valid_type(ty))
        && function_type.results().iter().all(|ty| is_valid_type(ty))
}

// Returns true if `ty` is a valid LowLevel type.
pub fn is_valid_type(ty: &Type) -> bool {
    // TODO: Support other types.
    match ty {
        Type::Int(ty) => is_valid_scalar_int_type(ty),
        Type::Float(ty) => is_valid_scalar_float_type(ty),
        Type::Function(ty) => is_valid_function_type(ty),
        _ => false,
    }
}

pub struct LowLevelTypeConverter;

impl TypeConverter for LowLevelTypeConverter {
    fn convert_type(&self, ty: &Type) -> Option<Type> {
        match ty {
            Type::Int(ty) => self.convert_int_ty(ty),
            Type::Float(ty) => self.convert_fp_ty(ty),
            Type::Function(ty) => self.convert_function_ty(ty),
            _ => None,
        }
    }
}

impl LowLevelTypeConverter {
    pub fn convert_int_ty(&self, ty: &IntegerType) -> Option<Type> {
        let width = ty.bitwidth();
        // Only some bitwidths are supported.
        if !(width == 8 || width == 16 || width == 32 || width == 64) {
            return None;
        }

        // Otherwise just returns the signless type.
        Some(IntegerType::new(width, None))
    }

    pub fn convert_fp_ty(&self, ty: &FloatType) -> Option<Type> {
        // Currently only F32 / F64 is supported.
        match ty {
            FloatType::F32 => Some(Type::Float(*ty)),
            FloatType::F64 => Some(Type::Float(*ty)),
        }
    }

    pub fn convert_function_ty(&self, ty: &FunctionType) -> Option<Type> {
        let mut new_arguments = Vec::new();
        for arg_ty in ty.arguments() {
            new_arguments.push(self.convert_type(arg_ty)?);
        }

        let mut new_results = Vec::new();
        for res_ty in ty.results() {
            new_results.push(self.convert_type(res_ty)?);
        }

        Some(FunctionType::new(new_arguments, new_results))
    }
}

pub fn convert_int_attr(attr: &IntegerAttr) -> Option<Attribute> {
    // Keep the same value, but remove the sign
    Some(IntegerAttr::new(
        attr.raw_val(),
        IntegerType::new(attr.bitwidth(), None),
    ))
}

pub fn convert_float_attr(attr: &FloatAttr) -> Option<Attribute> {
    // All float types are supported for now.
    Some(Attribute::Float(attr.clone()))
}

// Try to convert an attribute to low-level
pub fn convert_attr(attr: &Attribute) -> Option<Attribute> {
    match attr {
        Attribute::Int(integer_attr) => convert_int_attr(integer_attr),
        Attribute::Float(float_attr) => convert_float_attr(float_attr),
        _ => None,
    }
}
