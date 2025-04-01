use diagnostics::{
    diagnostics::{
        emit_error, CompilerDiagnostics, CompilerDiagnosticsEmitter, DiagnosticsEmitter,
    },
    result::CompilerResult,
};

use crate::{
    block::Block,
    ir_printer::ValueDefsMap,
    op_tags::TAG_DECLS_BLOCK_OP,
    operation::{GenericOperation, OperationImpl},
};

// Options for the IRVerifier
pub struct IRVerifierOptions {
    pub allow_unregistered_ops: bool,
}

impl IRVerifierOptions {
    // Create the default options.
    pub fn new() -> Self {
        Self {
            allow_unregistered_ops: false,
        }
    }
}

// Struct to verify if the IR structure is correct.
pub struct IRVerifier {
    diagnostics: CompilerDiagnostics,
    value_defs: ValueDefsMap,
    opts: IRVerifierOptions,
}

impl IRVerifier {
    // Create a new verifier
    pub fn new(opts: IRVerifierOptions) -> Self {
        Self {
            diagnostics: CompilerDiagnostics::new(),
            value_defs: ValueDefsMap::new(),
            opts,
        }
    }

    // Verify if an op is well formed. Return diagnostics if anything is wrong with it.
    pub fn verify(mut self, op: GenericOperation) -> CompilerResult<()> {
        // Prepare the values map first.
        self.value_defs.open_scope();
        self.value_defs.fill_with_predecessors_defs(op);

        // Then run the verifier.
        self._verify_op(op);

        CompilerResult::make(self.diagnostics, Some(()))
    }

    // Verify if a block is well formed. Return diagnostics if anything is wrong with it.
    pub fn verify_block(mut self, block: Block) -> CompilerResult<()> {
        // Prepare the values map first.
        self.value_defs.open_scope();
        self.value_defs.fill_with_predecessors_defs_block(block);

        // Then run the verifier.
        self._verify_block(block);

        CompilerResult::make(self.diagnostics, Some(()))
    }

    fn _call_verifier(&mut self, op: GenericOperation) {
        let mut diagnostics = DiagnosticsEmitter::new(&mut self.diagnostics, "IRVerifier");
        let interface = match op.get_builtin_op_interface() {
            Some(interface) => interface,
            None => {
                if !self.opts.allow_unregistered_ops {
                    emit_error(self, &op, format!("Unregistered operation not allowed"));
                }
                return;
            }
        };
        interface.verify(&mut diagnostics);
    }

    fn _verify_op(&mut self, op: GenericOperation) {
        // First, ensure all operands are defined.
        for (idx, input_val) in op.get_inputs().enumerate() {
            if !self.value_defs.contains_def(input_val.as_id()) {
                emit_error(
                    self,
                    &op,
                    format!(
                        "Value definition of operation input #{} doesn't dominate its use",
                        idx
                    ),
                )
                .attach_infos(&input_val, format!("Value definition is"));
            }
        }

        // Then visit all blocks.
        for block in op.get_blocks() {
            self._verify_block(block);
        }

        // Then define all the op results.
        for out_val in op.get_outputs() {
            self.value_defs.insert_op_out_def(out_val.as_id());
        }

        self._call_verifier(op);
    }

    fn _verify_block(&mut self, block: Block) {
        // Define the block operands.
        self.value_defs.open_scope();
        for operand_val in block.get_operands() {
            self.value_defs.insert_block_arg_def(operand_val.as_id());
        }

        let check_terminator = match block.parent() {
            Some(op) => !op.has_tag(TAG_DECLS_BLOCK_OP),
            None => false,
        };

        if check_terminator && block.get_num_ops() == 0 {
            emit_error(
                self,
                &block,
                format!("A block must finish by a terminator op"),
            );
        }

        // Then check the ops.
        for (idx, op) in block.get_ops().enumerate() {
            let is_last = idx + 1 == block.get_num_ops();
            if is_last && check_terminator && !op.is_terminator() {
                emit_error(
                    self,
                    &op,
                    format!("Last op of a block must be a terminator"),
                );
            } else if !is_last && op.is_terminator() {
                emit_error(
                    self,
                    &op,
                    format!("Terminators must be the ultimate ops of a block"),
                );
            }
            self._verify_op(op);
        }

        self.value_defs.close_scope();
    }
}

impl CompilerDiagnosticsEmitter for IRVerifier {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "IR Verifier"
    }
}

// Verify `op` and returns diagnostics.
pub fn verify_op(op: GenericOperation) -> CompilerResult<()> {
    let verifier = IRVerifier::new(IRVerifierOptions::new());
    verifier.verify(op)
}

// Verify `op` and returns diagnostics.
pub fn verify_op_with_options(
    op: GenericOperation,
    options: IRVerifierOptions,
) -> CompilerResult<()> {
    let verifier = IRVerifier::new(options);
    verifier.verify(op)
}

// Verify `block` and returns diagnostics.
pub fn verify_bock(block: Block) -> CompilerResult<()> {
    let verifier = IRVerifier::new(IRVerifierOptions::new());
    verifier.verify_block(block)
}

// Verify `block` and returns diagnostics.
pub fn verify_block_with_options(block: Block, options: IRVerifierOptions) -> CompilerResult<()> {
    let verifier = IRVerifier::new(options);
    verifier.verify_block(block)
}

// Helper functions to write op verifiers
pub mod ir_checks {
    use diagnostics::diagnostics::{emit_error, DiagnosticsEmitter};

    use crate::{
        attributes::{Attribute, AttributeSubClass, TypeAttr},
        ir_printer::IRPrintableObject,
        operation::{GenericOperation, OperationImpl},
        types::{Type, TypeSubClass},
    };

    pub fn verif<S: Into<String>>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        cond: bool,
        msg: S,
    ) {
        if !cond {
            emit_error(emitter, &op, msg.into());
        }
    }

    // Verify if op has the right number of inputs.
    pub fn verif_inputs_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_ninputs: usize,
    ) {
        if op.get_num_inputs() != exp_ninputs {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} inputs, but {} ops must have {}",
                    op.get_num_inputs(),
                    op.opname(),
                    exp_ninputs
                ),
            );
        }
    }

    // Verify if op has the right number of outputs.
    pub fn verif_outputs_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_noutputs: usize,
    ) {
        if op.get_num_outputs() != exp_noutputs {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} outputs, but {} ops must have {}",
                    op.get_num_outputs(),
                    op.opname(),
                    exp_noutputs
                ),
            );
        }
    }

    // Verify if op has the right number of blocks.
    pub fn verif_blocks_count(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        exp_nblocks: usize,
    ) {
        if op.get_num_blocks() != exp_nblocks {
            emit_error(
                emitter,
                &op,
                format!(
                    "Op has {} blocks, but {} ops must have {}",
                    op.get_num_blocks(),
                    op.opname(),
                    exp_nblocks
                ),
            );
        }
    }

    // Verify if the op has the same type for all inputs and outputs.
    pub fn verif_same_input_output_types(emitter: &mut DiagnosticsEmitter, op: GenericOperation) {
        let exp_ty = if op.get_num_inputs() > 0 {
            op.get_input(0).get_type()
        } else if op.get_num_outputs() > 0 {
            op.get_output(0).get_type()
        } else {
            return;
        };

        for in_ty in op.get_inputs_types() {
            if in_ty != exp_ty {
                emit_error(
                    emitter,
                    &op,
                    format!(
                        "All inputs and outputs of {} must have the same type",
                        op.opname()
                    ),
                );
                return;
            }
        }

        for out_ty in op.get_outputs_types() {
            if out_ty != exp_ty {
                emit_error(
                    emitter,
                    &op,
                    format!(
                        "All inputs and outputs of {} must have the same type",
                        op.opname()
                    ),
                );
                return;
            }
        }
    }

    // Verify if op has an attribute
    pub fn verif_has_attr(emitter: &mut DiagnosticsEmitter, op: GenericOperation, name: &str) {
        if op.get_attr(name).is_none() {
            emit_error(
                emitter,
                &op,
                format!("Missing required attribute `{}`", name),
            );
        }
    }

    // Verify if op has an attribute of type T
    pub fn verif_has_attr_of_type<T: AttributeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        name: &str,
    ) {
        let attr = match op.get_attr(name) {
            Some(attr) => attr,
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!("Missing required attribute `{}`", name),
                );
                return;
            }
        };
        if !attr.isa::<T>() {
            emit_error(
                emitter,
                &op,
                format!(
                    "Required attribute `{}` must be of type `{}`, but got {}",
                    name,
                    T::get_typename(),
                    attr.to_string_repr()
                ),
            );
        }
    }

    // Verify if op has a TypeAttribute of type T
    pub fn verif_has_type_attr_of_type<T: TypeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        name: &str,
    ) {
        let attr = match op.get_attr(name) {
            Some(attr) => attr,
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!("Missing required attribute `{}`", name),
                );
                return;
            }
        };
        let ty = match attr.cast::<TypeAttr>() {
            Some(attr) => attr.val(),
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!(
                        "Required attribute `{}` must be a TypedAttr of type {}",
                        name,
                        T::get_typename()
                    ),
                );
                return;
            }
        };
        if !ty.isa::<T>() {
            emit_error(
                emitter,
                &op,
                format!(
                    "Required attribute `{}` must be a TypedAttr of type {}",
                    name,
                    T::get_typename()
                ),
            );
        }
    }

    // Verify if op has an attribute of type T
    pub fn verif_has_attr_as<Predicate: FnOnce(&Attribute) -> bool>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        name: &str,
        predicate: Predicate,
        attr_label: &str,
    ) {
        let attr = match op.get_attr(name) {
            Some(attr) => attr,
            None => {
                emit_error(
                    emitter,
                    &op,
                    format!("Missing required attribute `{}`", name),
                );
                return;
            }
        };
        if !predicate(attr) {
            emit_error(
                emitter,
                &op,
                format!(
                    "Required attribute `{}` must be a {}, but got {}",
                    name,
                    attr_label,
                    attr.to_string_repr()
                ),
            );
        }
    }

    // Verify if op input #idx is of type T
    pub fn verif_input_is_of_type<T: TypeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        idx: usize,
        value_name: &str,
    ) {
        let val = op.get_input(idx);
        if !val.get_type().isa::<T>() {
            emit_error(
                emitter,
                &op,
                format!(
                    "Input #{} ({}) must be of type `{}`, but got {}",
                    idx,
                    value_name,
                    T::get_typename(),
                    val.get_type().to_string_repr()
                ),
            );
        }
    }

    // Verify if op input #idx verifies predicates.
    pub fn verif_input_is_as<Predicate: FnOnce(&Type) -> bool>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        idx: usize,
        value_name: &str,
        predicate: Predicate,
        value_label: &str,
    ) {
        let val = op.get_input(idx);
        if !predicate(val.get_type()) {
            emit_error(
                emitter,
                &op,
                format!(
                    "Input #{} ({}) must be a {}, but got {}",
                    idx,
                    value_name,
                    value_label,
                    val.get_type().to_string_repr()
                ),
            );
        }
    }

    // Verify if op output #idx is of type T
    pub fn verif_output_is_of_type<T: TypeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        idx: usize,
        value_name: &str,
    ) {
        let val = op.get_output(idx);
        if !val.get_type().isa::<T>() {
            emit_error(
                emitter,
                &op,
                format!(
                    "Output #{} ({}) must be of type `{}`, but got {}",
                    idx,
                    value_name,
                    T::get_typename(),
                    val.get_type().to_string_repr()
                ),
            );
        }
    }

    // Verify if op output #idx verifies predicates.
    pub fn verif_output_is_as<Predicate: FnOnce(&Type) -> bool>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        idx: usize,
        value_name: &str,
        predicate: Predicate,
        value_label: &str,
    ) {
        let val = op.get_output(idx);
        if !predicate(val.get_type()) {
            emit_error(
                emitter,
                &op,
                format!(
                    "Output #{} ({}) must be a {}, but got {}",
                    idx,
                    value_name,
                    value_label,
                    val.get_type().to_string_repr()
                ),
            );
        }
    }

    pub fn verif_io_is_of_type<T: TypeSubClass>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        is_input: bool,
        idx: usize,
        value_name: &str,
    ) {
        if is_input {
            verif_input_is_of_type::<T>(emitter, op, idx, value_name)
        } else {
            verif_output_is_of_type::<T>(emitter, op, idx, value_name)
        }
    }

    pub fn verif_io_type<Predicate: FnOnce(&Type) -> bool>(
        emitter: &mut DiagnosticsEmitter,
        op: GenericOperation,
        is_input: bool,
        idx: usize,
        value_name: &str,
        predicate: Predicate,
        value_label: &str,
    ) {
        if is_input {
            verif_input_is_as(emitter, op, idx, value_name, predicate, value_label)
        } else {
            verif_output_is_as(emitter, op, idx, value_name, predicate, value_label)
        }
    }

    pub fn pred_is_scalar_attribute(attr: &Attribute) -> bool {
        let ty = match attr.get_type() {
            Some(ty) => ty,
            None => return false,
        };
        match ty {
            Type::Int(_) | Type::Float(_) => true,
            _ => false,
        }
    }

    pub fn pred_match_type_of_attr(op: &GenericOperation, ty: &Type, attr_name: &str) -> bool {
        let attr = match op.get_attr(attr_name) {
            Some(attr) => attr,
            None => return false,
        };

        let attr_ty = match attr.get_type() {
            Some(ty) => ty,
            None => return false,
        };
        attr_ty == ty
    }
}

// Extra verifier definitions for XGenDef.

// Define a mod that runs some extra verify code for the op.
// @XGENDEF OpVerifierMod<Code>
// @kind "op-verifier-mod"
// @data Code

// Check that all inputs and outputs have the same types.
// @XGENDEF SameInputsAndOutputsTypes : OpVerifierMod<{{
//    ir_checks::verif_same_input_output_types(diagnostics, self.generic());
// }}>

// Base class for all definition of inputs / outputs.
// @XGENDEF ValueBase<Name, IsVariadic, VerifCode>
// @name Name
// @is_variadic IsVariadic
// @verif_code VerifCode

// Define a value that can be of any type.
// @XGENDEF AnyValue<Name> : ValueBase<Name, false, "">

// Define a variadic value that can take any type.
// @XGENDEF VariadicValue<Name> : ValueBase<Name, true, "">

// Define a value of type IntegerType.
// @XGENDEF IntegerValue<Name> : ValueBase<Name, false, {{
//  ir_checks::verif_io_is_of_type::<IntegerType>(diagnostics, self.generic(), $is_input, $idx, $value_name);
// }}>

// Define a value whose type must verify PredFn.
// @XGENDEF ValueWithPred<Name, PredFn, ExpValueDesc> : ValueBase<Name, false, {{
//  ir_checks::verif_io_type(diagnostics, self.generic(), $is_input, $idx, $value_name, |ty| ##PredFn, "##ExpValueDesc");
// }}>

// Predicate to ensure that the type is the same than the type of `AttrName`.
// @XGENDEF PredMatchTypeOfAttr<AttrName> = {{
//   ir_checks::pred_match_type_of_attr(&self.generic(), ty, "##AttrName")
// }}

// Base class for all attribute definitions.
// @XGENDEF AttrBase<Name, AttrType, GetterCode, VerifCode>
// @name Name
// @attr_type AttrType
// @getter_code GetterCode
// @verif_code VerifCode

// Define an attr that can be of any type.
// @XGENDEF AnyAttr<Name> : AttrBase<Name,
//    {{ &'a Attribute }},
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute") }},
//    {{ ir_checks::verif_has_attr(diagnostics, self.generic(), $attr_symbol); }}
// >

// Define an attr that must be of type AttrType
// @XGENDEF AttrOfType<Name, AttrType, ImplType> : AttrBase<Name, ImplType,
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute").cast::<##AttrType>().expect("`##Name` attribute must be a ##AttrType").val() }},
//    {{ ir_checks::verif_has_attr_of_type::<##AttrType>(diagnostics, self.generic(), $attr_symbol); }}
// >

// Define an attr that must be a StringAttr.
// @XGENDEF StringAttr<Name> : AttrOfType<Name, "StringAttr", "&'a str">

// Define an attr that must be a TypedAttr of type Type
// Define an attr that must be a FunctionType.
// @XGENDEF TypedAttrOfType<Name, Type> : AttrBase<Name,
//    {{ &'a ##Type }},
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute").cast::<TypeAttr>().expect("`##Name` attribute must be a Type").val().cast::<##Type>().expect("`##Name` type attribute must be a ##Type") }},
//    {{ ir_checks::verif_has_type_attr_of_type::<##Type>(diagnostics, self.generic(), $attr_symbol); }}
// >

// Define an attr that must be a FunctionType.
// @XGENDEF FunctionTypeAttr<Name> : TypedAttrOfType<Name, "FunctionType">

// Define an attr that must verify the PredFn function
// @XGENDEF AttrWithPred<Name, PredFn, ExpAttrDesc> : AttrBase<Name, "&'a Attribute",
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute") }},
//    {{ ir_checks::verif_has_attr_as(diagnostics, self.generic(), $attr_symbol, |attr| ##PredFn, "##ExpAttrDesc"); }}
// >

// Predicate function that attr is a scalar attribute.
// @XGENDEF PredIsScalarAttr = {{ ir_checks::pred_is_scalar_attribute(attr) }}

// Define an attr that represent a scalar number.
// @XGENDEF ScalarAttr<Name> : AttrWithPred<Name, PredIsScalarAttr, "scalar attribute">

// Add more defs ...
