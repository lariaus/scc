use diagnostics::diagnostics::{emit_error, LocatableObject};
use iostreams::location::Location;
use parse::{lexer::TokenValue, parser::Parser};

use crate::{
    attributes::{Attribute, DictAttr},
    block::Block,
    ir_builder::OpImplBuilderState,
    ir_context::IRContext,
    ir_data::{BlockID, OperationData, OperationID, ValueID},
    ir_parser::{IRParsableObject, IRParsableObjectWithContext, IRParser, OperationParserState},
    ir_printer::{IRPrintableObject, IRPrinter, IRPrinterOptions},
    ir_visitor::{walk_ir, walk_ir_mut, IRVisitor, IRVisitorMut, WalkOlder},
    op_interfaces::{BuiltinOp, OpInterfaceObject},
    op_tags::{OperationTag, TAG_TERMINATOR_OP},
    operation_type::{OperationTypeInfos, OperationTypeRef, OperationTypeUID},
    types::Type,
    value::Value,
};

// Every op must implement this trait.
pub trait OperationImpl<'a> {
    //////////////////////////////////////////////
    // Methods to implement
    //////////////////////////////////////////////

    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self;

    // Get the OperationData associated with the object.
    fn get_op_data(&self) -> &'a OperationData;

    // Get the IRContext associated with the object.
    fn get_context(&self) -> &'a IRContext;

    // Returns the uid of the op.
    fn get_op_type_uid() -> OperationTypeUID;

    // Returns true only for GenericOperation class
    fn is_generic_op() -> bool {
        false
    }

    //////////////////////////////////////////////
    // Helper functions.
    //////////////////////////////////////////////

    // Convert to a GenericOperation
    fn generic(&self) -> GenericOperation<'a> {
        GenericOperation::make_from_data(self.get_context(), self.get_op_data())
    }

    // Returns the uid of the operation.
    fn as_id(&self) -> OperationID {
        self.get_op_data().as_id()
    }

    // Returns the location of the operation.
    fn loc(&self) -> Location {
        self.get_op_data().loc()
    }

    // Returns the OperationTypeRef of the current operation.
    fn op_type(&self) -> &'a OperationTypeRef {
        self.get_op_data().op_type()
    }

    // Get the operand name of the current op.
    fn opname(&self) -> &'a str {
        match self.op_type() {
            OperationTypeRef::Registered(uid) => {
                self.get_context().get_op_type_infos(*uid).opname()
            }
            OperationTypeRef::Unknown(opname) => opname,
        }
    }

    // Returns the type id of the op. (or None if the op isn't registered).
    fn get_op_type_id(&self) -> Option<OperationTypeUID> {
        match self.op_type() {
            OperationTypeRef::Registered(uid) => Some(*uid),
            OperationTypeRef::Unknown(_) => None,
        }
    }

    // Returns the type infos of the op. (or None if the op isn't registered).
    fn get_op_type_infos(&self) -> Option<&'a OperationTypeInfos> {
        match self.op_type() {
            OperationTypeRef::Registered(uid) => Some(self.get_context().get_op_type_infos(*uid)),
            OperationTypeRef::Unknown(_) => None,
        }
    }

    // Returns the BuiltinOp interface implementation of the op. (or None is the op isn't registered).
    fn get_builtin_op_interface(&self) -> Option<BuiltinOp<'a>> {
        let wrapper = self.get_op_type_infos()?.builtin_interface();
        Some(BuiltinOp::get_from_builtin_interface(
            wrapper,
            self.get_context(),
            self.get_op_data(),
        ))
    }

    // Returns true if the op has the tag.
    fn has_tag(&self, tag: OperationTag) -> bool {
        let infos = match self.get_op_type_infos() {
            Some(infos) => infos,
            None => return false,
        };
        infos.has_tag(tag)
    }

    // Returns true if the op is a terminator.
    fn is_terminator(&self) -> bool {
        self.has_tag(TAG_TERMINATOR_OP)
    }

    // Returns the number of inputs of the operation.
    fn get_num_inputs(&self) -> usize {
        self.get_op_data().inputs().len()
    }

    // Returns the input at position #idx.
    fn get_input(&self, idx: usize) -> Value<'a> {
        let inputs = self.get_op_data().inputs();
        assert!(idx < inputs.len());

        let ctx = self.get_context();
        Value::make(ctx, ctx.get_value_data(inputs[idx]))
    }

    // Returns an iterator for the inputs.
    fn get_inputs(&self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        let ctx = self.get_context();
        self.get_op_data()
            .inputs()
            .iter()
            .map(|uid| Value::make(ctx, ctx.get_value_data(*uid)))
    }

    // Returns the list of ValueID for the inputs.
    fn get_inputs_ids(&self) -> &'a [ValueID] {
        self.get_op_data().inputs()
    }

    // Returns an iterator for the outputs types.
    fn get_inputs_types(&self) -> impl DoubleEndedIterator<Item = &'a Type> {
        self.get_inputs().map(|v| v.get_type())
    }

    // Returns the number of outputs of the operation.
    fn get_num_outputs(&self) -> usize {
        self.get_op_data().outputs().len()
    }

    // // Returns the input at position #idx.
    fn get_output(&self, idx: usize) -> Value<'a> {
        let outputs = self.get_op_data().outputs();
        assert!(idx < outputs.len());

        let ctx = self.get_context();
        Value::make(ctx, ctx.get_value_data(outputs[idx]))
    }

    // Returns an iterator for the outputs.
    fn get_outputs(&self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        let ctx = self.get_context();
        self.get_op_data()
            .outputs()
            .iter()
            .map(|uid| Value::make(ctx, ctx.get_value_data(*uid)))
    }

    // Returns an iterator for the outputs types.
    fn get_outputs_types(&self) -> impl DoubleEndedIterator<Item = &'a Type> {
        self.get_outputs().map(|v| v.get_type())
    }

    // Returns the attributes dict of the operation.
    fn get_attrs_dict(&self) -> &'a DictAttr {
        self.get_op_data().attrs()
    }

    // Finds an attribute by name.
    // Returns none if it's not in the attributes dict.
    fn get_attr(&self, name: &str) -> Option<&'a Attribute> {
        self.get_attrs_dict().get_with_str_key(name)
    }

    // Returns the number of blocks of the op.
    fn get_num_blocks(&self) -> usize {
        self.get_op_data().blocks().len()
    }

    // Returns the block #idx
    fn get_block(&self, idx: usize) -> Block<'a> {
        let blocks = self.get_op_data().blocks();
        assert!(idx < blocks.len());
        self.get_context().get_block(blocks[idx])
    }

    // Returns an iterator over all blocks of the op.
    fn get_blocks(&self) -> impl DoubleEndedIterator<Item = Block<'a>> {
        let ctx = self.get_context();
        self.get_op_data()
            .blocks()
            .iter()
            .map(|uid| ctx.get_block(*uid))
    }

    // Returns the parent block of the op, or none if it has no parent.
    fn parent(&self) -> Option<Block<'a>> {
        let block = self.get_op_data().parent()?;
        Some(self.get_context().get_block(block))
    }

    // Returns the parent of the op, or none if it has no parent.
    fn parent_op(&self) -> Option<GenericOperation<'a>> {
        self.parent()?.parent()
    }

    // Returns true if the following op implement the interface.
    fn has_interface<T: OpInterfaceObject<'a>>(&self) -> bool {
        let infos = match self.get_op_type_infos() {
            Some(infos) => infos,
            None => return false,
        };
        infos.has_interface(T::get_interface_uid())
    }

    // Cast the op to the OpInterface T.
    // Returns None if the op doesn't implement this interface.
    fn get_interface<T: OpInterfaceObject<'a>>(&self) -> Option<T> {
        let infos = self.get_op_type_infos()?;
        let interface = infos.get_interface(T::get_interface_uid())?;
        Some(T::make(interface, self.get_context(), self.get_op_data()))
    }

    // Walk the IR.
    fn walk<OpT: OperationImpl<'a>, Visitor: IRVisitor<'a, OpT>>(
        &self,
        order: WalkOlder,
        v: &Visitor,
    ) {
        walk_ir(self.generic(), order, v)
    }

    // Walk the IR.
    fn walk_mut<OpT: OperationImpl<'a>, Visitor: IRVisitorMut<'a, OpT>>(
        &self,
        order: WalkOlder,
        v: &mut Visitor,
    ) {
        walk_ir_mut(self.generic(), order, v)
    }
}

// Base class to look at any operation.
#[derive(Clone, Copy)]
pub struct GenericOperation<'a> {
    ctx: &'a IRContext,
    data: &'a OperationData,
}

impl<'a> OperationImpl<'a> for GenericOperation<'a> {
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {
        Self { ctx, data }
    }

    fn get_op_data(&self) -> &'a OperationData {
        self.data
    }

    fn get_context(&self) -> &'a IRContext {
        self.ctx
    }

    fn get_op_type_uid() -> OperationTypeUID {
        panic!("GenericOperation::get_op_type_uid should never be called");
    }

    fn is_generic_op() -> bool {
        true
    }
}

impl<'a> GenericOperation<'a> {
    pub fn make(ctx: &'a IRContext, uid: OperationID) -> Self {
        Self {
            ctx,
            data: ctx.get_operation_data(uid),
        }
    }

    // Returns if the op is of type `T`
    pub fn isa<'b, T: OperationImpl<'b>>(&self) -> bool {
        // We can always cast GenericOperation to GenericOperation.
        if T::is_generic_op() {
            return true;
        }
        match self.data.op_type() {
            OperationTypeRef::Registered(type_id) => *type_id == T::get_op_type_uid(),
            OperationTypeRef::Unknown(_) => false,
        }
    }

    // Tries to cast the op to a `T`.
    // Returns None if op is not of type T.
    pub fn cast<T: OperationImpl<'a>>(&self) -> Option<T> {
        if self.isa::<T>() {
            Some(T::make_from_data(&self.ctx, &self.data))
        } else {
            None
        }
    }
}

impl GenericOperation<'_> {
    pub fn build<
        InputsValues: Into<Vec<ValueID>>,
        OutputsTypes: Into<Vec<Type>>,
        Attrs: Into<DictAttr>,
        Blocks: Into<Vec<BlockID>>,
    >(
        op_type_uid: OperationTypeUID,
        inputs: InputsValues,
        outputs_types: OutputsTypes,
        attrs_dict: Attrs,
        blocks: Blocks,
    ) -> OpImplBuilderState<Self> {
        let mut st = OpImplBuilderState::make_with_type_uid(op_type_uid);
        st.set_inputs(inputs);
        st.set_outputs_types(outputs_types);
        st.set_attrs_dict(attrs_dict);
        st.set_blocks(blocks);
        st
    }
}

impl<'a> From<GenericOperation<'a>> for OperationID {
    fn from(op: GenericOperation<'a>) -> Self {
        op.as_id()
    }
}

impl<'a> From<GenericOperation<'a>> for ValueID {
    fn from(op: GenericOperation<'a>) -> Self {
        assert!(
            op.get_num_outputs() == 1,
            "Can't convert to value an operation with {} outputs",
            op.get_num_outputs()
        );
        op.get_output(0).as_id()
    }
}

impl<'a> From<GenericOperation<'a>> for Value<'a> {
    fn from(op: GenericOperation<'a>) -> Self {
        assert!(
            op.get_num_outputs() == 1,
            "Can't convert to value an operation with {} outputs",
            op.get_num_outputs()
        );
        op.get_output(0)
    }
}

impl<'a> LocatableObject for GenericOperation<'a> {
    fn get_location(&self) -> Location {
        self.loc()
    }

    fn get_string_repr(&self) -> Option<String> {
        // Always print using the generic form.
        let mut opts = IRPrinterOptions::new();
        opts.use_generic_form = true;
        let mut printer = IRPrinter::new_string_builder(opts);
        printer.print_root(self).unwrap();
        Some(printer.take_output_string().unwrap())
    }
}

// Override the printer for all ops object to call the dispatch.
impl<'b, T: OperationImpl<'b>> IRPrintableObject for T {
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        let generic_op = GenericOperation::make_from_data(self.get_context(), self.get_op_data());

        if printer.always_use_generic_form() {
            return printer.print_op_generic_form_impl(generic_op);
        }

        // Get the builtin interface or call the generic printer for unregistered ops.
        let builtin_interface = match generic_op.get_builtin_op_interface() {
            Some(int) => int,
            None => return printer.print_op_generic_form_impl(generic_op),
        };

        builtin_interface.custom_print(printer)
    }

    fn initialize_context_on_root(&self, printer: &mut IRPrinter) {
        printer.initialize_context_with_root_op(self.generic());
    }
}

fn parse_op_outputs(parser: &mut IRParser, st: &mut OperationParserState) -> Option<()> {
    // Parse the optional outputs.
    let mut outputs_names = vec![];
    if !(parser.next_token_is_string_literal() || parser.next_token_is_any_identifier()) {
        loop {
            outputs_names.push(parser.parse_value_ref()?);
            if !parser.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }
        parser
            .consume_sym_or_error(TokenValue::sym_assign())
            .unwrap();
    }
    st.set_outputs_names(outputs_names);

    Some(())
}

// Parse op in its generic form.
fn parse_op_generic_form(
    parser: &mut IRParser,
    ctx: &mut IRContext,
    mut st: OperationParserState,
) -> Option<OperationID> {
    // Parse the opname.
    let opname = parser
        .consume_string_literal_or_error()?
        .take_string_literal()
        .unwrap();
    st.set_raw_opname(opname);

    // Parse the inputs.
    let mut inputs_names = Vec::new();
    parser.consume_sym_or_error(TokenValue::sym_lparen())?;
    if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
        loop {
            inputs_names.push(parser.parse_value_ref()?);
            if !parser.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }
    }
    parser.consume_sym_or_error(TokenValue::sym_rparen())?;
    st.set_inputs_names(inputs_names);

    // Parse the optional attrs dict
    if !parser.next_token_is_sym(TokenValue::sym_colon()) {
        let attrs_dict = DictAttr::parse(parser)?;
        st.set_attrs_dict(attrs_dict);
    }
    parser.consume_sym_or_error(TokenValue::sym_colon())?;

    // Parse the inputs types.
    let mut inputs_types = Vec::new();
    parser.consume_sym_or_error(TokenValue::sym_lparen())?;
    if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
        loop {
            inputs_types.push(Type::parse(parser)?);
            if !parser.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }
    }
    parser.consume_sym_or_error(TokenValue::sym_rparen())?;
    st.set_inputs_types(inputs_types);

    // Parse the outputs types.
    parser.consume_sym_or_error(TokenValue::sym_deref());
    let mut outputs_types = Vec::new();
    parser.consume_sym_or_error(TokenValue::sym_lparen())?;
    if !parser.next_token_is_sym(TokenValue::sym_rparen()) {
        loop {
            outputs_types.push(Type::parse(parser)?);
            if !parser.try_consume_sym(TokenValue::sym_comma()) {
                break;
            }
        }
    }
    parser.consume_sym_or_error(TokenValue::sym_rparen())?;
    st.set_outputs_types(outputs_types);

    // Parse the blocks
    let mut blocks = Vec::new();
    if parser.try_consume_sym(TokenValue::sym_lcbracket()) {
        while !parser.try_consume_sym(TokenValue::sym_rcbracket()) {
            blocks.push(BlockID::parse_with_context(parser, ctx)?);
        }
    }
    st.set_blocks(blocks);

    parser.make_op_from_state(st, ctx)
}

// Override the parser for OperationID to call the dispatch.
impl IRParsableObjectWithContext for OperationID {
    fn parse_with_context(parser: &mut IRParser, ctx: &mut IRContext) -> Option<Self> {
        // Start parsing the outputs.
        let mut st = OperationParserState::new();
        st.set_beg_loc(parser.get_next_token_loc());
        parse_op_outputs(parser, &mut st);

        // If we force / have generic form parse it.
        if parser.opts().only_use_generic_form || parser.next_token_is_string_literal() {
            return parse_op_generic_form(parser, ctx, st);
        }

        // Here we parse using the constant implementation.
        let (opname, opname_loc) = parser.parse_scoped_identifier()?;
        // TODO: Get rid of the clone hack for the parser.
        // Without this ctx is captured twice and we get a compile error.
        let builtin_interface = match ctx.find_op_type_infos_from_opname(&opname) {
            Some(infos) => infos.cloned_builtin_interface(),
            None => {
                emit_error(
                    parser,
                    &opname_loc,
                    format!("No registered operation named `{}`", opname),
                );
                return None;
            }
        };

        // Parse.
        st.set_raw_opname(opname);
        builtin_interface.custom_parse(parser, ctx, &mut st)?;
        // Thend build the op
        parser.make_op_from_state(st, ctx)
    }
}

#[cfg(test)]
mod tests {

    use iostreams::location::Location;

    use super::*;
    use crate::{
        attributes::{DictAttr, FloatAttr, StringAttr},
        ir_data::ValueID,
        types::{FloatType, IntegerType, Type},
    };

    fn make_val(ctx: &mut IRContext, ty: Type) -> ValueID {
        let op = ctx._make_operation(
            Location::unknown_test_loc(),
            OperationTypeRef::Unknown("custom".to_owned()),
            vec![],
            vec![ty],
            DictAttr::empty(),
            vec![],
        );
        ctx.get_operation_data(op).outputs()[0]
    }

    #[test]
    fn test_make_val() {
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let val = make_val(&mut ctx, val_ty.clone());
        let op = ctx.get_value_data(val).get_opdef().unwrap();
        let op = ctx.get_generic_operation(op);
        assert_eq!(op.get_num_inputs(), 0);
        assert_eq!(op.get_num_outputs(), 1);
        assert_eq!(op.get_attrs_dict().size(), 0);
        assert_eq!(*op.get_output(0).get_type(), val_ty);
        assert_eq!(op.to_string_repr(), "%0 = \"custom\"() : () -> (i32)");
    }

    #[test]
    fn test_make_val_parser() {
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();

        let op = OperationID::from_string_repr_with_context(
            "%0 = \"custom\"() : () -> (i32)".to_string(),
            &mut ctx,
        );
        let op = ctx.get_generic_operation(op);

        assert_eq!(op.get_num_inputs(), 0);
        assert_eq!(op.get_num_outputs(), 1);
        assert_eq!(op.get_attrs_dict().size(), 0);
        assert_eq!(*op.get_output(0).get_type(), val_ty);
        assert_eq!(op.to_string_repr(), "%0 = \"custom\"() : () -> (i32)");
    }

    #[test]
    fn test_custom_ios() {
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let lhs = make_val(&mut ctx, val_ty.clone());
        let rhs = make_val(&mut ctx, val_ty.clone());

        // Build the op.
        let op = ctx._make_operation(
            Location::unknown_test_loc(),
            OperationTypeRef::Unknown("my_add".to_owned()),
            vec![lhs, rhs],
            vec![val_ty.clone()],
            DictAttr::empty(),
            vec![],
        );
        let op = ctx.get_generic_operation(op);

        // Check it
        assert_eq!(op.get_num_inputs(), 2);
        assert_eq!(op.get_num_outputs(), 1);
        assert_eq!(op.get_attrs_dict().size(), 0);
        assert_eq!(*op.get_output(0).get_type(), val_ty);
        assert_eq!(
            op.to_string_repr(),
            "%0 = \"my_add\"(<<UNKNOWN>, <<UNKNOWN>) : (i32, i32) -> (i32)"
        );
    }

    #[test]
    fn test_custom_no_ios() {
        // Build the op.
        let mut ctx = IRContext::new();
        let op = ctx._make_operation(
            Location::unknown_test_loc(),
            OperationTypeRef::Unknown("my_abort".to_owned()),
            vec![],
            vec![],
            DictAttr::empty(),
            vec![],
        );
        let op = ctx.get_generic_operation(op);

        // Check it
        assert_eq!(op.get_num_inputs(), 0);
        assert_eq!(op.get_num_outputs(), 0);
        assert_eq!(op.get_attrs_dict().size(), 0);
        assert_eq!(op.to_string_repr(), "\"my_abort\"() : () -> ()");
    }

    #[test]
    fn test_custom_ios_with_attrs() {
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();
        let lhs = make_val(&mut ctx, val_ty.clone());
        let rhs = make_val(&mut ctx, val_ty.clone());

        // Build the op.
        let attrs = DictAttr::new(vec![
            (
                StringAttr::new("foo".to_string()),
                FloatAttr::new(4.5, Type::Float(FloatType::F32)),
            ),
            (
                StringAttr::new("bar".to_string()),
                StringAttr::new("hello".to_string()),
            ),
        ]);
        let op = ctx._make_operation(
            Location::unknown_test_loc(),
            OperationTypeRef::Unknown("my_add".to_owned()),
            vec![lhs, rhs],
            vec![val_ty.clone()],
            attrs,
            vec![],
        );
        let op = ctx.get_generic_operation(op);

        // Check it.
        assert_eq!(op.get_num_inputs(), 2);
        assert_eq!(op.get_num_outputs(), 1);
        assert_eq!(op.get_attrs_dict().size(), 2);
        assert_eq!(*op.get_output(0).get_type(), val_ty);
        assert_eq!(op.to_string_repr(), "%0 = \"my_add\"(<<UNKNOWN>, <<UNKNOWN>) {\"foo\" = 4.5: f32, \"bar\" = \"hello\"} : (i32, i32) -> (i32)");
    }

    #[test]
    fn test_op_with_block() {
        let loc = Location::unknown_test_loc();
        let val_ty = IntegerType::new(32, None);
        let mut ctx = IRContext::new();

        let lhs = make_val(&mut ctx, val_ty.clone());
        let lhs_op = ctx.get_value_data(lhs).get_opdef().unwrap();
        let rhs = make_val(&mut ctx, val_ty.clone());
        let rhs_op = ctx.get_value_data(rhs).get_opdef().unwrap();

        // Build another op.
        let op = ctx._make_operation(
            loc,
            OperationTypeRef::Unknown("my_add".to_owned()),
            vec![lhs, rhs],
            vec![val_ty.clone()],
            DictAttr::empty(),
            vec![],
        );

        let block = ctx._make_block(loc, vec![], vec![lhs_op, rhs_op, op]);

        let fun_op = ctx._make_operation(
            loc,
            OperationTypeRef::Unknown("my_fun".to_owned()),
            vec![],
            vec![],
            DictAttr::empty(),
            vec![block],
        );
        let fun_op = ctx.get_generic_operation(fun_op);

        // Check it
        assert_eq!(fun_op.get_num_blocks(), 1);
        assert_eq!(fun_op.to_string_repr(), "\"my_fun\"() : () -> () {\n    ^() {\n        %0 = \"custom\"() : () -> (i32)\n        %1 = \"custom\"() : () -> (i32)\n        %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n    }\n    \n}");
    }

    #[test]
    fn test_op_with_block_parse() {
        let mut ctx = IRContext::new();

        let fun_op = OperationID::from_string_repr_with_context(
            "\"my_fun\"() : () -> () {\n    ^() {\n        %0 = \"custom\"() : () -> (i32)\n        %1 = \"custom\"() : () -> (i32)\n        %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n    }\n    \n}".to_string(),
            &mut ctx,
        );
        let fun_op = ctx.get_generic_operation(fun_op);

        // Check it
        assert_eq!(fun_op.get_num_blocks(), 1);
        assert_eq!(fun_op.to_string_repr(), "\"my_fun\"() : () -> () {\n    ^() {\n        %0 = \"custom\"() : () -> (i32)\n        %1 = \"custom\"() : () -> (i32)\n        %2 = \"my_add\"(%0, %1) : (i32, i32) -> (i32)\n    }\n    \n}");
    }
}
