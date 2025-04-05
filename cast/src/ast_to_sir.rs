use std::collections::HashMap;

use diagnostics::{diagnostics::CompilerDiagnostics, result::CompilerResult};
use sir_core::operation::OperationImpl;
use sir_core::{
    attributes::IntegerAttr,
    ir_builder::{IRBuilder, InsertionPoint},
    ir_context::IRContext,
    ir_data::{OperationID, ValueID},
    types::{FunctionType, IntegerType, PointerType},
};
use sir_func::func_ops::{register_func_ops, FunctionOp, ModuleOp, ReturnOp};
use sir_math::math_ops::{register_math_ops, MathIAddOp};
use sir_mem::mem_ops::{register_mem_ops, MemAllocaOp, MemLoadOp, MemStoreOp};

use crate::{
    ast::{ASTNode, ASTNodeImpl, ASTVarDeclUID},
    ast_types,
    ast_vistors::ASTVisitor,
};

// Options needed to support the convertion from CAST to SIR.
#[derive(Debug, Clone, Copy)]
pub struct CASTToSIROptions {
    char_bitwidth: usize,
    short_bitwidth: usize,
    int_bitwidth: usize,
    long_bitwidth: usize,
}

impl CASTToSIROptions {
    // Create the default options for CASTToSIR.
    pub fn new() -> Self {
        Self {
            char_bitwidth: 8,
            short_bitwidth: 16,
            int_bitwidth: 32,
            long_bitwidth: 64,
        }
    }
}

/// Convert CAST types to SIR types.
struct CASTTypeConverter {
    opts: CASTToSIROptions,
}

impl CASTTypeConverter {
    pub fn new(opts: CASTToSIROptions) -> Self {
        Self { opts }
    }

    /// Convert `ty` to SIR
    pub fn convert_type(&self, ty: &ast_types::Type) -> sir_core::types::Type {
        match ty {
            ast_types::Type::Integer(integer_type) => self.convert_int_type(integer_type),
            ast_types::Type::Float(float_type) => self.convert_float_type(float_type),
            ast_types::Type::Void => panic!("No direct support for conversion to void type"),
            ast_types::Type::Error => unreachable!(),
        }
    }

    pub fn convert_int_type(&self, ty: &ast_types::IntegerType) -> sir_core::types::Type {
        let bitwidth = match ty.size() {
            ast_types::IntegerSize::Char => self.opts.char_bitwidth,
            ast_types::IntegerSize::Short => self.opts.short_bitwidth,
            ast_types::IntegerSize::Int => self.opts.int_bitwidth,
            ast_types::IntegerSize::Long => self.opts.long_bitwidth,
        };
        let signed = match ty.signedness() {
            ast_types::IntegerSignedness::Signed => Some(true),
            ast_types::IntegerSignedness::Unsigned => Some(false),
            ast_types::IntegerSignedness::Signless => None,
        };

        sir_core::types::IntegerType::new(bitwidth, signed)
    }

    pub fn convert_float_type(&self, ty: &ast_types::FloatType) -> sir_core::types::Type {
        match ty {
            ast_types::FloatType::F32 => sir_core::types::FloatType::f32(),
            ast_types::FloatType::F64 => sir_core::types::FloatType::f64(),
        }
    }
}

/// STruct that converts an AST object to an SIR Module object.
pub struct ASTToSIR {
    opts: CASTToSIROptions,
}

struct ModuleBuilder<'a> {
    builder: IRBuilder<'a>,
    module: OperationID,
    diagnostics: CompilerDiagnostics,
    type_converter: CASTTypeConverter,
    vals_map: HashMap<ASTVarDeclUID, ValueID>,
    converted_val: Option<ValueID>,
}

impl<'a> ModuleBuilder<'a> {
    fn convert_value(&mut self, val: &ASTNode) -> ValueID {
        assert!(val.is_expr(), "only expr can be converted");
        let old_val = self.converted_val;
        self.converted_val = None;
        val.visit(self);
        let res = self.converted_val.expect("visit didn't set the value");
        self.converted_val = old_val;
        res
    }
}

impl<'a> ASTVisitor for ModuleBuilder<'a> {
    fn visit_int_literal(&mut self, _node: &crate::ast::ASTNodeIntLiteral) {
        todo!("support AST To SIR: int literal");
    }

    fn visit_float_literal(&mut self, _node: &crate::ast::ASTNodeFloatLiteral) {
        todo!("support AST To SIR: float literal");
    }

    fn visit_string_literal(&mut self, _node: &crate::ast::ASTNodeStringLiteral) {
        todo!("support AST To SIR: string literal");
    }

    fn visit_char_literal(&mut self, _node: &crate::ast::ASTNodeCharLiteral) {
        todo!("support AST To SIR: char literal");
    }

    fn visit_label_expr(&mut self, node: &crate::ast::ASTNodeLabelExpr) {
        // If we visit a label expr, we can be sure we are actually looking for a value (variables).
        let uid = node.var_decl().uid();
        let val = *self.vals_map.get(&uid).expect("value not found");

        // Variables are stored in the stack, and must be loaded.
        let elem_ty = self
            .builder
            .get_value(val)
            .get_type()
            .cast::<PointerType>()
            .unwrap()
            .element()
            .clone();
        let val = self
            .builder
            .create_op(node.get_loc(), MemLoadOp::build(val, elem_ty))
            .get_result()
            .as_id();

        self.converted_val = Some(val);
    }

    fn visit_binop_expr(&mut self, node: &crate::ast::ASTNodeBinopExpr) {
        // Convert the operands.
        let lhs = self.convert_value(node.lhs());
        let rhs = self.convert_value(node.rhs());
        let out_type = self.builder.get_value(lhs).get_type().clone();

        // Then build the final op.
        self.converted_val = Some(match node.kind() {
            crate::ast::ASTNodeBinopExprKind::Add => self
                .builder
                .create_op(node.get_loc(), MathIAddOp::build(lhs, rhs, out_type))
                .get_result()
                .as_id(),
        });
    }

    fn visit_block_statement(&mut self, node: &crate::ast::ASTNodeBlockStatement) {
        // Just visit all children.
        self.visit_node_children(node);
    }

    fn visit_return_statement(&mut self, node: &crate::ast::ASTNodeReturnStatement) {
        let val = self.convert_value(node.value());
        self.builder
            .create_op(node.get_loc(), ReturnOp::build(&[val]));
    }

    fn visit_var_decl_statement(&mut self, _node: &crate::ast::ASTNodeVarDeclStatement) {
        todo!("support AST To SIR: var decl");
    }

    fn visit_label_type(&mut self, _node: &crate::ast::ASTNodeLabelType) {
        unreachable!("CAST type should be converted with the TypeConverter");
    }

    fn visit_decls_list(&mut self, node: &crate::ast::ASTNodeDeclsList) {
        // Convert all declaration nodes.
        self.visit_node_children(node);
    }

    fn visit_function_decl(&mut self, node: &crate::ast::ASTNodeFunctionDecl) {
        let loc = node.get_loc();

        // 1) Convert the function type.
        // Start by converting the arguments.
        let args_types: Vec<sir_core::types::Type> = node
            .args_types()
            .iter()
            .map(|arg| self.type_converter.convert_type(arg.get_type_value()))
            .collect();

        // Then convert the result type.
        let mut result_types = Vec::new();
        let res_ast_ty = node.return_type().get_type_value();
        if !res_ast_ty.is_void() {
            result_types.push(self.type_converter.convert_type(res_ast_ty));
        }

        let fn_type = FunctionType::new(args_types, result_types);

        // 2) Create the function op and its block.

        // Create the function.
        self.builder.set_insertion_point(InsertionPoint::AtEndOf(
            self.builder.get_operation(self.module).get_block(0).as_id(),
        ));
        let fun_op = self
            .builder
            .create_op(loc, FunctionOp::build(node.label(), fn_type.clone()))
            .as_id();

        // Create the block.
        let fn_type = fn_type.cast::<FunctionType>().unwrap();
        let block = self
            .builder
            .create_block_at_begin_of(fun_op, loc, fn_type.arguments());
        let block_args = block.get_operands_ids().to_owned();
        let block = block.as_id();
        self.builder
            .set_insertion_point(InsertionPoint::AtEndOf(block));

        // 3) Allocate the operands on the stack and store the values.
        for (idx, (arg_decl, arg_ty)) in node
            .get_args_bindings()
            .iter()
            .zip(fn_type.arguments())
            .enumerate()
        {
            // Fine for now as we don't use align anyway.
            let arg_align = 4;
            let arg_align = IntegerAttr::new(arg_align, IntegerType::index_type());
            let arg_ptr_ty = sir_core::types::PointerType::new(arg_ty.clone());

            // Allocate the value.
            let alloc_op = self
                .builder
                .create_op(loc, MemAllocaOp::build(arg_align, arg_ptr_ty));
            let alloc_ptr = alloc_op.get_result().as_id();
            self.vals_map.insert(arg_decl.uid(), alloc_ptr);

            // Store the argument.
            let arg_val = block_args[idx];
            self.builder
                .create_op(loc, MemStoreOp::build(arg_val, alloc_ptr));
        }

        // 4) Convert the function body.
        if let Some(body) = node.get_body() {
            body.visit(self);
        }

        // 5) In CAST the return statement is optional.
        // Add one if there isn't.
        let block = self.builder.get_block(block);
        if block
            .get_terminator_op()
            .is_none_or(|op| !op.isa::<ReturnOp>())
        {
            self.builder.create_op(loc, ReturnOp::build(&[]));
        }
    }
}

impl ASTToSIR {
    // Create the ASTToSIR object with the predefined options.
    pub fn new(opts: CASTToSIROptions) -> Self {
        Self { opts }
    }

    /// Lower `root` AST node to a new module object.
    /// Returns the created module if conversation succeeded.
    /// Returns an error otherhwise.
    pub fn convert_to_sir(
        &self,
        root: &ASTNode,
        ctx: &mut IRContext,
    ) -> CompilerResult<OperationID> {
        // Let's start by building the builder and the module object.
        let mut builder = IRBuilder::new(ctx);
        let module = builder
            .create_detached_op(root.get_loc(), ModuleOp::build())
            .as_id();
        builder.create_block_at_begin_of(module, root.get_loc(), []);

        // Build the module builder.
        let diagnostics = CompilerDiagnostics::new();
        let mut builder = ModuleBuilder {
            builder,
            module,
            diagnostics,
            type_converter: CASTTypeConverter::new(self.opts),
            vals_map: HashMap::new(),
            converted_val: None,
        };

        // Run the conversion.
        root.visit(&mut builder);

        // Returns the output.
        CompilerResult::make(builder.diagnostics, Some(builder.module))
    }

    // Setup the SIR context to be ready for the conversion.
    pub fn setup_context(&self, ctx: &mut IRContext) {
        register_func_ops(ctx);
        register_math_ops(ctx);
        register_mem_ops(ctx);
    }
}
