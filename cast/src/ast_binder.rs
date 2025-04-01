// The AST Binder is responsible for binding types and values use / decls.
// It also ensures the semantics of the program is correct (and returns an error if it's not).

use std::collections::HashMap;

use diagnostics::{emit_error, CompilerDiagnostics, CompilerDiagnosticsEmitter};
use iostreams::location::Location;

use crate::{
    ast::{ASTNode, ASTNodeImpl, ASTTypeDecl, ASTTypeDeclUID, ASTVarDecl, ASTVarDeclUID},
    ast_types::{self, FloatType, IntegerSignedness, IntegerSize, IntegerType, Type},
    ast_vistors::MutableASTVisitor,
};

// Data associated to each scope.
struct ScopeData {
    vars: HashMap<String, ASTVarDecl>,
    types: HashMap<String, ASTTypeDecl>,
}

// Scoped map implementation for the declarations in the AST.
struct ScopedBindingsMap {
    levels: Vec<ScopeData>,
}

// Informations about the function being analyzed.
struct FunctionData {
    ret_type: Type,
}

impl ScopedBindingsMap {
    pub fn new() -> Self {
        Self { levels: Vec::new() }
    }

    // Start a new scope.
    fn open_scope(&mut self) {
        self.levels.push(ScopeData {
            vars: HashMap::new(),
            types: HashMap::new(),
        });
    }

    // Close the last scope.
    fn close_scope(&mut self) {
        assert!(self.levels.len() > 0);
        self.levels.pop();
    }

    // Declare a new variable.
    fn declare_variable(&mut self, name: &str, ty: Type) -> Result<&ASTVarDecl, String> {
        assert!(self.levels.len() > 0);
        let var_decl = ASTVarDecl::new(ASTVarDeclUID::new(), ty);

        let vars_map = &mut self.levels.last_mut().unwrap().vars;
        match vars_map.insert(name.to_string(), var_decl) {
            Some(_old) => return Err(format!("Redefinition of variable `{}`", name)),
            None => {}
        }

        Ok(vars_map.get(name).unwrap())
    }

    // Declare a new type.
    fn declare_type(&mut self, name: &str, ty: Type) -> Result<&ASTTypeDecl, String> {
        assert!(self.levels.len() > 0);
        let type_decl = ASTTypeDecl::new(ASTTypeDeclUID::new(), ty);

        let types_map = &mut self.levels.last_mut().unwrap().types;
        match types_map.insert(name.to_string(), type_decl) {
            Some(_old) => return Err(format!("Redefinition of type `{}`", name)),
            None => {}
        }

        Ok(types_map.get(name).unwrap())
    }

    // Find the variable definition by name.
    fn find_variable(&self, name: &str) -> Result<&ASTVarDecl, String> {
        for level in self.levels.iter().rev() {
            if let Some(decl) = level.vars.get(name) {
                return Ok(decl);
            }
        }

        Err(format!("Variable `{}` is undefined", name))
    }

    // Find the type definition by name.
    fn find_type(&self, name: &str) -> Result<&ASTTypeDecl, String> {
        for level in self.levels.iter().rev() {
            if let Some(decl) = level.types.get(name) {
                return Ok(decl);
            }
        }

        Err(format!("Type `{}` is undefined", name))
    }
}

pub struct ASTBinder {
    diagnostics: CompilerDiagnostics,
    decls_map: ScopedBindingsMap,
    err_var_decl: ASTVarDecl,
    err_type_decl: ASTTypeDecl,
    curr_fun: Option<FunctionData>,
}

impl ASTBinder {
    pub fn new() -> Self {
        Self {
            diagnostics: CompilerDiagnostics::new(),
            decls_map: ScopedBindingsMap::new(),
            err_var_decl: ASTVarDecl::new(ASTVarDeclUID::new(), Type::Error),
            err_type_decl: ASTTypeDecl::new(ASTTypeDeclUID::new(), Type::Error),
            curr_fun: None,
        }
    }

    // Running the bindings on the full AST.
    pub fn bind(&mut self, root: &mut ASTNode) {
        self.decls_map.open_scope();
        self._add_builtin_c_decls(root.get_loc());
        root.visit_mut(self);
        self.decls_map.close_scope();
        assert!(
            self.decls_map.levels.is_empty(),
            "All scopes should have been closed"
        );
    }

    // Add builtin vars / types defined in C.
    fn _add_builtin_c_decls(&mut self, root_loc: Location) {
        // Declare bultin integer types.
        self.declare_type(
            &ast_types::TYPENAME_CHAR,
            Type::Integer(IntegerType::new(
                IntegerSize::Char,
                IntegerSignedness::Signless,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_SIGNED_CHAR,
            Type::Integer(IntegerType::new(
                IntegerSize::Char,
                IntegerSignedness::Signed,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_UNSIGNED_CHAR,
            Type::Integer(IntegerType::new(
                IntegerSize::Char,
                IntegerSignedness::Unsigned,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_SHORT,
            Type::Integer(IntegerType::new(
                IntegerSize::Short,
                IntegerSignedness::Signless,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_SIGNED_SHORT,
            Type::Integer(IntegerType::new(
                IntegerSize::Short,
                IntegerSignedness::Signed,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_UNSIGNED_SHORT,
            Type::Integer(IntegerType::new(
                IntegerSize::Short,
                IntegerSignedness::Unsigned,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_INT,
            Type::Integer(IntegerType::new(
                IntegerSize::Int,
                IntegerSignedness::Signless,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_SIGNED_INT,
            Type::Integer(IntegerType::new(
                IntegerSize::Int,
                IntegerSignedness::Signed,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_UNSIGNED_INT,
            Type::Integer(IntegerType::new(
                IntegerSize::Int,
                IntegerSignedness::Unsigned,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_LONG,
            Type::Integer(IntegerType::new(
                IntegerSize::Long,
                IntegerSignedness::Signless,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_SIGNED_LONG,
            Type::Integer(IntegerType::new(
                IntegerSize::Long,
                IntegerSignedness::Signed,
            )),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_UNSIGNED_LONG,
            Type::Integer(IntegerType::new(
                IntegerSize::Long,
                IntegerSignedness::Unsigned,
            )),
            root_loc,
        );

        // Declare builtin float types.
        self.declare_type(
            &ast_types::TYPENAME_FLOAT,
            Type::Float(FloatType::F32),
            root_loc,
        );
        self.declare_type(
            &ast_types::TYPENAME_DOUBLE,
            Type::Float(FloatType::F64),
            root_loc,
        );

        // Declare builtin void type.
        self.declare_type(&ast_types::TYPENAME_VOID, Type::Void, root_loc);
    }

    fn _typecheck_binop(
        &mut self,
        lhs_ty: &Type,
        lhs_loc: Location,
        rhs_ty: &Type,
        rhs_loc: Location,
        op_loc: Location,
    ) -> Type {
        if lhs_ty.is_error() || rhs_ty.is_error() {
            return Type::Error;
        }

        // We only support int or float types.
        match lhs_ty {
            Type::Integer(_) | Type::Float(_) => {}
            _ => {
                emit_error(
                    self,
                    lhs_loc,
                    format!(
                        "Binop lhs operand expects integer of float types, but got `{}`",
                        lhs_ty
                    ),
                );
                return Type::Error;
            }
        }
        match rhs_ty {
            Type::Integer(_) | Type::Float(_) => {}
            _ => {
                emit_error(
                    self,
                    rhs_loc,
                    format!(
                        "Binop rhs operand expects integer of float types, but got `{}`",
                        rhs_ty
                    ),
                );
                return Type::Error;
            }
        }

        // TODO: Do proper type cast for binop ops with different types.

        if lhs_ty != rhs_ty {
            emit_error(
                self,
                op_loc,
                format!(
                    "Binop operand types differs: lhs is {}, rhs is {}",
                    lhs_ty, rhs_ty
                ),
            );
            return Type::Error;
        }

        lhs_ty.clone()
    }

    // Verify that exp_type == real_type.
    // Throws an error if types differ
    fn _check_types_match(&mut self, exp_type: &Type, real_type: &Type, loc: Location) {
        if !exp_type.is_error() && !real_type.is_error() && exp_type != real_type {
            emit_error(
                self,
                loc,
                format!(
                    "Invalid type: expected `{}` but got `{}`",
                    exp_type, real_type
                ),
            );
        }
    }

    // Wrapper around decls_map.declare_variable to handle errors.
    fn declare_variable(&mut self, name: &str, ty: Type, loc: Location) -> ASTVarDecl {
        match self.decls_map.declare_variable(name, ty) {
            Ok(decl) => decl.clone(),
            Err(err) => {
                emit_error(self, loc, err);
                self.err_var_decl.clone()
            }
        }
    }

    // Wrapper around decls_map.declare_type to handle errors.
    fn declare_type(&mut self, name: &str, ty: Type, loc: Location) -> ASTTypeDecl {
        match self.decls_map.declare_type(name, ty) {
            Ok(decl) => decl.clone(),
            Err(err) => {
                emit_error(self, loc, err);
                self.err_type_decl.clone()
            }
        }
    }

    // Wrapper around decls_map.find_variable to handle errors.
    fn find_variable(&mut self, name: &str, loc: Location) -> ASTVarDecl {
        match self.decls_map.find_variable(name) {
            Ok(decl) => decl.clone(),
            Err(err) => {
                emit_error(self, loc, err);
                self.err_var_decl.clone()
            }
        }
    }

    // Wrapper around decls_map.find_type to handle errors.
    fn find_type(&mut self, name: &str, loc: Location) -> ASTTypeDecl {
        match self.decls_map.find_type(name) {
            Ok(decl) => decl.clone(),
            Err(err) => {
                emit_error(self, loc, err);
                self.err_type_decl.clone()
            }
        }
    }
}

impl CompilerDiagnosticsEmitter for ASTBinder {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        &mut self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        "binder"
    }
}

impl MutableASTVisitor for ASTBinder {
    fn visit_int_literal(&mut self, node: &mut crate::ast::ASTNodeIntLiteral) {
        // By default we set it to int.
        node.set_expr_type(Type::Integer(IntegerType::new(
            IntegerSize::Int,
            IntegerSignedness::Signless,
        )));
    }

    fn visit_float_literal(&mut self, node: &mut crate::ast::ASTNodeFloatLiteral) {
        // By default we set it to f64.
        node.set_expr_type(Type::Float(FloatType::F64));
    }

    fn visit_string_literal(&mut self, _node: &mut crate::ast::ASTNodeStringLiteral) {
        todo!("Unsupported string type");
    }

    fn visit_char_literal(&mut self, node: &mut crate::ast::ASTNodeCharLiteral) {
        // By default we set it to char.
        node.set_expr_type(Type::Integer(IntegerType::new(
            IntegerSize::Char,
            IntegerSignedness::Signless,
        )));
    }

    fn visit_label_expr(&mut self, node: &mut crate::ast::ASTNodeLabelExpr) {
        let var_decl = self.find_variable(node.label(), node.get_loc());
        node.set_var_decl(var_decl);
    }

    fn visit_binop_expr(&mut self, node: &mut crate::ast::ASTNodeBinopExpr) {
        self.visit_node_children(node);
        let lhs_ty = node.lhs().get_expr_type();
        let rhs_ty = node.rhs().get_expr_type();
        let res_ty = self._typecheck_binop(
            lhs_ty,
            node.lhs().get_loc(),
            rhs_ty,
            node.rhs().get_loc(),
            node.get_loc(),
        );
        node.set_expr_type(res_ty);
    }

    fn visit_block_statement(&mut self, node: &mut crate::ast::ASTNodeBlockStatement) {
        self.visit_node_children(node);
    }

    fn visit_return_statement(&mut self, node: &mut crate::ast::ASTNodeReturnStatement) {
        self.visit_node_children(node);

        // Check if the expr type match the result type of the function.
        // I think it's impossible to have return outside of function ?
        assert!(self.curr_fun.is_some());
        let exp_ty = self.curr_fun.as_ref().unwrap().ret_type.clone();
        let val_ty = node.value().get_expr_type();
        self._check_types_match(&exp_ty, val_ty, node.get_loc());
    }

    fn visit_var_decl_statement(&mut self, node: &mut crate::ast::ASTNodeVarDeclStatement) {
        // First visit all the inits.
        self.visit_node_children(node);

        // Find all the var bindings while checking the types
        let mut bindings = Vec::new();

        for i in 0..node.decls_count() {
            let (var_name, var_type, var_init, var_dims) = node.get_var_decl(i);

            let exp_type = var_type.get_type_value();

            if let Some(var_init) = var_init {
                // Type check the init type if available.
                let init_type = var_init.get_expr_type();
                self._check_types_match(exp_type, init_type, node.get_loc());
            }

            if var_dims.len() > 0 {
                emit_error(
                    self,
                    node.get_loc(),
                    format!("Array variable declarations not supported yet (TODO)"),
                );
            }

            let var_decl = self.declare_variable(var_name, exp_type.clone(), node.get_loc());
            bindings.push(var_decl);
        }

        node.set_var_decls_bindings(bindings);
    }

    fn visit_label_type(&mut self, node: &mut crate::ast::ASTNodeLabelType) {
        let type_decl = self.find_type(node.label(), node.get_loc());
        node.set_type_decl(type_decl);
    }

    fn visit_decls_list(&mut self, node: &mut crate::ast::ASTNodeDeclsList) {
        self.visit_node_children(node);
    }

    fn visit_function_decl(&mut self, node: &mut crate::ast::ASTNodeFunctionDecl) {
        // First visit the args / types defs
        for arg in node.args_types_mut() {
            arg.visit_mut(self);
        }
        node.return_type_mut().visit_mut(self);

        // Stop if there is no body.
        if !node.has_body() {
            return;
        }
        // Add the arguments declarations.
        let mut args_bindings = Vec::new();
        self.decls_map.open_scope();
        for (arg_name, arg_ty) in node.args_names().iter().zip(node.args_types()) {
            let arg_loc = arg_ty.get_loc();
            let arg_ty = arg_ty.get_type_value().clone();
            let arg_decl = self.declare_variable(&arg_name, arg_ty, arg_loc);
            args_bindings.push(arg_decl);
        }
        node.set_args_bindings(args_bindings);

        // Add the function info.
        // Impossible to have recursive functions given the parser ?
        assert!(self.curr_fun.is_none());
        self.curr_fun = Some(FunctionData {
            ret_type: node.return_type().get_type_value().clone(),
        });

        // Visit the body.
        node.get_body_mut().unwrap().visit_mut(self);

        self.decls_map.close_scope();
    }
}
