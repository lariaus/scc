// Implement basic visitor helper trait for all AST methods.

use crate::ast::{
    ASTNode, ASTNodeBinopExpr, ASTNodeBlockStatement, ASTNodeCharLiteral, ASTNodeDeclsList,
    ASTNodeFloatLiteral, ASTNodeFunctionDecl, ASTNodeImpl, ASTNodeIntLiteral, ASTNodeLabelExpr,
    ASTNodeLabelType, ASTNodeReturnStatement, ASTNodeStringLiteral, ASTNodeVarDeclStatement,
};

pub trait MutableASTVisitor: Sized {
    // Call `visit_mut` on all children of a node.
    fn visit_node_children<T: ASTNodeImpl>(&mut self, node: &mut T) {
        for children in node.get_children_mut() {
            children.visit_mut(self);
        }
    }

    // Base function called when `x.visit` is called.
    // Will dispatch automatically to the right function.
    fn visit_generic_node(&mut self, node: &mut ASTNode) {
        match node {
            ASTNode::IntLiteral(node) => self.visit_int_literal(node),
            ASTNode::FloatLiteral(node) => self.visit_float_literal(node),
            ASTNode::StringLiteral(node) => self.visit_string_literal(node),
            ASTNode::CharLiteral(node) => self.visit_char_literal(node),
            ASTNode::LabelExpr(node) => self.visit_label_expr(node),
            ASTNode::BinopExpr(node) => self.visit_binop_expr(node),
            ASTNode::BlockStatement(node) => self.visit_block_statement(node),
            ASTNode::ReturnStatement(node) => self.visit_return_statement(node),
            ASTNode::VarDeclStatement(node) => self.visit_var_decl_statement(node),
            ASTNode::LabelType(node) => self.visit_label_type(node),
            ASTNode::DeclsList(node) => self.visit_decls_list(node),
            ASTNode::FunctionDecl(node) => self.visit_function_decl(node),
        }
    }

    // Implementation for all type nodes
    // By default, they all just call `visit_node_children`

    // FunctionDecl(ASTNodeFunctionDecl),

    fn visit_int_literal(&mut self, node: &mut ASTNodeIntLiteral) {
        self.visit_node_children(node);
    }

    fn visit_float_literal(&mut self, node: &mut ASTNodeFloatLiteral) {
        self.visit_node_children(node);
    }

    fn visit_string_literal(&mut self, node: &mut ASTNodeStringLiteral) {
        self.visit_node_children(node);
    }

    fn visit_char_literal(&mut self, node: &mut ASTNodeCharLiteral) {
        self.visit_node_children(node);
    }

    fn visit_label_expr(&mut self, node: &mut ASTNodeLabelExpr) {
        self.visit_node_children(node);
    }

    fn visit_binop_expr(&mut self, node: &mut ASTNodeBinopExpr) {
        self.visit_node_children(node);
    }

    fn visit_block_statement(&mut self, node: &mut ASTNodeBlockStatement) {
        self.visit_node_children(node);
    }

    fn visit_return_statement(&mut self, node: &mut ASTNodeReturnStatement) {
        self.visit_node_children(node);
    }

    fn visit_var_decl_statement(&mut self, node: &mut ASTNodeVarDeclStatement) {
        self.visit_node_children(node);
    }

    fn visit_label_type(&mut self, node: &mut ASTNodeLabelType) {
        self.visit_node_children(node);
    }

    fn visit_decls_list(&mut self, node: &mut ASTNodeDeclsList) {
        self.visit_node_children(node);
    }

    fn visit_function_decl(&mut self, node: &mut ASTNodeFunctionDecl) {
        self.visit_node_children(node);
    }
}
