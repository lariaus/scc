use std::io::stderr;

use global_counter::primitive::exact::CounterUsize;
use iostreams::location::Location;
use utils::stringutils::encode_string_literal;

use crate::{
    ast_printer::{ASTPrinter, ASTPrinterOptions},
    ast_types::Type,
    ast_vistors::{ASTVisitor, MutableASTVisitor},
};

////////////////////////////////////////////////
/// Base AST Node
////////////////////////////////////////////////

// Unique identifier for the nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ASTNodeUID(usize);

static AST_NODE_UID_COUNTER: CounterUsize = CounterUsize::new(0);

impl ASTNodeUID {
    pub fn new() -> Self {
        Self(AST_NODE_UID_COUNTER.inc())
    }
}

// Unique identifier for the type declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ASTTypeDeclUID(usize);

static AST_TYPE_DECL_UID_COUNTER: CounterUsize = CounterUsize::new(0);

impl ASTTypeDeclUID {
    pub fn new() -> Self {
        Self(AST_TYPE_DECL_UID_COUNTER.inc())
    }
}

// Represent the declaration of a new type in C.
// Any AST node that refers to the declaration will have this object.
#[derive(Debug, Clone)]
pub struct ASTTypeDecl {
    uid: ASTTypeDeclUID,
    ty: Type,
}

impl ASTTypeDecl {
    pub fn new(uid: ASTTypeDeclUID, ty: Type) -> Self {
        Self { uid, ty }
    }
    pub fn uid(&self) -> ASTTypeDeclUID {
        self.uid
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

// Unique identifier for the variable declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ASTVarDeclUID(usize);

static AST_VAR_DECL_UID_COUNTER: CounterUsize = CounterUsize::new(0);

impl ASTVarDeclUID {
    pub fn new() -> Self {
        Self(AST_VAR_DECL_UID_COUNTER.inc())
    }
}

// Represent the declaration of a new variable in C.
// Any AST node that refers to the declaration will have this object.
#[derive(Debug, Clone)]
pub struct ASTVarDecl {
    uid: ASTVarDeclUID,
    ty: Type,
}

impl ASTVarDecl {
    pub fn new(uid: ASTVarDeclUID, ty: Type) -> Self {
        Self { uid, ty }
    }
    pub fn uid(&self) -> ASTVarDeclUID {
        self.uid
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

// The kind of node for the AST.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ASTNodeKind {
    Decl,
    Expr,
    Statement,
    Type,
}

// Base class for the AST type.
pub enum ASTNode {
    IntLiteral(ASTNodeIntLiteral),
    FloatLiteral(ASTNodeFloatLiteral),
    StringLiteral(ASTNodeStringLiteral),
    CharLiteral(ASTNodeCharLiteral),
    LabelExpr(ASTNodeLabelExpr),
    BinopExpr(ASTNodeBinopExpr),
    BlockStatement(ASTNodeBlockStatement),
    ReturnStatement(ASTNodeReturnStatement),
    VarDeclStatement(ASTNodeVarDeclStatement),
    LabelType(ASTNodeLabelType),
    DeclsList(ASTNodeDeclsList),
    FunctionDecl(ASTNodeFunctionDecl),
}

// Implementation needed by all AST children.
pub trait ASTNodeImpl {
    // Get the node identifier.
    fn get_uid(&self) -> ASTNodeUID;

    // Get the node kind
    fn get_kind(&self) -> ASTNodeKind;

    // Returns the location of the node.
    fn get_loc(&self) -> Location;

    // Returns the children of the nodes.
    fn get_children(&self) -> &[ASTNode];
    fn get_children_mut(&mut self) -> &mut [ASTNode];

    // Pretty-print the current node.
    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error>;

    // The type of an expr for all expr nodes.
    // Called on any other AST node, it will return an error.
    fn get_expr_type(&self) -> &Type {
        assert!(
            self.get_kind() == ASTNodeKind::Expr,
            "should only be called on AST objects of kind Expr"
        );
        panic!(
            "Missing `get_expr_type` for Expr AST Node `{}`",
            self.to_string_repr()
        );
    }

    // The type for all type nodes.
    // Called on any other AST Node, it will return an error.
    fn get_type_value(&self) -> &Type {
        assert!(
            self.get_kind() == ASTNodeKind::Type,
            "should only be called on AST objects of kind Type"
        );
        panic!(
            "Missing `get_type_value` for Type AST Node `{}`",
            self.to_string_repr()
        );
    }

    // Write the AST representation to a string.
    fn to_string_repr(&self) -> String {
        let mut buf = Vec::new();
        let mut printer = ASTPrinter::new(ASTPrinterOptions::new(), &mut buf);
        self.print(&mut printer).unwrap();
        String::from_utf8(buf).unwrap()
    }
}

impl ASTNode {
    // Print the current node to stderr.
    pub fn dump(&self) {
        self.dump_to(&mut stderr());
    }

    // Print the current node to a writer
    pub fn dump_to<T: std::io::Write>(&self, os: &mut T) {
        let mut printer = ASTPrinter::new(ASTPrinterOptions::new(), os);
        printer.print(self).unwrap();
        write!(os, "\n").unwrap();
    }

    // Returns true if the node is of kind declaration.
    pub fn is_decl(&self) -> bool {
        self.get_kind() == ASTNodeKind::Decl
    }

    // Returns true if the node is of kind expression.
    pub fn is_expr(&self) -> bool {
        self.get_kind() == ASTNodeKind::Expr
    }

    // Returns true if the node is of kind statement.
    pub fn is_statement(&self) -> bool {
        self.get_kind() == ASTNodeKind::Statement
    }

    // Returns true if the node is of kind type.
    pub fn is_type(&self) -> bool {
        self.get_kind() == ASTNodeKind::Type
    }

    // Visit the node (might mutable the AST).
    pub fn visit_mut<V: MutableASTVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_generic_node(self);
    }

    // Visit the node.
    pub fn visit<V: ASTVisitor>(&self, visitor: &mut V) {
        visitor.visit_generic_node(self);
    }
}

impl ASTNodeImpl for ASTNode {
    fn get_uid(&self) -> ASTNodeUID {
        match self {
            ASTNode::IntLiteral(node) => node.get_uid(),
            ASTNode::FloatLiteral(node) => node.get_uid(),
            ASTNode::StringLiteral(node) => node.get_uid(),
            ASTNode::CharLiteral(node) => node.get_uid(),
            ASTNode::LabelExpr(node) => node.get_uid(),
            ASTNode::BinopExpr(node) => node.get_uid(),
            ASTNode::BlockStatement(node) => node.get_uid(),
            ASTNode::ReturnStatement(node) => node.get_uid(),
            ASTNode::VarDeclStatement(node) => node.get_uid(),
            ASTNode::LabelType(node) => node.get_uid(),
            ASTNode::DeclsList(node) => node.get_uid(),
            ASTNode::FunctionDecl(node) => node.get_uid(),
        }
    }

    fn get_kind(&self) -> ASTNodeKind {
        match self {
            ASTNode::IntLiteral(node) => node.get_kind(),
            ASTNode::FloatLiteral(node) => node.get_kind(),
            ASTNode::StringLiteral(node) => node.get_kind(),
            ASTNode::CharLiteral(node) => node.get_kind(),
            ASTNode::LabelExpr(node) => node.get_kind(),
            ASTNode::BinopExpr(node) => node.get_kind(),
            ASTNode::BlockStatement(node) => node.get_kind(),
            ASTNode::ReturnStatement(node) => node.get_kind(),
            ASTNode::VarDeclStatement(node) => node.get_kind(),
            ASTNode::LabelType(node) => node.get_kind(),
            ASTNode::DeclsList(node) => node.get_kind(),
            ASTNode::FunctionDecl(node) => node.get_kind(),
        }
    }

    fn get_loc(&self) -> Location {
        match self {
            ASTNode::IntLiteral(node) => node.get_loc(),
            ASTNode::FloatLiteral(node) => node.get_loc(),
            ASTNode::StringLiteral(node) => node.get_loc(),
            ASTNode::CharLiteral(node) => node.get_loc(),
            ASTNode::LabelExpr(node) => node.get_loc(),
            ASTNode::BinopExpr(node) => node.get_loc(),
            ASTNode::BlockStatement(node) => node.get_loc(),
            ASTNode::ReturnStatement(node) => node.get_loc(),
            ASTNode::VarDeclStatement(node) => node.get_loc(),
            ASTNode::LabelType(node) => node.get_loc(),
            ASTNode::DeclsList(node) => node.get_loc(),
            ASTNode::FunctionDecl(node) => node.get_loc(),
        }
    }

    fn get_children(&self) -> &[ASTNode] {
        match self {
            ASTNode::IntLiteral(node) => node.get_children(),
            ASTNode::FloatLiteral(node) => node.get_children(),
            ASTNode::StringLiteral(node) => node.get_children(),
            ASTNode::CharLiteral(node) => node.get_children(),
            ASTNode::LabelExpr(node) => node.get_children(),
            ASTNode::BinopExpr(node) => node.get_children(),
            ASTNode::BlockStatement(node) => node.get_children(),
            ASTNode::ReturnStatement(node) => node.get_children(),
            ASTNode::VarDeclStatement(node) => node.get_children(),
            ASTNode::LabelType(node) => node.get_children(),
            ASTNode::DeclsList(node) => node.get_children(),
            ASTNode::FunctionDecl(node) => node.get_children(),
        }
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        match self {
            ASTNode::IntLiteral(node) => node.get_children_mut(),
            ASTNode::FloatLiteral(node) => node.get_children_mut(),
            ASTNode::StringLiteral(node) => node.get_children_mut(),
            ASTNode::CharLiteral(node) => node.get_children_mut(),
            ASTNode::LabelExpr(node) => node.get_children_mut(),
            ASTNode::BinopExpr(node) => node.get_children_mut(),
            ASTNode::BlockStatement(node) => node.get_children_mut(),
            ASTNode::ReturnStatement(node) => node.get_children_mut(),
            ASTNode::VarDeclStatement(node) => node.get_children_mut(),
            ASTNode::LabelType(node) => node.get_children_mut(),
            ASTNode::DeclsList(node) => node.get_children_mut(),
            ASTNode::FunctionDecl(node) => node.get_children_mut(),
        }
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        match self {
            ASTNode::IntLiteral(node) => node.print(printer),
            ASTNode::FloatLiteral(node) => node.print(printer),
            ASTNode::StringLiteral(node) => node.print(printer),
            ASTNode::CharLiteral(node) => node.print(printer),
            ASTNode::LabelExpr(node) => node.print(printer),
            ASTNode::BinopExpr(node) => node.print(printer),
            ASTNode::BlockStatement(node) => node.print(printer),
            ASTNode::ReturnStatement(node) => node.print(printer),
            ASTNode::VarDeclStatement(node) => node.print(printer),
            ASTNode::LabelType(node) => node.print(printer),
            ASTNode::DeclsList(node) => node.print(printer),
            ASTNode::FunctionDecl(node) => node.print(printer),
        }
    }

    fn get_expr_type(&self) -> &Type {
        match self {
            ASTNode::IntLiteral(node) => node.get_expr_type(),
            ASTNode::FloatLiteral(node) => node.get_expr_type(),
            ASTNode::StringLiteral(node) => node.get_expr_type(),
            ASTNode::CharLiteral(node) => node.get_expr_type(),
            ASTNode::LabelExpr(node) => node.get_expr_type(),
            ASTNode::BinopExpr(node) => node.get_expr_type(),
            ASTNode::BlockStatement(node) => node.get_expr_type(),
            ASTNode::ReturnStatement(node) => node.get_expr_type(),
            ASTNode::VarDeclStatement(node) => node.get_expr_type(),
            ASTNode::LabelType(node) => node.get_expr_type(),
            ASTNode::DeclsList(node) => node.get_expr_type(),
            ASTNode::FunctionDecl(node) => node.get_expr_type(),
        }
    }

    fn get_type_value(&self) -> &Type {
        match self {
            ASTNode::IntLiteral(node) => node.get_type_value(),
            ASTNode::FloatLiteral(node) => node.get_type_value(),
            ASTNode::StringLiteral(node) => node.get_type_value(),
            ASTNode::CharLiteral(node) => node.get_type_value(),
            ASTNode::LabelExpr(node) => node.get_type_value(),
            ASTNode::BinopExpr(node) => node.get_type_value(),
            ASTNode::BlockStatement(node) => node.get_type_value(),
            ASTNode::ReturnStatement(node) => node.get_type_value(),
            ASTNode::VarDeclStatement(node) => node.get_type_value(),
            ASTNode::LabelType(node) => node.get_type_value(),
            ASTNode::DeclsList(node) => node.get_type_value(),
            ASTNode::FunctionDecl(node) => node.get_type_value(),
        }
    }
}

////////////////////////////////////////////////
/// AST IntLiteral Node
////////////////////////////////////////////////

pub struct ASTNodeIntLiteral {
    uid: ASTNodeUID,
    loc: Location,
    value: u64,
    expr_type: Option<Type>,
}

impl ASTNodeIntLiteral {
    pub fn new(loc: Location, value: u64) -> ASTNode {
        ASTNode::IntLiteral(Self {
            uid: ASTNodeUID::new(),
            loc,
            value,
            expr_type: None,
        })
    }
    pub fn value(&self) -> u64 {
        self.value
    }

    // Bind the right type for the int literal.
    pub(crate) fn set_expr_type(&mut self, ty: Type) {
        assert!(self.expr_type.is_none());
        self.expr_type = Some(ty);
    }
}

impl ASTNodeImpl for ASTNodeIntLiteral {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}", self.value)
    }

    fn get_expr_type(&self) -> &Type {
        self.expr_type.as_ref().unwrap()
    }
}

////////////////////////////////////////////////
/// AST FloatLiteral Node
////////////////////////////////////////////////

pub struct ASTNodeFloatLiteral {
    uid: ASTNodeUID,
    loc: Location,
    value: f64,
    expr_type: Option<Type>,
}

impl ASTNodeFloatLiteral {
    pub fn new(loc: Location, value: f64) -> ASTNode {
        ASTNode::FloatLiteral(Self {
            uid: ASTNodeUID::new(),
            loc,
            value,
            expr_type: None,
        })
    }
    pub fn value(&self) -> f64 {
        self.value
    }

    // Bind the right type for the float literal.
    pub(crate) fn set_expr_type(&mut self, ty: Type) {
        assert!(self.expr_type.is_none());
        self.expr_type = Some(ty);
    }
}

impl ASTNodeImpl for ASTNodeFloatLiteral {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}", self.value)
    }

    fn get_expr_type(&self) -> &Type {
        self.expr_type.as_ref().unwrap()
    }
}

////////////////////////////////////////////////
/// AST StringLiteral Node
////////////////////////////////////////////////

pub struct ASTNodeStringLiteral {
    uid: ASTNodeUID,
    loc: Location,
    value: String,
    expr_type: Option<Type>,
}

impl ASTNodeStringLiteral {
    pub fn new(loc: Location, value: String) -> ASTNode {
        ASTNode::StringLiteral(Self {
            uid: ASTNodeUID::new(),
            loc,
            value,
            expr_type: None,
        })
    }
    pub fn value(&self) -> &str {
        &self.value
    }

    // Bind the right type for the string literal.
    // TODO: Use and rename
    pub(crate) fn _set_expr_type(&mut self, ty: Type) {
        assert!(self.expr_type.is_none());
        self.expr_type = Some(ty);
    }
}

impl ASTNodeImpl for ASTNodeStringLiteral {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "\"{}\"", encode_string_literal(&self.value))
    }

    fn get_expr_type(&self) -> &Type {
        self.expr_type.as_ref().unwrap()
    }
}

////////////////////////////////////////////////
/// AST CharLiteral Node
////////////////////////////////////////////////

pub struct ASTNodeCharLiteral {
    uid: ASTNodeUID,
    loc: Location,
    value: char,
    expr_type: Option<Type>,
}

impl ASTNodeCharLiteral {
    pub fn new(loc: Location, value: char) -> ASTNode {
        ASTNode::CharLiteral(Self {
            uid: ASTNodeUID::new(),
            loc,
            value,
            expr_type: None,
        })
    }
    pub fn value(&self) -> char {
        self.value
    }
    // Bind the right type for the int literal.
    pub(crate) fn set_expr_type(&mut self, ty: Type) {
        assert!(self.expr_type.is_none());
        self.expr_type = Some(ty);
    }
}

impl ASTNodeImpl for ASTNodeCharLiteral {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(
            printer.os(),
            "'{}'",
            encode_string_literal(&self.value.to_string())
        )
    }

    fn get_expr_type(&self) -> &Type {
        self.expr_type.as_ref().unwrap()
    }
}

////////////////////////////////////////////////
/// AST LabelExpr Node
////////////////////////////////////////////////

pub struct ASTNodeLabelExpr {
    uid: ASTNodeUID,
    loc: Location,
    label: String,
    var_decl: Option<ASTVarDecl>,
}

impl ASTNodeLabelExpr {
    pub fn new(loc: Location, label: String) -> ASTNode {
        ASTNode::LabelExpr(Self {
            uid: ASTNodeUID::new(),
            loc,
            label,
            var_decl: None,
        })
    }
    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn var_decl(&self) -> &ASTVarDecl {
        self.var_decl.as_ref().unwrap()
    }

    // Set the var declartion bound to the label.
    pub(crate) fn set_var_decl(&mut self, var_decl: ASTVarDecl) {
        assert!(self.var_decl.is_none());
        self.var_decl = Some(var_decl);
    }
}

impl ASTNodeImpl for ASTNodeLabelExpr {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}", self.label)
    }

    fn get_expr_type(&self) -> &Type {
        &self.var_decl.as_ref().unwrap().ty
    }
}

////////////////////////////////////////////////
/// AST BinopExpr Node
////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
pub enum ASTNodeBinopExprKind {
    Add,
}

impl ASTNodeBinopExprKind {
    pub fn get_operator_name(&self) -> &'static str {
        match self {
            ASTNodeBinopExprKind::Add => "Add",
        }
    }
}

pub struct ASTNodeBinopExpr {
    uid: ASTNodeUID,
    loc: Location,
    kind: ASTNodeBinopExprKind,
    children: Vec<ASTNode>,
    expr_type: Option<Type>,
}

impl ASTNodeBinopExpr {
    pub fn new(loc: Location, kind: ASTNodeBinopExprKind, lhs: ASTNode, rhs: ASTNode) -> ASTNode {
        assert!(lhs.is_expr(), "binop operand must be an expression");
        assert!(rhs.is_expr(), "binop operand must be an expression");
        ASTNode::BinopExpr(Self {
            uid: ASTNodeUID::new(),
            loc,
            kind,
            children: vec![lhs, rhs],
            expr_type: None,
        })
    }

    pub fn lhs(&self) -> &ASTNode {
        &self.children[0]
    }

    pub fn rhs(&self) -> &ASTNode {
        &self.children[1]
    }

    pub fn kind(&self) -> ASTNodeBinopExprKind {
        self.kind
    }

    // Bind the right type for the int literal.
    pub(crate) fn set_expr_type(&mut self, ty: Type) {
        assert!(self.expr_type.is_none());
        self.expr_type = Some(ty);
    }
}

impl ASTNodeImpl for ASTNodeBinopExpr {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Expr
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.children
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.children
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "OP<{}>(", self.kind.get_operator_name())?;
        printer.print(self.lhs())?;
        write!(printer.os(), ", ")?;
        printer.print(self.rhs())?;
        write!(printer.os(), ")")
    }

    fn get_expr_type(&self) -> &Type {
        self.expr_type.as_ref().unwrap()
    }
}

////////////////////////////////////////////////
/// AST BlockStatement Node
////////////////////////////////////////////////

pub struct ASTNodeBlockStatement {
    uid: ASTNodeUID,
    loc: Location,
    statements: Vec<ASTNode>,
}

impl ASTNodeBlockStatement {
    pub fn new(loc: Location, statements: Vec<ASTNode>) -> ASTNode {
        for st in &statements {
            assert!(
                st.is_statement(),
                "All children of a ASTNodeBlockStatement must be statements"
            );
        }
        ASTNode::BlockStatement(Self {
            uid: ASTNodeUID::new(),
            loc,
            statements,
        })
    }
    pub fn statements(&self) -> &[ASTNode] {
        &self.statements
    }
}

impl ASTNodeImpl for ASTNodeBlockStatement {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Statement
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.statements
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.statements
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}", "{")?;
        printer.inc_indent();
        for child in &self.statements {
            printer.newline()?;
            child.print(printer)?;
        }
        printer.nl_dec_indent()?;
        write!(printer.os(), "{}", "}")
    }
}

////////////////////////////////////////////////
/// AST ReturnStatement Node
////////////////////////////////////////////////

pub struct ASTNodeReturnStatement {
    uid: ASTNodeUID,
    loc: Location,
    value: Vec<ASTNode>,
}

impl ASTNodeReturnStatement {
    pub fn new(loc: Location, value: ASTNode) -> ASTNode {
        assert!(value.is_expr(), "return statement expect a value");
        ASTNode::ReturnStatement(Self {
            uid: ASTNodeUID::new(),
            loc,
            value: vec![value],
        })
    }
    pub fn value(&self) -> &ASTNode {
        &self.value[0]
    }
}

impl ASTNodeImpl for ASTNodeReturnStatement {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Statement
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.value
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.value
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "return ")?;
        self.value().print(printer)?;
        write!(printer.os(), ";")
    }
}

////////////////////////////////////////////////
/// AST VarDeclStatement Node
////////////////////////////////////////////////

pub struct ASTNodeVarDeclStatement {
    uid: ASTNodeUID,
    loc: Location,
    children: Vec<ASTNode>,
    vars_decls: Vec<(usize, String, bool)>,
    vars_decls_bindings: Option<Vec<ASTVarDecl>>,
}

impl ASTNodeVarDeclStatement {
    pub fn new(
        loc: Location,
        vars_type: ASTNode,
        vars_decls: Vec<(String, Option<ASTNode>, Vec<ASTNode>)>,
    ) -> ASTNode {
        let mut children = Vec::new();
        assert!(vars_type.is_type());
        children.push(vars_type);

        let vars_decls: Vec<_> = vars_decls
            .into_iter()
            .map(|v| {
                let arr_offset = children.len();
                let var_id = v.0;
                let has_init = v.1.is_some();

                if let Some(init_expr) = v.1 {
                    assert!(init_expr.is_expr());
                    children.push(init_expr);
                }

                for dim_expr in v.2 {
                    assert!(dim_expr.is_expr());
                    children.push(dim_expr);
                }

                (arr_offset, var_id, has_init)
            })
            .collect();

        ASTNode::VarDeclStatement(Self {
            uid: ASTNodeUID::new(),
            loc,
            children,
            vars_decls,
            vars_decls_bindings: None,
        })
    }

    // Returns how many declarations there are.
    pub fn decls_count(&self) -> usize {
        self.vars_decls.len()
    }

    // Returns the (name, type, [init_expr], [dims*]) of var #<i>
    pub fn get_var_decl(&self, i: usize) -> (&str, &ASTNode, Option<&ASTNode>, &[ASTNode]) {
        assert!(i < self.decls_count());
        let decl = &self.vars_decls[i];

        // Compute children offsets
        let offset = decl.0;
        let has_init = decl.2;
        let next_offset = if i + 1 < self.decls_count() {
            self.vars_decls[i + 1].0
        } else {
            self.children.len()
        };
        let dims_offset = offset + if has_init { 1 } else { 0 };

        let name = &decl.1;
        let init_expr = if has_init {
            Some(&self.children[offset])
        } else {
            None
        };
        let dims_exprs = &self.children[dims_offset..next_offset];

        (name, &self.children[0], init_expr, dims_exprs)
    }

    // Get the var decls bindings.
    pub fn vars_decls_bindings(&self) -> &[ASTVarDecl] {
        self.vars_decls_bindings.as_ref().unwrap()
    }

    pub(crate) fn set_var_decls_bindings(&mut self, bindings: Vec<ASTVarDecl>) {
        assert!(self.vars_decls_bindings.is_none());
        assert!(bindings.len() == self.vars_decls.len());
        self.vars_decls_bindings = Some(bindings);
    }
}

impl ASTNodeImpl for ASTNodeVarDeclStatement {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Statement
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.children
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.children
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        for i in 0..self.decls_count() {
            let (name, ty_node, init_node, dims_nodes) = self.get_var_decl(i);
            write!(printer.os(), "var {}: ", name)?;
            printer.print(ty_node)?;
            for dim in dims_nodes {
                write!(printer.os(), "[")?;
                printer.print(dim)?;
                write!(printer.os(), "]")?;
            }
            if let Some(init_node) = init_node {
                write!(printer.os(), " = ")?;
                printer.print(init_node)?;
            }
            write!(printer.os(), ";")?;
            if i + 1 < self.decls_count() {
                printer.newline()?;
            }
        }
        Ok(())
    }
}

////////////////////////////////////////////////
/// AST LabelType Node
////////////////////////////////////////////////

pub struct ASTNodeLabelType {
    uid: ASTNodeUID,
    loc: Location,
    label: String,
    type_decl: Option<ASTTypeDecl>,
}

impl ASTNodeLabelType {
    pub fn new(loc: Location, label: String) -> ASTNode {
        ASTNode::LabelType(Self {
            uid: ASTNodeUID::new(),
            loc,
            label,
            type_decl: None,
        })
    }
    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn get_type_decl(&self) -> &ASTTypeDecl {
        self.type_decl.as_ref().unwrap()
    }

    // Set the type declaration bound to the label.
    pub(crate) fn set_type_decl(&mut self, type_decl: ASTTypeDecl) {
        assert!(self.type_decl.is_none());
        self.type_decl = Some(type_decl);
    }
}

impl ASTNodeImpl for ASTNodeLabelType {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Type
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &[]
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut []
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        write!(printer.os(), "{}", self.label)
    }

    fn get_type_value(&self) -> &Type {
        &self.type_decl.as_ref().unwrap().ty
    }
}

////////////////////////////////////////////////
/// AST DeclsList Node
////////////////////////////////////////////////

// List of declarations, this is always the root of the AST in C.
pub struct ASTNodeDeclsList {
    uid: ASTNodeUID,
    loc: Location,
    decls: Vec<ASTNode>,
}

impl ASTNodeDeclsList {
    pub fn new(loc: Location, decls: Vec<ASTNode>) -> ASTNode {
        for decl in &decls {
            assert!(
                decl.is_decl(),
                "DeclsList children must all be declarations"
            );
        }
        ASTNode::DeclsList(Self {
            uid: ASTNodeUID::new(),
            loc,
            decls,
        })
    }
    pub fn decls(&self) -> &[ASTNode] {
        &self.decls
    }
}

impl ASTNodeImpl for ASTNodeDeclsList {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Decl
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.decls
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.decls
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        for decl in &self.decls {
            decl.print(printer)?;
            write!(printer.os(), "\n")?;
        }
        Ok(())
    }
}

////////////////////////////////////////////////
/// AST FunctionDecl Node
////////////////////////////////////////////////

// List of declarations, this is always the root of the AST in C.
pub struct ASTNodeFunctionDecl {
    uid: ASTNodeUID,
    loc: Location,
    label: String,
    args: Vec<String>,
    // Hold args + return + optional body
    children: Vec<ASTNode>,
    args_bindings: Option<Vec<ASTVarDecl>>,
}

impl ASTNodeFunctionDecl {
    pub fn new(
        loc: Location,
        label: String,
        args: Vec<(String, ASTNode)>,
        ret: ASTNode,
        body: Option<ASTNode>,
    ) -> ASTNode {
        // Collect all children
        let mut children = Vec::new();
        let mut args_names = Vec::new();
        for (arg_name, arg_ty) in args {
            assert!(arg_ty.is_type(), "function arguments must be types");
            children.push(arg_ty);
            args_names.push(arg_name);
        }
        assert!(ret.is_type(), "function return must be a type");
        children.push(ret);

        if let Some(body) = body {
            match &body {
                ASTNode::BlockStatement(_) => {}
                _ => panic!("function body must be a block statement"),
            }
            children.push(body);
        }

        ASTNode::FunctionDecl(Self {
            uid: ASTNodeUID::new(),
            loc,
            label,
            args: args_names,
            children,
            args_bindings: None,
        })
    }

    // Returns the name of the function
    pub fn label(&self) -> &str {
        &self.label
    }

    // Return the argument names of the function.
    pub fn args_names(&self) -> &[String] {
        &self.args
    }

    // Returns the argument types of the function.
    pub fn args_types(&self) -> &[ASTNode] {
        &self.children[0..self.args.len()]
    }
    pub fn args_types_mut(&mut self) -> &mut [ASTNode] {
        &mut self.children[0..self.args.len()]
    }

    // Return the return type of the function
    pub fn return_type(&self) -> &ASTNode {
        &self.children[self.args.len()]
    }
    pub fn return_type_mut(&mut self) -> &mut ASTNode {
        &mut self.children[self.args.len()]
    }

    // Returns true if the function has a body.
    pub fn has_body(&self) -> bool {
        self.args.len() + 1 < self.children.len()
    }

    // Returns the body of the function if it has one.
    pub fn get_body(&self) -> Option<&ASTNode> {
        if self.has_body() {
            self.children.last()
        } else {
            None
        }
    }
    pub fn get_body_mut(&mut self) -> Option<&mut ASTNode> {
        if self.has_body() {
            self.children.last_mut()
        } else {
            None
        }
    }

    pub fn get_args_bindings(&self) -> &[ASTVarDecl] {
        self.args_bindings.as_ref().unwrap()
    }

    // Set the var decl bindings for the function arguments.
    pub(crate) fn set_args_bindings(&mut self, bindings: Vec<ASTVarDecl>) {
        assert!(self.args_bindings.is_none());
        assert!(bindings.len() == self.args_names().len());
        self.args_bindings = Some(bindings);
    }
}

impl ASTNodeImpl for ASTNodeFunctionDecl {
    fn get_uid(&self) -> ASTNodeUID {
        self.uid
    }

    fn get_kind(&self) -> ASTNodeKind {
        ASTNodeKind::Decl
    }

    fn get_loc(&self) -> Location {
        self.loc
    }

    fn get_children(&self) -> &[ASTNode] {
        &self.children
    }

    fn get_children_mut(&mut self) -> &mut [ASTNode] {
        &mut self.children
    }

    fn print<'a, W: std::io::Write>(
        &self,
        printer: &mut ASTPrinter<'a, W>,
    ) -> Result<(), std::io::Error> {
        // Print function name.
        write!(printer.os(), "function @{}(", self.label)?;

        // Print function args.
        let mut is_first = true;
        for (arg_name, arg_ty) in self.args_names().iter().zip(self.args_types()) {
            if !is_first {
                write!(printer.os(), ", ")?;
            }
            write!(printer.os(), "{}: ", arg_name)?;
            arg_ty.print(printer)?;
            is_first = false;
        }

        // Print function return type
        write!(printer.os(), ") -> ")?;
        self.return_type().print(printer)?;

        if let Some(body) = self.get_body() {
            write!(printer.os(), "\n")?;
            body.print(printer)
        } else {
            write!(printer.os(), ";")
        }
    }
}
