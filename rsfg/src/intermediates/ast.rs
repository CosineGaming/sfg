//! the Abstract Syntax Tree ([AST]) is the result of parsing, and contains
//! all the data in your program in actual logical chunks, most closely
//! resembling the frontend conceptual syntax.
//!
//! the actual AST is the [AST] type, which is just a list of functions. also
//! in this file is all the data structures *below* that root in
//! the tree. i know there's a lot of data structures here, but they
//! mostly make sense based on their names. Just remember that they refer
//! to the **sfg** language, not rust Ifs or Fns or Literals or anything.

use crate::{llr, span::Span, Type};

/// An Abstract Syntax Tree
pub type AST = Vec<ASTNode>;

/// A function really. Although it can be extern (just for typechecking,
/// provided in the VM) or fn (real function, with code)
#[derive(PartialEq, Debug)]
pub enum ASTNode {
    Fn(Fn),
    ExternFn(ExternFn),
}
/// Fn includes the signature of the function and the entire body of code
/// to be executed
#[derive(PartialEq, Debug)]
pub struct Fn {
    pub statements: Vec<Statement>,
    pub signature: Signature,
}
#[derive(PartialEq, Debug)]
pub struct ExternFn {
    pub signature: Signature,
}
#[derive(PartialEq, Debug, Default)]
pub struct Signature {
    pub name: NameSpan,
    pub return_type: Option<Type>, // None => Void, Some(t) => Type
    pub parameters: Vec<TypedName>,
    pub span: Span,
}
/// A name that's guaranteed to have a type, *because it's required
/// syntactically*. Recall that the AST mirrors frontend syntax more than
/// compiled code, so even though identifiers and expressions need types eventually,
/// they don't syntactically
#[derive(PartialEq, Clone, Debug)]
pub struct TypedName {
    pub name: NameSpan,
    pub id_type: Type,
}
/// Because we need spans for *everything* for error reporting, and we need
/// Strings a lot for names, we just plop them together
#[derive(PartialEq, Clone, Debug, Default)]
pub struct NameSpan {
    pub name: String,
    pub span: Span,
}
impl NameSpan {
    pub fn fake(name: &'static str) -> Self {
        Self { name: name.to_string(), ..Default::default() }
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Assignment(Assignment),
    /// The data in a declaration is the same as assignment
    Declaration(Declaration),
    FnCall(FnCall),
    Return(Option<Expression>),
    If(If),
    WhileLoop(WhileLoop),
    LLRInsts(Vec<llr::Instruction>),
}
#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(NameSpan),
    Not(Box<Expression>),
    // A FnCall can be an expression as well as a statement
    // A statement FnCall is lowered differently than an expression FnCall
    FnCall(FnCall),
    Binary(Box<BinaryExpr>),
}
#[derive(PartialEq, Clone, Debug)]
pub struct Literal {
    pub data: LiteralData,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub enum LiteralData {
    String(String),
    Int(i32),
    Bool(bool),
    Float(f32),
}
#[derive(PartialEq, Clone, Debug)]
pub struct Assignment {
    pub lvalue: NameSpan,
    pub rvalue: Expression,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub struct Declaration {
    pub assignment: Assignment,
    pub id_type: Option<Type>,
}
/// This is the actual call like add_stuff(5, 6.0), not the function
#[derive(PartialEq, Clone, Debug)]
pub struct FnCall {
    pub name: NameSpan,
    pub arguments: Vec<Expression>,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub struct BinaryExpr {
    pub left: Expression,
    pub op: BinaryOp,
    pub right: Expression,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub else_statements: Vec<Statement>,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub struct WhileLoop {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub span: Span,
}
#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Or,
    And,
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}
