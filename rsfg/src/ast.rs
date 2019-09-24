use super::Type;

pub type AST = Vec<ASTNode>;

#[derive(PartialEq, Eq, Debug)]
pub enum ASTNode {
    Fn(Fn),
    ExternFn(ExternFn),
}
#[derive(PartialEq, Eq, Debug)]
pub struct Fn {
    pub statements: Vec<Statement>,
    pub signature: Signature,
}
#[derive(PartialEq, Eq, Debug)]
pub struct ExternFn {
    pub signature: Signature,
}
#[derive(PartialEq, Eq, Debug)]
pub struct Signature {
    pub name: String,
    pub parameters: Vec<TypedId>,
    pub return_type: Option<Type>, // Can be void (None)
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypedId {
    pub name: String,
    pub id_type: Type,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    Assignment(Assignment),
    /// The data in a declaration is the same as assignment
    Declaration(Assignment),
    FnCall(FnCall),
    Return(Option<Expression>),
    If(If),
    WhileLoop(WhileLoop),
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(TypedId),
    Not(Box<Expression>),
    // A FnCall can be an expression as well as a statement
    // A statement FnCall is lowered differently than an expression FnCall
    FnCall(FnCall),
    Binary(Box<BinaryExpr>),
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    String(String),
    Int(i32),
    Bool(bool),
    //pub Float(f32),
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Assignment {
    pub lvalue: String,
    pub rvalue: Expression,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FnCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BinaryExpr {
    pub left: Expression,
    pub op: BinaryOp,
    pub right: Expression,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub else_statements: Vec<Statement>,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct WhileLoop {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BinaryOp {
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Plus,
    Minus,
    Times,
    Divide,
}
