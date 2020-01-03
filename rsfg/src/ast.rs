use super::{llr, Span, Type};

pub type AST = Vec<ASTNode>;

#[derive(PartialEq, Debug)]
pub enum ASTNode {
    Fn(Fn),
    ExternFn(ExternFn),
}
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
#[derive(PartialEq, Clone, Debug)]
pub struct TypedName {
    pub name: NameSpan,
    pub id_type: Type,
}
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

impl Expression {
    pub fn full_span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span,
            Self::Identifier(id) => id.span,
            Self::Not(expr) => {
                warn!("unimplemented span on not to include ! symbol");
                expr.full_span()
            }
            // A FnCall can be an expression as well as a statement
            // A statement FnCall is lowered differently than an expression FnCall
            Self::FnCall(call) => call.span,
            Self::Binary(binary) => {
                Span::set(vec![binary.left.full_span(), binary.right.full_span()])
            }
        }
    }
}
