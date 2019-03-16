use super::Type;

pub type AST = Vec<ASTNode>;

#[derive(PartialEq, Eq, Debug)]
pub type ASTNode = Function;
#[derive(PartialEq, Eq, Debug)]
pub struct Function {
	pub name: String,
	pub statements: Vec<Statement>,
	pub signature: Signature,
	pub is_extern: bool,
}
#[derive(PartialEq, Eq, Debug)]
pub struct Signature {
	pub parameters: Vec<TypedId>,
	pub return_type: Option<Type>, // Can be void (None)
}
#[derive(PartialEq, Eq, Debug)]
pub struct TypedId {
	pub name: String,
	pub id_type: Type,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
	//pub Assignment(Assignment),
	FnCall(FnCall),
}
#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
	Literal(Literal),
	Identifier(TypedId),
	//pub BinaryExpr,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Literal {
	String(String),
	Int(i32),
	//pub Float(f32),
}
#[derive(PartialEq, Eq, Debug)]
pub struct Assignment {
	pub lvalue: String,
	pub rvalue: Expression,
}
#[derive(PartialEq, Eq, Debug)]
pub struct FnCall {
	pub name: String,
	pub arguments: Vec<Expression>,
}

