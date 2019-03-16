// Low-level representation, this is the final goal form before converting
// to bytecode

use super::Type;

pub type LLR = Vec<Fn>;

#[derive(PartialEq, Eq, Debug)]
pub struct Fn {
	pub name: String, // All fns are public and may need to interact with ABI
	pub statements: Vec<Statement>,
	pub signature: Signature,
	pub namespace: Namespace,
	pub is_extern: bool,
}
#[derive(PartialEq, Eq, Debug)]
pub struct Signature {
	pub parameters: Vec<NameKey>,
	pub return_type: Option<Type>, // Can be void (None)
}
pub type NameKey = usize;
pub type Namespace = Vec<TypedVar>;
#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
	//pub Assignment(Assignment),
	FnCall(FnCall),
}
#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
	Literal(Literal),
	//Identifier(TypedVar),
	//pub BinaryExpr,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Literal {
	String(String),
	Int(i32),
	//pub Float(f32),
}
pub type TypedVar = Type;
#[derive(PartialEq, Eq, Debug)]
pub struct FnCall {
	pub index: NameKey,
	pub arguments: Vec<Expression>,
}

