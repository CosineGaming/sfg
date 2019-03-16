// Low-level representation, this is the final goal form before converting
// to bytecode

// Note that this representation is meant to mirror BYTECODE not SYNTAX. as
// such, until it can be generated, it doesn't belong in this representation
// (so some AST constructs are simply lost in lower())

// We're racing to hello world so there's a lot commented out

use super::Type;

pub type LLR = Vec<Fn>;

#[derive(PartialEq, Eq, Debug)]
pub struct Fn {
	pub name: String, // All fns are public and may need to interact with ABI
	pub statements: Vec<Statement>,
	pub signature: Signature,
	//pub namespace: Namespace,// my idea is to use the namespace as where variables are placed
}
#[derive(PartialEq, Eq, Debug)]
pub struct ExternFn {
	pub name: String,
	pub signature: Signature,
}
#[derive(PartialEq, Eq, Debug)]
pub struct Signature {
	pub parameters: Vec<TypedVar>, // When namespace is added, this should be Vec<NameKey> and point to the namespace
	pub return_type: Option<Type>, // Can be void (None)
}
pub type NameKey = usize;
//pub type Namespace = Vec<TypedVar>;
#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
	//pub Assignment(Assignment),
	//FnCall(FnCall),
	ExternFnCall(ExternFnCall),
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
	//Int(i32),
	//pub Float(f32),
}
pub type TypedVar = Type;
//#[derive(PartialEq, Eq, Debug)]
//pub struct FnCall {
	//pub index: NameKey,
	//pub arguments: Vec<Expression>,
//}
#[derive(PartialEq, Eq, Debug)]
pub struct ExternFnCall {
	pub index: NameKey,
	pub arguments: Vec<Expression>,
}

