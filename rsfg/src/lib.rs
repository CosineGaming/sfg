// all roads lead to lib.rs

mod lexer;
mod parser;
mod codegen;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
	Identifier(String),
	Tab,
	StringLit(String),
	IntLit(i32),
	Type(Type),
	Fn,
	ExternFn,
	ExternFnCall(String),
	Comma,
	Colon,
	Newline,
	LParen,
	RParen,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Type {
	Int,
	Infer,
}

mod ast {
	use super::Type;

	pub type AST = Vec<ASTNode>;

	#[derive(PartialEq, Eq, Debug)]
	pub enum ASTNode {
		Function(Function),
		ExternFn(ExternFn),
	}
	#[derive(PartialEq, Eq, Debug)]
	pub struct Function {
		pub name: String,
		pub statements: Vec<Statement>,
		pub signature: Signature,
	}
	#[derive(PartialEq, Eq, Debug)]
	pub struct ExternFn {
		pub name: String,
		pub signature: Signature,
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
		Identifier(String),
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

}

pub fn compile(text: &str) -> Vec<u8> {
	codegen::gen(parser::parse(&mut lexer::lex(text)))
}

