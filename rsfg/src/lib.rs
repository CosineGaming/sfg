// all roads lead to lib.rs

mod lexer;
mod parser;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
	Identifier(String),
	Tab,
	StringLit(String),
	Type(Type),
	Fn,
	Comma,
	Colon,
	Newline,
	LParen,
	RParen,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
	Int,
	Infer,
}

pub fn compile(text: &str) -> String {
	parser::parse(&mut lexer::lex(text));
	String::from("not implemented yet")
}

