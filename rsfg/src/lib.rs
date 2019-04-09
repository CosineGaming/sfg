// all roads lead to lib.rs

mod ast;
mod llr;

mod lexer;
mod parser;
mod lower;
mod codegen;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
	Identifier(String),
	Tab,
	StringLit(String),
	IntLit(i32),
	Type(Type),
	Fn,
	ExternFn,
	ExternFnCall(String),
	Return,
	Comma,
	Equals,
	Colon,
	Newline,
	LParen,
	RParen,
	If,
	While,
	Declare,
	Assignment,
	Plus,
	Minus,
	Times,
	Divide,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
	kind: TokenType,
	line: usize,
	col: usize,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Type {
	Int,
	Str,
	Infer,
}

// TODO: add an actual import system so that we don't use this
// "#include-but-worse" hack for the stdlib
pub fn compile(text: &str, stdlib: &str) -> Vec<u8> {
	let full_text = format!("{}\n{}", text, stdlib);
	codegen::gen(
		lower::lower(
			parser::parse(
				lexer::lex(&full_text))))
}

