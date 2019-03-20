// all roads lead to lib.rs

mod ast;
mod llr;

mod lexer;
mod parser;
mod lower;
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
	Return,
	Comma,
	Colon,
	Newline,
	LParen,
	RParen,
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

