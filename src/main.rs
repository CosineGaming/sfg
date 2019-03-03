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

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
	Int,
	Infer,
}

pub fn compile(text: String) -> String {
	parser::parse(&mut lexer::lex(text));
	String::from("not implemented yet")
}


fn main() {
	let script_filename = std::env::args().nth(1)
		.expect("no filename given");
	let script_string = std::fs::read_to_string(script_filename)
		.expect("could not load given file");
    println!("{}", script_string);
    println!("{}", compile(script_string));
}

