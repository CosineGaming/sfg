// all roads lead to lib.rs

#[macro_use]
extern crate log;

mod ast;
mod llr;
mod codegen;
mod lexer;
mod lower;
mod parser;

#[derive(Debug)]
pub enum CompileError {
	Parse(parser::ParseError),
	Lower(lower::LowerError),
}
impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use CompileError::*;
        match self {
            Parse(err) => write!(f, "{}", err),
            Lower(err) => write!(f, "{}", err),
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for CompileError {}
type Result<T> = std::result::Result<T, CompileError>;

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
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Not,
    NotEqual,
    Or,
    And,
    Colon,
    Newline,
    LParen,
    RParen,
    If,
    Else,
    While,
    Declare,
    Assignment,
    Plus,
    Minus,
    Times,
    Divide,
    OpAssign(Box<TokenType>),
    True,
    False,
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
    Bool,
    Str,
    Infer,
}

// TODO: add an actual import system so that we don't use this
// "#include-but-worse" hack for the stdlib
pub fn compile(text: &str, stdlib: &str) -> Result<Vec<u8>> {
    let full_text = format!("{}\n{}", text, stdlib);
    let result = parser::parse(lexer::lex(&full_text));
    let ast = match result {
	    Ok(ast) => ast,
	    Err(err) => return Err(CompileError::Parse(err)),
    };
    let result = lower::lower(ast);
    let llr = match result {
	    Ok(llr) => llr,
	    Err(err) => return Err(CompileError::Lower(err)),
    };
    Ok(codegen::gen(llr))
}

