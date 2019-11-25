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
    span: Span,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Type {
    Int,
    Bool,
    Str,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Span {
	lo: (usize, usize),
	hi: (usize, usize),
	// TODO: file
}
impl Span {
	pub fn new() -> Self {
		Self { hi: (0,0), lo: (0,0) }
	}
	pub fn set(mut spans: Vec<Span>) -> Span {
		let first = spans.pop().expect("cannot form set of less than one span");
		let mut lo = first.lo;
		let mut hi = first.hi;
		for span in spans {
			// if lower, go lower
			if span.lo.0 < lo.0 {
				lo = span.lo;
			} else if span.lo.0 == lo.0 && span.lo.1 < lo.1 {
				lo = span.lo;
			}
			// if higher go higher
			if span.hi.0 > hi.0 {
				hi = span.hi;
			} else if span.hi.0 == hi.0 && span.hi.1 > hi.1 {
				hi = span.hi;
			}
		}
		Span { lo, hi }
	}
}
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	    if *self == Span::new() {
		    write!(f, "internal")
	    } else {
		    write!(f, "{}:{}", self.lo.0, self.lo.1)
	    }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
        }
    }
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

pub fn compile_or_print(text: &str, stdlib: &str) -> Vec<u8> {
    match compile(text, stdlib) {
	    Ok(c) => c,
	    Err(err) => {
		    eprintln!("{}", err);
		    std::process::exit(1);
	    }
    }
}

#[cfg(test)]
mod test {
	use super::Span;
	#[test]
	fn test_span_set() {
		let set = Span::set(vec![
			Span {
				lo: (4,4),
				hi: (5,5),
			},
			Span {
				lo: (4, 2),
				hi: (4, 10),
			},
		]);
		assert_eq!(set, Span {
			lo: (4, 2),
			hi: (5, 5),
		});
	}
}

