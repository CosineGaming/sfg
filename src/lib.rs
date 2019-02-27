
static SPACE: &str = " \t";
static SYMBOL: &str = "+-*/%";

#[derive(Clone)]
pub enum Token {
	Identifier(String),
	Symbol(String),
	Space(String),
	NewLine(String),
}
use Token::*;

// Rules: A-Z,a-z
fn is_id_1st(c: char) -> bool {
	c >= 'A' && c <= 'z'
}
// A-Z or 0-9
fn is_id(c: char) -> bool {
	is_id_1st(c) || (c >= '0' && c <= '9')
}

fn id_char(c: char) -> Token {
	let text = c.to_string();
	if is_id_1st(c) {
		Identifier(text)
	} else if SPACE.contains(c) {
		Space(text)
	} else if c == '\n' {
		NewLine(text)
	} else if SYMBOL.contains(c) {
		Symbol(text)
	} else {
		// TODO: How to make error show these automatically like rust?
		panic!("expected space, identifier, or newline");
	}
}

pub fn lex(text: String) -> Vec<Token> {
	// Start with a newline. We could probly find a better way but meh
	let mut state = NewLine(String::from("\n"));
	let mut tokens = Vec::<Token>::new();
	for c in text.chars() {
		match state {
			Space(ref mut text) => {
				if SPACE.contains(c) {
					text.push(c);
				} else {
					tokens.push(state);
					state = id_char(c);
				}
			},
			Identifier(ref mut text) => {
				if is_id(c) {
					text.push(c);
				} else {
					tokens.push(state);
					state = id_char(c);
				}
			},
			NewLine(ref mut text) => {
				if c == '\n' {
					text.push(c);
				} else {
					tokens.push(state);
					state = id_char(c);
				}
			},
			Symbol(ref mut text) => {
				if SYMBOL.contains(c) {
					text.push(c);
				} else {
					tokens.push(state);
					state = id_char(c);
				}
			},
		};
	}
	tokens
}

type AST = Vec<Function>;

struct Function {
	statements: Vec<Statement>,
	signature: Signature,
}
struct Signature {
	parameters: Vec<Identifier>,
	return_type: Type,
}
struct Identifier {
	name: String,
	id_type: Type,
}
enum Type {
	//Extern,
	Int,
	//Float,
}
struct Statement {
	lvalue: Identifier,
	rvalue: Identifier,
}

enum ParseState {
	Global,
	Signature,
	Expression,
}

fn parse(tokens: Vec<Token>) -> AST {
	let mut ast: Vec<Token> = vec![];
	let mut state = ParseState::Global;
	for t in tokens {
		match state {
			ParseState::Global => {
				if let Identifier(keyword) = t {
					if keyword == "fn" {
						state = ParseState::Signature;
					}
				} else if let Space(_) = t {
					panic!("unexpected indent in global space");
				} else {
					panic!("expected `fn`");
				}
			},
			ParseState::Signature => {
				if let Symbol("(")
		};
	}
	vec![Function {
		statements: vec![Statement {
			lvalue: Identifier {
				name: String::from("hi"),
				id_type: Type::Int,
			},
			rvalue: Identifier {
				name: String::from("hello"),
				id_type: Type::Int,
			},
		}],
		signature: Signature {
			parameters: vec![],
			return_type: Type::Int,
		}
	}]
}

pub fn compile(text: String) -> String {
	lex(text);
	String::from("not implemented yet")
}

