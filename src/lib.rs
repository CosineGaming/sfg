
static SPACE: &str = " \t";

#[derive(Debug)]
pub enum Token {
	Identifier(String),
	Space(String),
	StringLit(String),
	Fn,
	Newline,
	LParen,
	RParen,
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
		Newline
	} else if c == '(' {
		LParen
	} else if c == ')' {
		RParen
	} else if c == '"' {
		StringLit(String::new())
	} else {
		// TODO: How to make error show these automatically like rust?
		panic!("expected space, identifier, or newline");
	}
}

pub fn lex(text: String) -> Vec<Token> {
	// Start with a newline. We could probly find a better way but meh
	let mut state = Newline;
	let mut tokens = Vec::<Token>::new();
	let mut chars = text.chars();
	for c in chars {
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
					let symbol_or_id = match text.as_ref() {
						"fn" => Fn,
						_ => state,
					};
					tokens.push(symbol_or_id);
					state = id_char(c);
				}
			},
			StringLit(ref mut text) => {
				if c != '"' {
					text.push(c);
				} else {
					tokens.push(state);
					// Let the token advance without identifying
					// TODO: Don't use this awful hack which creates empty space
					state = Space(String::new());
				}
			},
			Newline | LParen | RParen => {
				tokens.push(state);
				state = id_char(c);
			},
			Fn => {
				panic!("lexer shouldn't encounter this identifier-like keyword");
			}
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
	let mut tokens = tokens.iter();
	let mut indents = 0;
	let mut t;
	// Every token
	'outer: loop {
		t = match tokens.next() {
			Some(t) => t,
			None => break,
		};
		match t {
			// Parse a function
			Fn => {
				// Every token within function
				loop {
					t = match tokens.next() {
						Some(t) => t,
						None => break 'outer,
					};
					match t {
						Space(text) => {
							let mut chars = text.chars();
							let mut new_count = 1;
							let mut is_tab = match chars.next() {
								Some('\t') => true,
								Some(' ') => false,
								Some(_) => panic!("lexer gave non-space space"),
								None => continue, // No-space space ignore due to hack from lexer (TODO)
							};
							for c in chars {
								if ('\t' == c) == is_tab {
									new_count += 1;
								} else {
									panic!("mixing tabs and spaces at the indent level");
								}
							}
						},
						_ => {
							panic!("expected indented block in function, got {:?}", t);
						}
					}
				}
			},
			Newline => {},
			_ => {
				panic!("expected Fn in global space, got {:?}", t);
			},
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
	parse(lex(text));
	String::from("not implemented yet")
}

