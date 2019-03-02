
static SPACE: &str = " \t";

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
	Identifier(String),
	Space(String),
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

// Rules: A-Z,a-z
fn is_id_1st(c: char) -> bool {
	c >= 'A' && c <= 'z'
}
// A-Z or 0-9
fn is_id(c: char) -> bool {
	is_id_1st(c) || (c >= '0' && c <= '9')
}

fn id_char(c: char) -> Token {
	use Token::*;
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
	use Token::*;
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
					// Space is only included at beginning of line
					if tokens.last() == Some(&Newline) {
						tokens.push(state);
					}
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
struct Statement {
	lvalue: Identifier,
	rvalue: Expression,
}
type Expression = isize; // TODO

fn parse_id(rtokens: &mut Vec<Token>, type_required: bool) -> Identifier {
	let name = match rtokens.pop() {
		Some(Token::Identifier(name)) => name,
		Some(_) => panic!("identifier wasn't identifier (compiler bug)"),
		None => panic!("unexpected EOF parsing identifier"),
	};
	match rtokens.last() {
		Some(Token::Colon) => {
			rtokens.pop();
			let id_type = match rtokens.pop() {
				Some(Token::Type(id_type)) => id_type,
				_ => panic!("expected type after colon"),
			};
			return Identifier {
				name,
				id_type,
			};
		}
		_ => (),
	}
	Identifier {
		name,
		id_type: Type::Infer,
	}
}

fn parse_statement(rtokens: &mut Vec<Token>) -> Statement {
	println!("WARNING: parse statement unimplemented");
	Statement {
		lvalue: Identifier {
			name: "TODO".to_string(),
			id_type: Type::Infer,
		},
		rvalue: 7040,
	}
}

fn parse_fn(rtokens: &mut Vec<Token>) -> Function {
	let mut parameters = Vec::<Identifier>::new();
	// Parse signature
	match rtokens.pop() {
		Some(Token::Fn) => (),
		Some(_) => panic!("fn didn't start with fn"),
		None => panic!("expected signature after fn"),
	}
	let name = match rtokens.last() {
		Some(Token::Identifier(name)) => { rtokens.pop(); name },
		Some(_) => panic!("expected fn name"),
		None => panic!("unexpected EOF parsing fn"),
	};
	match rtokens.pop() {
		Some(Token::LParen) => (),
		Some(_) => panic!("expected ("),
		None => panic!("unexpected EOF parsing fn"),
	}
	loop {
		let id = match rtokens.last() {
			Some(Token::Identifier(_)) => parse_id(&mut rtokens, true),
			Some(Token::RParen) => break,
			Some(_) => panic!("expected Identifier or RParen"),
			None => panic!("unexpected EOF parsing fn"),
		};
		match rtokens.pop() {
			Some(Token::Comma) => (),
			Some(Token::RParen) => break,
			_ => panic!("expected comma or RParen"),
		}
	}
	let return_type = match rtokens.last() {
		Some(Token::Type(r_type)) => { rtokens.pop(); *r_type },
		Newline => Type::Infer,
		None => panic!("expected end of signature"),
	};
	match rtokens.pop() {
		Newline => (),
		_ => panic!("expected newline after definition"),
	}
	let signature = Signature {
		parameters: parameters,
		return_type: return_type,
	};
	let mut statements = vec![];
	loop {
		let t = match rtokens.last() {
			Some(t) => t,
			None => return Function {
				signature: signature,
				statements: statements,
			}
		};
		match t {
			Token::Space(text) => {
				let mut chars = text.chars();
				let mut new_count = 1;
				let mut is_tab = match chars.next() {
					Some('\t') => true,
					Some(' ') => false,
					Some(_) => panic!("lexer gave non-space space"),
					None => {
						println!("WARNING: no-space space (safe to ignore due to hack from lexer)");
						new_count = 0;
						continue
					}, // TODO
				};
				for c in chars {
					if ('\t' == c) == is_tab {
						new_count += 1;
					} else {
						panic!("mixing tabs and spaces at the indent level");
					}
				}
				if !is_tab {
					println!("WARNING: spaces must be 4 for partial implementation"); // TODO
					if new_count % 4 != 0 {
						panic!("space count undivisible by 4!");
					}
					new_count = new_count / 4;
				}
				if new_count != 1 {
					// We've collected all the statements
					break;
				}
				// Eat the space
				rtokens.pop();
			},
			_ => {
				panic!("expected indented block in function, got {:?}", t);
			}
		}
	}
	Function {
		signature,
		statements,
	}
}

fn parse(tokens: &mut Vec<Token>) -> AST {
	tokens.reverse();
	// This is just for clarity
	let mut rtokens = tokens;
	let mut indents = 0;
	let mut t;
	let mut ast = vec![];
	// Every token
	loop {
		t = match rtokens.last() {
			Some(t) => t,
			None => break,
		};
		match t {
			// Parse a function
			Fn => {
				ast.push(parse_fn(&mut tokens));
			},
			Newline => {},
			_ => {
				panic!("expected Fn in global space, got {:?}", t);
			},
		};
		rtokens.pop();
	}
	vec![Function {
		statements: vec![Statement {
			lvalue: Identifier {
				name: String::from("hi"),
				id_type: Type::Int,
			},
			rvalue: 7040,
		}],
		signature: Signature {
			parameters: vec![],
			return_type: Type::Int,
		}
	}]
}

pub fn compile(text: String) -> String {
	parse(&mut lex(text));
	String::from("not implemented yet")
}

