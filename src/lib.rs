
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

pub fn lex(text: String) -> Vec<Token> {
	use Token::*;
	// Start with a newline. We could probly find a better way but meh
	let mut tokens = Vec::<Token>::new();
	let mut rchars: Vec<char> = text.chars().collect();
	rchars.reverse();
	loop {
		let c = match rchars.last() {
			Some(c) => *c,
			None => break, // We weren't in the middle of anything so successful EOF
		};
		let token = if is_id_1st(c) {
			let mut text = c.to_string();
			rchars.pop();
			loop {
				let x = match rchars.last() {
					Some(x) => *x,
					None => break, // End of ID is fine
				};
				if is_id(x) {
					text.push(c);
				} else {
					break;
				}
			}
			let symbol_or_id = match text.as_ref() {
				"fn" => Fn,
				// These names clash, it sucks
				"int" => Token::Type(self::Type::Int),
				_ => Identifier(text),
			};
			symbol_or_id
		} else if SPACE.contains(c) {
			// TODO: Store space as more nuanced than string?
			if tokens.last() == Some(&Newline) {
				let mut text = String::new();
				loop {
					let x = match rchars.last() {
						Some(x) => *x,
						None => break, // Ending on whitespace is fine
					};
					if SPACE.contains(x) {
						text.push(x);
						rchars.pop();
					} else {
						break;
					}
				}
				Space(text)
			} else {
				rchars.pop();
				continue
			}
		} else if c == '\n' {
			rchars.pop();
			Newline
		} else if c == '(' {
			rchars.pop();
			LParen
		} else if c == ')' {
			rchars.pop();
			RParen
		} else if c == ':' {
			rchars.pop();
			Colon
		} else if c == ',' {
			rchars.pop();
			Comma
		} else if c == '"' {
			let mut text = String::new();
			// Don't include literal quote
			rchars.pop();
			loop {
				// Pop immediately because don't include literal pop
				match rchars.pop() {
					Some('"') => break StringLit(text),
					Some(_) => text.push(c),
					None => panic!("unexpected EOF parsing string literal"),
				}
			}
		} else {
			// TODO: How to make error show these automatically like rust?
			panic!("lexer doesn't know what to do with character {}", c);
		};
		tokens.push(token);
	}
	tokens
}

type AST = Vec<Function>;

struct Function {
	name: String,
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
		_ => if type_required {
			panic!("type required and not given for {}", name);
		},
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

fn parse_fn(mut rtokens: &mut Vec<Token>) -> Function {
	let mut parameters = Vec::<Identifier>::new();
	// Parse signature
	match rtokens.pop() {
		Some(Token::Fn) => (),
		Some(_) => panic!("fn didn't start with fn"),
		None => panic!("expected signature after fn"),
	}
	let name = match rtokens.pop() {
		Some(Token::Identifier(name)) => name,
		Some(_) => panic!("expected fn name"),
		None => panic!("unexpected EOF parsing fn"),
	};
	match rtokens.pop() {
		Some(Token::LParen) => (),
		Some(_) => panic!("expected ("),
		None => panic!("unexpected EOF parsing fn"),
	}
	loop {
		parameters.push(match rtokens.last() {
			Some(Token::Identifier(_)) => parse_id(&mut rtokens, true),
			Some(Token::RParen) => break,
			Some(_) => panic!("expected Identifier or RParen"),
			None => panic!("unexpected EOF parsing fn"),
		});
		match rtokens.pop() {
			Some(Token::Comma) => (),
			Some(Token::RParen) => break,
			_ => panic!("expected comma or RParen"),
		}
	}
	let return_type = match rtokens.last() {
		Some(Token::Type(_)) => match rtokens.pop() {
			Some(Token::Type(r_type)) => r_type,
			_ => panic!("shouldn't be reachable"),
		},
		Some(Token::Newline) => Type::Infer,
		Some(_) => panic!("expected newline or return type"),
		None => panic!("expected end of signature"),
	};
	match rtokens.pop() {
		Some(Token::Newline) => (),
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
				name,
				signature,
				statements,
			}
		};
		match t {
			Token::Space(text) => {
				let mut chars = text.chars();
				let mut new_count = 1;
				let is_tab = match chars.next() {
					Some('\t') => true,
					Some(' ') => false,
					Some(_) => panic!("lexer gave non-space space"),
					None => {
						println!("WARNING: no-space space (safe to ignore due to hack from lexer)");
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
		statements.push(parse_statement(rtokens));
	}
	Function {
		name,
		signature,
		statements,
	}
}

fn parse(tokens: &mut Vec<Token>) -> AST {
	tokens.reverse();
	// This is just for clarity
	let mut rtokens = tokens;
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
			Token::Fn => {
				ast.push(parse_fn(&mut rtokens));
			},
			Token::Newline => {},
			_ => {
				panic!("expected Fn in global space, got {:?}", t);
			},
		};
		rtokens.pop();
	}
	vec![Function {
		name: String::from("test"),
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

