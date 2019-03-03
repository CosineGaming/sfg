// This is the parser. yay.

use crate::{Token, Type};

pub type AST = Vec<Function>;

pub struct Function {
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
enum Statement {
	Assignment(Assignment),
	FnCall(FnCall),
}
struct Assignment {
	lvalue: Identifier,
	rvalue: Expression,
}
struct FnCall {
	name: String,
	arguments: Vec<Expression>,
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

fn parse_expression(mut rtokens: &mut Vec<Token>) -> Result<Expression, &str> {
	rtokens.pop();
	Ok(5)
}

fn parse_call(mut rtokens: &mut Vec<Token>) -> Result<FnCall, &str> {
	let name = match rtokens.pop() {
		Some(Token::Identifier(name)) => name,
		_ => return Err("expected identifier in function call"),
	};
	// Arguments
	match rtokens.pop() {
		Some(Token::LParen) => (),
		Some(_) => panic!("expected ("),
		None => panic!("unexpected EOF parsing arguments"),
	}
	let mut arguments = vec![];
	loop {
		// TODO: is allowing commas at the beginning the worst idea ive had?
		let result = match rtokens.last() {
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(Token::Comma) => { rtokens.pop(); continue },
			_ => parse_expression(rtokens),
		};
		match result {
			Ok(expr) => arguments.push(expr),
			Err(err) => panic!(format!("{}", err)),
		}
	}
	Ok(FnCall {
		name,
		arguments,
	})
}

fn parse_args(mut rtokens: &mut Vec<Token>) -> Vec<Identifier> {
	let mut args = vec![];
	match rtokens.pop() {
		Some(Token::LParen) => (),
		Some(_) => panic!("expected ("),
		None => panic!("unexpected EOF parsing parameters"),
	}
	loop {
		args.push(match rtokens.last() {
			Some(Token::Identifier(_)) => parse_id(&mut rtokens, true),
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(_) => panic!("expected Identifier or RParen"),
			None => panic!("unexpected EOF parsing parameters"),
		});
		match rtokens.pop() {
			Some(Token::Comma) => (),
			Some(Token::RParen) => break,
			_ => panic!("expected comma or RParen"),
		}
	}
	args
}

fn parse_statement(mut rtokens: &mut Vec<Token>) -> Statement {
	println!("WARNING: assignment statement unimplemented");
	match parse_call(&mut rtokens) {
		Ok(call) => Statement::FnCall(call),
		Err(err) => { println!("{}", err); panic!("^^^^^ this, bro") },
	}
}

fn parse_fn(mut rtokens: &mut Vec<Token>) -> Function {
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
	let parameters = parse_args(&mut rtokens);
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
			Token::Tab(text) => {
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
		match rtokens.pop() {
			Some(Token::Newline) => (),
			None => (),
			Some(t) => panic!("expected newline after statement, got {:?}", t),
		}
	}
	Function {
		name,
		signature,
		statements,
	}
}

pub fn parse(tokens: &mut Vec<Token>) -> AST {
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
		statements: vec![Statement::Assignment(Assignment {
			lvalue: Identifier {
				name: String::from("hi"),
				id_type: Type::Int,
			},
			rvalue: 7040,
		})],
		signature: Signature {
			parameters: vec![],
			return_type: Type::Int,
		}
	}]
}
