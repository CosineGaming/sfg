// This is the parser. yay.

use crate::{Token, Type};

pub type AST = Vec<Function>;

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
	name: String,
	statements: Vec<Statement>,
	signature: Signature,
}
#[derive(PartialEq, Eq, Debug)]
struct Signature {
	parameters: Vec<Identifier>,
	return_type: Type,
}
#[derive(PartialEq, Eq, Debug)]
struct Identifier {
	name: String,
	id_type: Type,
}
#[derive(PartialEq, Eq, Debug)]
enum Statement {
	//Assignment(Assignment),
	FnCall(FnCall),
}
#[derive(PartialEq, Eq, Debug)]
enum Expression {
	Literal(Literal),
	//BinaryExpr,
}
#[derive(PartialEq, Eq, Debug)]
enum Literal {
	String(String),
	//Int(i32),
	//Float(f32),
}
#[derive(PartialEq, Eq, Debug)]
struct Assignment {
	lvalue: Identifier,
	rvalue: Expression,
}
#[derive(PartialEq, Eq, Debug)]
struct FnCall {
	name: String,
	arguments: Vec<Expression>,
}

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
	println!("WARNING: parse expression not implemented");
	match rtokens.last() {
		Some(Token::StringLit(_)) => {
			if let Some(Token::StringLit(string)) = rtokens.pop() {
				return Ok(Expression::Literal(Literal::String(string)));
			} else { unreachable!() }
		},
		_ => panic!("other expressions unimplemented"),
	}
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
			Token::Tab => { rtokens.pop(); },
			_ => break,
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
	ast
}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn hello_world() {
		use super::Token::*;
		let ast = parse(&mut vec![
			Fn,
			Identifier("main".to_string()),
			LParen, RParen,
			Newline,
			Tab,
			Identifier("log".to_string()),
			LParen,
			StringLit("hi".to_string()),
			RParen,
		]);
		assert_eq!(ast, vec![
			Function {
				name: "main".to_string(),
				signature: Signature {
					parameters: vec![],
					return_type: crate::Type::Infer,
				},
				statements: vec![
					Statement::FnCall(
						FnCall {
							name: "log".to_string(),
							arguments: vec![Expression::Literal(
								Literal::String("hi".to_string())
							)],
						}
					)
				],
			}
		]);
	}
}

