// This is the parser. yay.

use crate::{Token, Type, ast::*};

#[derive(Debug)]
enum ParseError {
	Expected(String, String),
	Unsupported(String),
	EOF(String),
}
impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use ParseError::*;
		match self {
			Expected(what, after) => write!(f, "expected {} after {}", what, after),
			Unsupported(what) => write!(f, "unsupported {}", what),
			EOF(parsing) => write!(f, "unexpected EOF parsing {}", parsing),
		}
	}
}
// All relevant details in Display and Debug
impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

fn pop_no_eof(from: &mut Vec<Token>, parsing_what: &str) -> Result<Token> {
	match from.pop() {
		Some(token) => Ok(token),
		None => Err(ParseError::EOF(parsing_what.to_string()))
	}
}
/// Only pops if the next token is expected, then returns that token (otherwise Err::EOF)
fn expect_token(rtokens: &mut Vec<Token>, what: Token, during: &str) -> Result<Token> {
	let next = match rtokens.last() {
		Some(token) => token,
		None => return Err(ParseError::EOF(during.to_string())),
	};
	if next == &what {
		Ok(rtokens.pop().unwrap())
	} else {
		Err(ParseError::Expected(format!("{:?}", what), during.to_string()))
	}
}

fn parse_id(rtokens: &mut Vec<Token>, type_required: bool) -> Result<TypedId> {
	let name = match pop_no_eof(rtokens, "identifier")? {
		Token::Identifier(name) => name,
		_ => panic!("identifier wasn't identifier (compiler bug)"),
	};
	match rtokens.last() {
		Some(Token::Colon) => {
			rtokens.pop();
			let id_type = match rtokens.pop() {
				Some(Token::Type(id_type)) => id_type,
				_ => return Err(ParseError::Expected(String::from("type"), String::from("colon"))),
			};
			return Ok(TypedId {
				name,
				id_type,
			});
		}
		_ => if type_required {
			return Err(ParseError::Expected(String::from("type"), name));
		},
	}
	Ok(TypedId {
		name,
		id_type: Type::Infer,
	})
}

macro_rules! return_or {
	($e: expr) => {
		match $e {
			Ok(value) => return Ok(value),
			Err(error) => error,
		}
	}
}

fn parse_binary(out_rtokens: &mut Vec<Token>) -> Result<BinaryExpr> {
	let rtokens = &mut out_rtokens.clone();
	let left = parse_expression(rtokens)?;
	let op = match rtokens.pop() {
		Some(Token::Equals) => BinaryOp::Equals,
		// TODO: Make Expected accept a token or vec of token
		_ => return Err(ParseError::Expected("binary operator".to_string(), "expression".to_string())),
	};
	let right = parse_expression(rtokens)?;
	out_rtokens.resize(rtokens.len(), Token::Newline);
	Ok(BinaryExpr {
		left,
		op,
		right,
	})
}

fn parse_expression(rtokens: &mut Vec<Token>) -> Result<Expression> {
	match rtokens.pop() {
		Some(Token::StringLit(string)) => {
			Expression::Literal(Literal::String(string.clone()));
		},
		Some(Token::IntLit(number)) => {
			Expression::Literal(Literal::Int(number));
		},
		Some(Token::Identifier(name)) => {
			// An identifier can start a call or just an identifier
			// It can be a call...
			return_or!(parse_call(rtokens).and_then(|x| Ok(Expression::FnCall(x))));
			// Otherwise just reference the identifier
			if let Token::Identifier(name) = rtokens.pop().unwrap() {
				return Ok(Expression::Identifier(TypedId {
					name: name,
					id_type: Type::Infer,
				}))
			}
		},
		_ => return Err(ParseError::Unsupported("expression".to_string())),
	}
	return_or!(parse_binary(rtokens).and_then(|x| Ok(Expression::Binary(Box::new(x)))));
	unreachable!();
}

fn parse_call(rtokens: &mut Vec<Token>) -> Result<FnCall> {
	let name = match rtokens.last() {
		Some(Token::Identifier(_)) => match rtokens.pop() {
			Some(Token::Identifier(name)) => name,
			_ => unreachable!(),
		},
		_ => return Err(ParseError::Expected("identifier".to_string(), "fn call".to_string())),
	};
	// Arguments
	expect_token(rtokens, Token::LParen, "fn call")?;
	let mut arguments = vec![];
	loop {
		// TODO: is allowing commas at the beginning the worst idea ive had?
		arguments.push(match rtokens.last() {
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(Token::Comma) => { rtokens.pop(); continue },
			_ => parse_expression(rtokens),
		}?);
	}
	Ok(FnCall {
		name,
		arguments,
	})
}

fn parse_args(rtokens: &mut Vec<Token>) -> Result<Vec<TypedId>> {
	let mut args = vec![];
	expect_token(rtokens, Token::LParen, "fn parameters")?;
	loop {
		args.push(match rtokens.last() {
			Some(Token::Identifier(_)) => parse_id(rtokens, true)?,
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(_) => return Err(ParseError::Expected("Identifier or RParen".to_string(), "LParen".to_string())),
			None => return Err(ParseError::EOF("parameters".to_string())),
		});
		match rtokens.pop() {
			Some(Token::Comma) => (),
			Some(Token::RParen) => break,
			_ => return Err(ParseError::Expected("comma or RParen".to_string(), "argument".to_string())),
		}
	}
	Ok(args)
}

fn parse_return(rtokens: &mut Vec<Token>) -> Result<Option<Expression>> {
	expect_token(rtokens, Token::Return, "return statement")?;
	Ok(match parse_expression(rtokens) {
		Ok(expr) => Some(expr),
		Err(_) => None,
	})
}

fn parse_statement(rtokens: &mut Vec<Token>) -> Result<Statement> {
	if let Ok(rv) = parse_call(rtokens) {
		Ok(Statement::FnCall(rv))
	}
	else if let Ok(rv) = parse_return(rtokens) {
		Ok(Statement::Return(rv))
	} else {
		Err(ParseError::Expected("return or function call".to_string(), "statement".to_string()))
	}
}

fn parse_signature(rtokens: &mut Vec<Token>) -> Result<Signature> {
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let name = match pop_no_eof(rtokens, "fn")? {
		Token::Identifier(name) => name,
		Token::ExternFnCall(name) => name,
		_ => return Err(ParseError::Expected("fn name".to_string(), "fn".to_string())),
	};
	let parameters = parse_args(rtokens)?;
	let return_type = match rtokens.last() {
		Some(Token::Type(_)) => match rtokens.pop() {
			Some(Token::Type(r_type)) => Some(r_type),
			_ => unreachable!(),
		},
		Some(Token::Newline) => None,
		Some(_) => return Err(ParseError::Expected("newline or return type".to_string(), "signature".to_string())),
		None => return Err(ParseError::EOF("signature".to_string())),
	};
	match rtokens.pop() {
		Some(Token::Newline) => (),
		_ => return Err(ParseError::Expected("newline".to_string(), "definition".to_string())),
	}
	Ok(Signature {
		name,
		parameters,
		return_type,
	})
}

fn parse_fn(rtokens: &mut Vec<Token>) -> Result<Fn> {
	expect_token(rtokens, Token::Fn, "fn")?;
	// Parse signature
	let signature = parse_signature(rtokens)?;
	let mut statements = vec![];
	loop {
		let t = match rtokens.last() {
			Some(t) => t,
			None => return Ok(Fn {
				signature,
				statements,
			})
		};
		match t {
			Token::Tab => { rtokens.pop(); },
			_ => break,
		}
		statements.push(parse_statement(rtokens)?);
		match rtokens.pop() {
			Some(Token::Newline) => (),
			None => (),
			Some(_) => return Err(ParseError::Expected("newline".to_string(), "statement".to_string())),
		}
	}
	Ok(Fn {
		signature,
		statements,
	})
}

fn parse_extern_fn(rtokens: &mut Vec<Token>) -> Result<ExternFn> {
	match rtokens.pop() {
		Some(Token::ExternFn) => (),
		_ => panic!("internal: extern fn didn't start with @fn"),
	}
	let signature = parse_signature(rtokens)?;
	Ok(ExternFn {
		signature,
	})
}

pub fn parse(mut tokens: Vec<Token>) -> AST {
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
				ast.push(ASTNode::Fn(parse_fn(&mut rtokens).unwrap()));
			},
			Token::ExternFn => {
				ast.push(ASTNode::ExternFn(parse_extern_fn(&mut rtokens).unwrap()));
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
	// This is NOT meant to test recursion, it's meant as a hello-world
	// that only uses function definition and calling, which means we have
	// no one to call but ourselves
	fn recurse() {
		use super::Token::*;
		let ast = parse(vec![
			Fn,
			Identifier("main".to_string()),
			LParen, RParen,
			Newline,
			Tab,
			Identifier("main".to_string()),
			LParen,
			RParen,
		]);
		assert_eq!(ast, vec![
			ASTNode::Fn(crate::ast::Fn {
				signature: Signature {
					name: "main".to_string(),
					parameters: vec![],
					return_type: None,
				},
				statements: vec![
					Statement::FnCall(
						FnCall {
							name: "main".to_string(),
							arguments: vec![],
						}
					)
				],
			})
		]);
	}
}

