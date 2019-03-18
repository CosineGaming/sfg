// This is the parser. yay.

use crate::{Token, Type, ast::*};

#[derive(Debug)]
enum ParseError {
	Expected(String, String),
	Unsupported(String),
	EOF(String),
	Other(String),
}
impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use ParseError::*;
		match self {
			Expected(what, after) => write!(f, "expected {} after {}", what, after),
			Unsupported(what) => write!(f, "unsupported {}", what),
			EOF(parsing) => write!(f, "unexpected EOF parsing {}", parsing),
			Other(msg) => write!(f, "{}", msg),
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
fn expect_token(rtokens: &mut Vec<Token>, what: Token, during: &str) -> Result<()> {
	let next = pop_no_eof(rtokens, during)?;
	if next == what {
		Ok(())
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

fn parse_expression(rtokens: &mut Vec<Token>) -> Result<Expression> {
	match rtokens.last() {
		Some(Token::StringLit(_)) => {
			if let Some(Token::StringLit(string)) = rtokens.pop() {
				return Ok(Expression::Literal(Literal::String(string)));
			}
		},
		Some(Token::IntLit(_)) => {
			if let Token::IntLit(number) = rtokens.pop().unwrap() {
				return Ok(Expression::Literal(Literal::Int(number)));
			}
		},
		Some(Token::Identifier(_)) => {
			if let Token::Identifier(name) = rtokens.pop().unwrap() {
				return Ok(Expression::Identifier(TypedId {
					name: name,
					id_type: Type::Infer,
				}));
			}
		},
		_ => return Err(ParseError::Unsupported("expression".to_string())),
	}
	unreachable!();
}

fn parse_call(rtokens: &mut Vec<Token>) -> Result<FnCall> {
	let name = match rtokens.pop() {
		Some(Token::Identifier(name)) => name,
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

fn parse_statement(rtokens: &mut Vec<Token>) -> Result<Statement> {
	Ok(Statement::FnCall(parse_call(rtokens)?))
}

fn parse_signature(rtokens: &mut Vec<Token>) -> Result<Signature> {
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
		parameters: parameters,
		return_type: return_type,
	})
}

fn parse_fn(rtokens: &mut Vec<Token>) -> Result<Fn> {
	// Parse signature
	match rtokens.pop() {
		Some(Token::Fn) => (),
		_ => panic!("internal: fn didn't start with fn"),
	}
	let name = match pop_no_eof(rtokens, "fn")? {
		Token::Identifier(name) => name,
		_ => return Err(ParseError::Expected("fn name".to_string(), "fn".to_string())),
	};
	let signature = parse_signature(rtokens)?;
	let mut statements = vec![];
	loop {
		let t = match rtokens.last() {
			Some(t) => t,
			None => return Ok(Fn {
				name,
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
		name,
		signature,
		statements,
	})
}

fn parse_extern_fn(rtokens: &mut Vec<Token>) -> Result<ExternFn> {
	match rtokens.pop() {
		Some(Token::ExternFn) => (),
		_ => panic!("internal: extern fn didn't start with @fn"),
	}
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let name = match pop_no_eof(rtokens, "fn")? {
		Token::ExternFnCall(name) => name,
		Token::Identifier(name) => name,
		_ => return Err(ParseError::Other("expected name of function, with or without leading @".to_string())),
	};
	let signature = parse_signature(rtokens)?;
	Ok(ExternFn {
		name,
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
				name: "main".to_string(),
				signature: Signature {
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

