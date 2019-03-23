// This is the parser. yay.

use crate::{Token, Type, ast::*};

#[derive(Debug)]
enum ParseError {
	// Expected, got
	Expected(Vec<Token>, Token),
	Unsupported(String),
	EOF(String),
}
impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use ParseError::*;
		match self {
			Expected(expected, got) => {
				let expected_strings: Vec<String> = expected.iter().map(|e| format!("{:?}", e)).collect();
				let expected_str = expected_strings.join(" or ");
				write!(f, "expected {}, got {:?}", expected_str, got)
			}
			Unsupported(what) => write!(f, "unsupported {}", what),
			EOF(parsing) => write!(f, "unexpected EOF parsing {}", parsing),
		}
	}
}
// All relevant details in Display and Debug
impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

/// There is a different calling convention for a parse result because of token handling
macro_rules! tokens_try {
	( $tokens:ident, $res:expr ) => {
		{
			// Allow rollback on error
			let saved_tokens = $tokens.clone();
			println!("len {} @ {}", $tokens.sp, line!());
			match $res {
				Ok(what) => {
					what
				},
				Err(err) => {
					*$tokens = saved_tokens;
					return Err(err);
				}
			}
		}
	}
}
macro_rules! ok_or {
	( $x:expr ) => {
		match $x {
			Ok(what) => {
				return Ok(what);
			},
			Err(err) => {}
		}
	}
}

fn pop_no_eof(from: &mut Tokens, parsing_what: &str) -> Result<Token> {
	match from.pop() {
		Some(token) => Ok(token),
		None => Err(ParseError::EOF(parsing_what.to_string()))
	}
}
/// Only pops if the next token is expected, then returns that token (otherwise Err::EOF)
fn expect_token(rtokens: &mut Tokens, what: Token, during: &str) -> Result<Token> {
	match rtokens.pop() {
		Some(token) => if token == what {
			Ok(token)
		} else {
			Err(ParseError::Expected(vec![what], token))
		},
		None => return Err(ParseError::EOF(during.to_string())),
	}
}

fn parse_id(rtokens: &mut Tokens, type_required: bool) -> Result<TypedId> {
	let name = match tokens_try!(rtokens, pop_no_eof(rtokens, "identifier")) {
		Token::Identifier(name) => name,
		_ => panic!("identifier wasn't identifier (compiler bug)"),
	};
	match rtokens.last() {
		Some(Token::Colon) => {
			rtokens.pop();
			let id_type = match rtokens.pop() {
				Some(Token::Type(id_type)) => id_type,
				Some(what) => return Err(ParseError::Expected(vec![Token::Type(Type::Str), Token::Type(Type::Int)], what)),
				None => return Err(ParseError::EOF("identifier".to_string())),
			};
			return Ok(TypedId {
				name,
				id_type,
			});
		}
		_ => if type_required {
			return Err(ParseError::Expected(vec![Token::Type(Type::Str), Token::Type(Type::Int)], Token::Comma)); // TODO: not Token::Comma, None!
		},
	}
	Ok(TypedId {
		name,
		id_type: Type::Infer,
	})
}

fn parse_expression(rtokens: &mut Tokens) -> Result<Expression> {
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
			// An identifier can start a call or just an identifier
			// If it can be a call...
			return Ok(match parse_call(rtokens) {
				// Let it be a call
				Ok(call) => Expression::FnCall(call),
				// Otherwise just reference the identifier
				Err(_) => if let Token::Identifier(name) = rtokens.pop().unwrap() {
					Expression::Identifier(TypedId {
						name,
						id_type: Type::Infer,
					})
				} else { unreachable!() },
			})
		},
		_ => return Err(ParseError::Unsupported("expression".to_string())),
	}
	unreachable!();
}

fn parse_call(rtokens: &mut Tokens) -> Result<FnCall> {
	let name = match rtokens.last() {
		Some(Token::Identifier(_)) => match rtokens.pop() {
			Some(Token::Identifier(name)) => name,
			_ => unreachable!(),
		},
		Some(what) => return Err(ParseError::Expected(vec![Token::Identifier("".to_string())], what.clone())),
		None => return Err(ParseError::EOF("call".to_string())),
	};
	// Arguments
	tokens_try!(rtokens, expect_token(rtokens, Token::LParen, "fn call"));
	let mut arguments = vec![];
	loop {
		// TODO: is allowing commas at the beginning the worst idea ive tokens_try!(rtokens, had)
		arguments.push(match rtokens.last() {
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(Token::Comma) => { rtokens.pop(); continue },
			_ => tokens_try!(rtokens, parse_expression(rtokens)),
		});
	}
	Ok(FnCall {
		name,
		arguments,
	})
}

fn parse_args(rtokens: &mut Tokens) -> Result<Vec<TypedId>> {
	let mut args = vec![];
	tokens_try!(rtokens, expect_token(rtokens, Token::LParen, "fn parameters"));
	loop {
		println!("{}", rtokens.sp);
		args.push(match rtokens.last() {
			Some(Token::Identifier(_)) => tokens_try!(rtokens, parse_id(rtokens, true)),
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(got) => return Err(ParseError::Expected(vec![Token::Identifier(String::new()), Token::RParen], got.clone())),
			None => return Err(ParseError::EOF("parameters".to_string())),
		});
		match pop_no_eof(rtokens, "fn params")? {
			Token::Comma => (),
			Token::RParen => break,
			got => return Err(ParseError::Expected(vec![Token::Comma, Token::RParen], got)),
		}
	}
	Ok(args)
}

fn parse_return(rtokens: &mut Tokens) -> Result<Option<Expression>> {
	tokens_try!(rtokens, expect_token(rtokens, Token::Return, "return statement"));
	Ok(match parse_expression(rtokens) {
		Ok(expr) => Some(expr),
		Err(_) => None,
	})
}

fn parse_statement(rtokens: &mut Tokens) -> Result<Statement> {
	if let Ok(rv) = parse_call(rtokens) {
		Ok(Statement::FnCall(rv))
	}
	else if let Ok(rv) = parse_return(rtokens) {
		Ok(Statement::Return(rv))
	} else {
		Err(ParseError::Expected(vec![Token::Return, Token::Identifier(String::new())], Token::Comma)) // TODO: not comma, something to refer to "couldn't construct"
	}
}

fn parse_signature(rtokens: &mut Tokens) -> Result<Signature> {
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let name = match tokens_try!(rtokens, pop_no_eof(rtokens, "fn")) {
		Token::Identifier(name) => name,
		Token::ExternFnCall(name) => name,
		got => return Err(ParseError::Expected(vec![Token::Identifier(String::new()), Token::ExternFnCall(String::new())], got)),
	};
	let parameters = tokens_try!(rtokens, parse_args(rtokens));
	let return_type = match rtokens.last() {
		Some(Token::Type(_)) => match rtokens.pop() {
			Some(Token::Type(r_type)) => Some(r_type),
			_ => unreachable!(),
		},
		Some(Token::Newline) => None,
		Some(got) => return Err(ParseError::Expected(vec![Token::Newline, Token::Type(Type::Str)], got.clone())),
		None => return Err(ParseError::EOF("signature".to_string())),
	};
	match rtokens.pop() {
		Some(Token::Newline) => (),
		Some(got) => return Err(ParseError::Expected(vec![Token::Newline], got)),
		None => return Err(ParseError::EOF("signature".to_string())),
	}
	Ok(Signature {
		name,
		parameters,
		return_type,
	})
}

fn parse_fn(rtokens: &mut Tokens) -> Result<Fn> {
	tokens_try!(rtokens, expect_token(rtokens, Token::Fn, "fn"));
	// Parse signature
	let signature = tokens_try!(rtokens, parse_signature(rtokens));
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
		statements.push(tokens_try!(rtokens, parse_statement(rtokens)));
		match rtokens.pop() {
			Some(Token::Newline) => (),
			None => (),
			Some(got) => return Err(ParseError::Expected(vec![Token::Newline], got)),
		}
	}
	Ok(Fn {
		signature,
		statements,
	})
}

fn parse_extern_fn(rtokens: &mut Tokens) -> Result<ExternFn> {
	match rtokens.pop() {
		Some(Token::ExternFn) => (),
		_ => panic!("internal: extern fn didn't start with @fn"),
	}
	let signature = tokens_try!(rtokens, parse_signature(rtokens));
	Ok(ExternFn {
		signature,
	})
}

pub fn parse(mut tokens: Vec<Token>) -> AST {
	tokens.reverse();
	// This is just for clarity
	let mut rtokens = NoPop::new(&tokens);
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

#[derive(Clone, Copy)]
struct NoPop<'a, T:Clone> {
	vec: &'a Vec<T>,
	sp: usize,
}
impl<'a, T:Clone> NoPop<'a, T> {
	fn new(vec: &'a Vec<T>) -> Self {
		Self {
			vec,
			sp: vec.len(),
		}
	}
	fn pop(&mut self) -> Option<T> {
		if self.sp > 0 {
			self.sp -= 1;
			Some(self.vec[self.sp].clone())
		} else {
			None
		}
	}
	fn last(&mut self) -> Option<&'a T> {
		if self.sp > 0 {
			self.vec.get(self.sp-1)
		} else {
			None
		}
	}
}
type Tokens<'a> = NoPop<'a, Token>;

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

