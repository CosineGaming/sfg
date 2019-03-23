// This is the parser. yay.

use crate::{Token, Type, ast::*};

#[derive(Debug)]
enum ParseError {
	// Expected, got
	Expected(Vec<Token>, Token),
	CouldNotConstruct(Vec<ParseError>),
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
			CouldNotConstruct(errs) => {
				let error_strs: Vec<String> = errs.iter().map(|e| e.to_string()).collect();
				let error_str = error_strs.join("\n\n");
				write!(f, "could not construct any possible variant expected here. the following errors were returned:\n\n{}", error_str)
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
/// Rolls back tokens if try fails
macro_rules! rb {
	( $tokens:ident, $call:expr ) => {
		{
			// Allow rollback on error
			let saved_tokens = $tokens.clone();
			// Rollback on any error, regardless of intended use
			let res = $call;
			if let Err(_) = res {
				*$tokens = saved_tokens;
			}
			res
		}
	}
}
/// Combines rb with try/?
macro_rules! rb_try {
	( $tokens:ident, $call:expr ) => {
		rb!($tokens, $call)?
	}
}
/// Combines rb with ok_or, which is a hypothetical macro
/// ok_or returns result on ok, or on err continues on as normal
macro_rules! rb_ok_or {
	( $tokens:ident, $call:expr ) => {
		match rb!($tokens, $call) {
			Ok(what) => return Ok(what),
			Err(err) => err,
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
	let name = match rb_try!(rtokens, pop_no_eof(rtokens, "identifier")) {
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

fn parse_binary(rtokens: &mut Tokens, left: Expression) -> Result<BinaryExpr> {
	let op = match rb_try!(rtokens, pop_no_eof(rtokens, "binary expr")) {
		Token::Equals => BinaryOp::Equals,
		// TODO: Make Expected accept a token or vec of token
		got => return Err(ParseError::Expected(vec![Token::Equals], got)),
	};
	let right = rb_try!(rtokens, parse_expression(rtokens));
	Ok(BinaryExpr {
		left,
		op,
		right,
	})
}

fn parse_expression(rtokens: &mut Tokens) -> Result<Expression> {
	// First we parse the left side of a binary expression which COULD be the whole expression
	println!("{:?}", rtokens.last());
	let left = match rtokens.last() {
		Some(Token::StringLit(string)) => {
			rtokens.pop();
			Ok(Expression::Literal(Literal::String(string.clone())))
		},
		Some(Token::IntLit(number)) => {
			rtokens.pop();
			Ok(Expression::Literal(Literal::Int(*number)))
		},
		Some(Token::Identifier(name)) => {
			// An identifier can start a call or just an identifier
			// It can be a call...
			let call_res = rb!(rtokens, parse_call(rtokens).and_then(|x| Ok(Expression::FnCall(x))));
			// This is kinda messy, i wish i could short circuit it more like rb_ok_or
			if let Err(_) = call_res {
				rtokens.pop();
				// Otherwise just reference the identifier
				Ok(Expression::Identifier(TypedId {
					name: name.clone(),
					id_type: Type::Infer,
				}))
			} else {
				call_res
			}
		},
		_ => Err(ParseError::Unsupported("expression".to_string())),
	}?;
	// Then we try to parse a binary expression with it
	rb_ok_or!(rtokens, parse_binary(rtokens, left.clone()).and_then(|x| Ok(Expression::Binary(Box::new(x)))));
	// If not, it's just a unary one
	Ok(left)
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
	rb_try!(rtokens, expect_token(rtokens, Token::LParen, "fn call"));
	let mut arguments = vec![];
	loop {
		// TODO: is allowing commas at the beginning the worst idea ive rb_try!(rtokens, had)
		arguments.push(match rtokens.last() {
			Some(Token::RParen) => { rtokens.pop(); break },
			Some(Token::Comma) => { rtokens.pop(); continue },
			_ => rb_try!(rtokens, parse_expression(rtokens)),
		});
	}
	Ok(FnCall {
		name,
		arguments,
	})
}

fn parse_args(rtokens: &mut Tokens) -> Result<Vec<TypedId>> {
	let mut args = vec![];
	rb_try!(rtokens, expect_token(rtokens, Token::LParen, "fn parameters"));
	loop {
		args.push(match rtokens.last() {
			Some(Token::Identifier(_)) => rb_try!(rtokens, parse_id(rtokens, true)),
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
	rb_try!(rtokens, expect_token(rtokens, Token::Return, "return statement"));
	Ok(match parse_expression(rtokens) {
		Ok(expr) => Some(expr),
		Err(_) => None,
	})
}

fn parse_statement(rtokens: &mut Tokens) -> Result<Statement> {
	let mut errors = vec![];
	errors.push(rb_ok_or!(rtokens, parse_call(rtokens)
	                     .and_then(|x| Ok(Statement::FnCall(x)))));
	errors.push(rb_ok_or!(rtokens, parse_return(rtokens)
	                     .and_then(|x| Ok(Statement::Return(x)))));
	Err(ParseError::CouldNotConstruct(errors))
}

fn parse_signature(rtokens: &mut Tokens) -> Result<Signature> {
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let name = match rb_try!(rtokens, pop_no_eof(rtokens, "fn")) {
		Token::Identifier(name) => name,
		Token::ExternFnCall(name) => name,
		got => return Err(ParseError::Expected(vec![Token::Identifier(String::new()), Token::ExternFnCall(String::new())], got)),
	};
	let parameters = rb_try!(rtokens, parse_args(rtokens));
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
	rb_try!(rtokens, expect_token(rtokens, Token::Fn, "fn"));
	// Parse signature
	let signature = rb_try!(rtokens, parse_signature(rtokens));
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
		statements.push(rb_try!(rtokens, parse_statement(rtokens)));
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
	let signature = rb_try!(rtokens, parse_signature(rtokens));
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
	#[test]
	fn parse_expression_rollback() {
		// The code is listed in order FnCall, then return
		// We need to test to make sure it can roll back properly
		use super::Token::*;
		let ast = parse(vec![
			Fn,
			Identifier("main".to_string()),
			LParen, RParen,
			Newline,
			Tab,
			Return,
		]);
		if let ASTNode::Fn(func) = &ast[0] {
			assert_eq!(func.statements, vec![
				Statement::Return(None),
			]);
		} else { unreachable!() }
	}
}

