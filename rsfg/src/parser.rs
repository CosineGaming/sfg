// This is the parser. yay.

use crate::{Token, TokenType, Type, ast::*};

#[derive(Debug)]
enum ParseError {
	// Expected, got
	Expected(Vec<TokenType>, Token),
	CouldNotConstruct(Vec<ParseError>),
	Unsupported(String, usize, usize),
	EOF(String),
}
impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		use ParseError::*;
		match self {
			Expected(expected, got) => {
				let expected_strings: Vec<String> = expected.iter().map(|e| format!("{:?}", e)).collect();
				let expected_str = expected_strings.join(" or ");
				write!(f, "expected {}, got {:?} at {}:{}", expected_str, got.kind, got.line, got.col)
			}
			CouldNotConstruct(errs) => {
				let error_strs: Vec<String> = errs.iter().map(|e| e.to_string()).collect();
				let error_str = error_strs.join("\n\n");
				write!(f, "could not construct any possible variant expected here. the following errors were returned:\n\n{}", error_str)
			}
			Unsupported(what, line, col) => write!(f, "unsupported {} at {}:{}", what, line, col),
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
fn expect_token(rtokens: &mut Tokens, what: TokenType, during: &str) -> Result<Token> {
	match rtokens.pop() {
		Some(token) => if token.kind == what {
			Ok(token)
		} else {
			Err(ParseError::Expected(vec![what], token))
		},
		None => return Err(ParseError::EOF(during.to_string())),
	}
}

fn parse_id(rtokens: &mut Tokens, type_required: bool) -> Result<TypedId> {
	let name = match rb_try!(rtokens, pop_no_eof(rtokens, "identifier")).kind {
		TokenType::Identifier(name) => name,
		_ => panic!("identifier wasn't identifier (compiler bug)"),
	};
	match rtokens.last() {
		Some(Token { kind: TokenType::Colon, .. }) => {
			rtokens.pop();
			let id_type = match rtokens.pop() {
				Some(Token { kind: TokenType::Type(id_type), .. }) => id_type,
				Some(what) => return Err(ParseError::Expected(vec![TokenType::Type(Type::Str), TokenType::Type(Type::Int)], what)),
				None => return Err(ParseError::EOF("identifier".to_string())),
			};
			return Ok(TypedId {
				name,
				id_type,
			});
		}
		Some(got) => if type_required {
			return Err(ParseError::Expected(vec![TokenType::Type(Type::Str), TokenType::Type(Type::Int)], got.clone()));
		},
		None => return Err(ParseError::EOF("identifier".to_string())),
	}
	Ok(TypedId {
		name,
		id_type: Type::Infer,
	})
}

fn parse_binary(rtokens: &mut Tokens, left: Expression) -> Result<BinaryExpr> {
	let op = match rb_try!(rtokens, pop_no_eof(rtokens, "binary expr")) {
		Token { kind: TokenType::Equals, .. } => BinaryOp::Equals,
		Token { kind: TokenType::Plus, .. } => BinaryOp::Plus,
		Token { kind: TokenType::Minus, .. } => BinaryOp::Minus,
		// TODO: Make Expected accept a token or vec of token
		got => return Err(ParseError::Expected(vec![TokenType::Equals], got)),
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
	let left = match rtokens.last() {
		Some(Token { kind: TokenType::StringLit(string), .. }) => {
			rtokens.pop();
			Ok(Expression::Literal(Literal::String(string.clone())))
		},
		Some(Token { kind: TokenType::IntLit(number), .. }) => {
			rtokens.pop();
			Ok(Expression::Literal(Literal::Int(*number)))
		},
		// This minus, because we're parsing an expression, is part of an int literal
		Some(Token{kind:TokenType::Minus,..}) => {
			rtokens.pop();
			match pop_no_eof(rtokens, "expression")? {
				Token{kind:TokenType::IntLit(number),..} =>
					Ok(Expression::Literal(Literal::Int(-1 * number))),
				got => Err(ParseError::Expected(vec![TokenType::IntLit(0)], got)),
			}
		}
		Some(Token { kind: TokenType::Identifier(name), .. }) => {
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
		Some(token) => Err(ParseError::Unsupported("expression".to_string(), token.line, token.col)),
		None => Err(ParseError::EOF("expression".to_string())),
	}?;
	// Then we try to parse a binary expression with it
	rb_ok_or!(rtokens, parse_binary(rtokens, left.clone()).and_then(|x| Ok(Expression::Binary(Box::new(x)))));
	// If not, it's just a unary one
	Ok(left)
}

fn parse_call(rtokens: &mut Tokens) -> Result<FnCall> {
	let first_token = rtokens.last();
	let name = match first_token {
		Some(Token { kind: TokenType::Identifier(_), .. }) => match rtokens.pop() {
			Some(Token { kind: TokenType::Identifier(name), .. }) => name,
			_ => unreachable!(),
		},
		Some(what) => return Err(ParseError::Expected(vec![TokenType::Identifier("".to_string())], what.clone())),
		None => return Err(ParseError::EOF("call".to_string())),
	};
	// Arguments
	rb_try!(rtokens, expect_token(rtokens, TokenType::LParen, "fn call"));
	let mut arguments = vec![];
	loop {
		// TODO: is allowing commas at the beginning the worst idea ive had?
		arguments.push(match rtokens.last() {
			Some(Token { kind: TokenType::RParen, .. }) => { rtokens.pop(); break },
			Some(Token { kind: TokenType::Comma, .. }) => { rtokens.pop(); continue },
			_ => rb_try!(rtokens, parse_expression(rtokens)),
		});
	}
	// Assert has special handling because of line/col args
	if &name[..] == "assert" {
		if arguments.len() == 1 {
			// Safe because we wouldn't be here without a token
			arguments.push(Expression::Literal(Literal::Int(first_token.unwrap().line as i32)));
			arguments.push(Expression::Literal(Literal::Int(first_token.unwrap().col as i32)));
		}
	}
	Ok(FnCall {
		name,
		arguments,
	})
}

fn parse_args(rtokens: &mut Tokens) -> Result<Vec<TypedId>> {
	let mut args = vec![];
	rb_try!(rtokens, expect_token(rtokens, TokenType::LParen, "fn parameters"));
	loop {
		args.push(match rtokens.last() {
			Some(Token { kind: TokenType::Identifier(_), .. }) => rb_try!(rtokens, parse_id(rtokens, true)),
			Some(Token { kind: TokenType::RParen, .. }) => { rtokens.pop(); break },
			Some(got) => return Err(ParseError::Expected(vec![TokenType::Identifier(String::new()), TokenType::RParen], got.clone())),
			None => return Err(ParseError::EOF("parameters".to_string())),
		});
		match pop_no_eof(rtokens, "fn params")? {
			Token { kind: TokenType::Comma, .. } => (),
			Token { kind: TokenType::RParen, .. } => break,
			got => return Err(ParseError::Expected(vec![TokenType::Comma, TokenType::RParen], got)),
		}
	}
	Ok(args)
}

fn parse_return(rtokens: &mut Tokens) -> Result<Option<Expression>> {
	rb_try!(rtokens, expect_token(rtokens, TokenType::Return, "return statement"));
	Ok(match parse_expression(rtokens) {
		Ok(expr) => Some(expr),
		Err(_) => None,
	})
}

fn parse_if(rtokens: &mut Tokens, tabs: usize) -> Result<If> {
	rb_try!(rtokens, expect_token(rtokens, TokenType::If, "if statement"));
	let condition = rb_try!(rtokens, parse_expression(rtokens));
	let statements = rb_try!(rtokens, parse_indented_block(rtokens, tabs+1));
	Ok(If { condition, statements })
}

fn parse_statement(rtokens: &mut Tokens, tabs: usize) -> Result<Statement> {
	let mut errors = vec![];
	errors.push(rb_ok_or!(rtokens, parse_call(rtokens)
	                     .and_then(|x| Ok(Statement::FnCall(x)))));
	errors.push(rb_ok_or!(rtokens, parse_return(rtokens)
	                     .and_then(|x| Ok(Statement::Return(x)))));
	errors.push(rb_ok_or!(rtokens, parse_if(rtokens, tabs)
	                     .and_then(|x| Ok(Statement::If(x)))));
	Err(ParseError::CouldNotConstruct(errors))
}

fn parse_signature(rtokens: &mut Tokens) -> Result<Signature> {
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let name = match rb_try!(rtokens, pop_no_eof(rtokens, "fn")) {
		Token { kind: TokenType::Identifier(name), .. } => name,
		Token { kind: TokenType::ExternFnCall(name), .. } => name,
		got => return Err(ParseError::Expected(vec![TokenType::Identifier(String::new()), TokenType::ExternFnCall(String::new())], got)),
	};
	let parameters = rb_try!(rtokens, parse_args(rtokens));
	let return_type = match rtokens.last() {
		Some(Token { kind: TokenType::Type(_), .. }) => match rtokens.pop() {
			Some(Token { kind: TokenType::Type(r_type), .. }) => Some(r_type),
			_ => unreachable!(),
		},
		Some(Token { kind: TokenType::Newline, .. }) => None,
		Some(got) => return Err(ParseError::Expected(vec![TokenType::Newline, TokenType::Type(Type::Str)], got.clone())),
		None => return Err(ParseError::EOF("signature".to_string())),
	};
	match rtokens.pop() {
		Some(Token { kind: TokenType::Newline, .. }) => (),
		Some(got) => return Err(ParseError::Expected(vec![TokenType::Newline], got)),
		None => return Err(ParseError::EOF("signature".to_string())),
	}
	Ok(Signature {
		name,
		parameters,
		return_type,
	})
}

fn parse_indented_block(rtokens: &mut Tokens, expect_tabs: usize) -> Result<Vec<Statement>> {
	let mut statements = vec![];
	loop {
		// Allow empty lines amongst function an indented statement
		if let Some(Token{kind:TokenType::Newline,..}) = rtokens.last() {
			rtokens.pop();
			continue;
		}
		// This looks safe to me.... it's rb! macro but expanded out a bit
		let saved_tokens = rtokens.clone();
		for _ in 0..expect_tabs {
			if let Err(_) = expect_token(rtokens, TokenType::Tab, "indented block") {
				*rtokens = saved_tokens;
				return Ok(statements);
			}
		}
		// We have to allow a newline /again/ because sometimes there's a tab and then no statement (usually comments or long indented blocks)
		if let Some(Token{kind:TokenType::Newline,..}) = rtokens.last() {
			rtokens.pop();
			continue;
		}
		statements.push(rb_try!(rtokens, parse_statement(rtokens, expect_tabs)));
	}
}

fn parse_fn(rtokens: &mut Tokens) -> Result<Fn> {
	rb_try!(rtokens, expect_token(rtokens, TokenType::Fn, "fn"));
	// Parse signature
	let signature = rb_try!(rtokens, parse_signature(rtokens));
	let statements = rb_try!(rtokens, parse_indented_block(rtokens, 1));
	Ok(Fn {
		signature,
		statements,
	})
}

fn parse_extern_fn(rtokens: &mut Tokens) -> Result<ExternFn> {
	match rtokens.pop() {
		Some(Token { kind: TokenType::ExternFn, .. }) => (),
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
			Token { kind: TokenType::Fn, .. } => {
				ast.push(ASTNode::Fn(parse_fn(&mut rtokens).unwrap()));
			},
			Token { kind: TokenType::ExternFn, .. } => {
				ast.push(ASTNode::ExternFn(parse_extern_fn(&mut rtokens).unwrap()));
			},
			Token { kind: TokenType::Newline, .. } => { rtokens.pop(); },
			_ => {
				panic!("expected Fn in global space, got {:?}", t);
			},
		};
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
		use super::TokenType::*;
		let ast = parse(vec![
			Fn,
			Identifier("main".to_string()),
			LParen, RParen,
			Newline,
			Tab,
			Identifier("main".to_string()),
			LParen,
			RParen,
		].iter().map(|t| Token { kind: t.clone(), line: 0, col: 0 }).collect());
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
		use super::TokenType::*;
		let ast = parse(vec![
			Fn,
			Identifier("main".to_string()),
			LParen, RParen,
			Newline,
			Tab,
			Return,
		].iter().map(|t| Token { kind: t.clone(), line: 0, col: 0 }).collect());
		if let ASTNode::Fn(func) = &ast[0] {
			assert_eq!(func.statements, vec![
				Statement::Return(None),
			]);
		} else { unreachable!() }
	}
}

