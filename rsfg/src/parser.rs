// This is the parser. yay.

use crate::{Token, Type, ast::*};

fn pop_no_eof(from: &mut Vec<Token>, parsing_what: &str) -> Token {
	from.pop().expect(&format!("enexpected EOF parsing {}", parsing_what))
}

fn parse_id(rtokens: &mut Vec<Token>, type_required: bool) -> TypedId {
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
			return TypedId {
				name,
				id_type,
			};
		}
		_ => if type_required {
			panic!("type required and not given for {}", name);
		},
	}
	TypedId {
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
		_ => return Err("other expressions unimplemented"),
	}
	unreachable!();
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
		id: None,
	})
}

fn parse_args(mut rtokens: &mut Vec<Token>) -> Vec<TypedId> {
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

fn parse_signature(mut rtokens: &mut Vec<Token>) -> Signature {
	let parameters = parse_args(&mut rtokens);
	let return_type = match rtokens.last() {
		Some(Token::Type(_)) => match rtokens.pop() {
			Some(Token::Type(r_type)) => Some(r_type),
			_ => panic!("shouldn't be reachable"),
		},
		Some(Token::Newline) => None,
		Some(_) => panic!("expected newline or return type"),
		None => panic!("expected end of signature"),
	};
	match rtokens.pop() {
		Some(Token::Newline) => (),
		_ => panic!("expected newline after definition"),
	}
	Signature {
		parameters: parameters,
		return_type: return_type,
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
	let signature = parse_signature(&mut rtokens);
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

fn expression_type(expr: &Expression) -> Type {
	match expr {
		Expression::Literal(lit) => {
			match lit {
				Literal::String(_) => Type::Int, // TODO: Add String type and change to that
				Literal::Int(_) => Type::Int,
			}
		},
		Expression::Identifier(id) => id.id_type,
	}
}

// Some(true) is like (Int, Int) OR (Int, Infer)
// Some(false) is like (Int, String)
// None is (Infer, Infer)
fn types_match(a: Type, b: Type) -> Option<bool> {
	if a == Type::Infer && b == Type::Infer { None }
	else if a == b { Some(true) }
	else { Some(false) }
}

fn fill_out_ast(ast: &mut AST) {
	use std::collections::HashMap;
	// Nothing to do rn, but this will infer types etc
	let mut fn_map = HashMap::<String, u8>::new();
	for (i, node) in ast.iter().enumerate() {
		match node {
			ASTNode::Function(func) =>
				fn_map.insert(func.name.clone(), i as u8),
			ASTNode::ExternFn(func) =>
				fn_map.insert(func.name.clone(), i as u8),
		};
	}
	for node in ast.iter_mut() {
		if let ASTNode::Function(func) = node {
			for statement in func.statements.iter_mut() {
				if let Statement::FnCall(ref mut call) = statement {
					let i = fn_map.get(&*call.name);
					if i == None {
						panic!("could not find function {}", call.name);
					}
					call.id = i.and_then(|x|Some(*x));
				}
			}
		}
	}
	for node in ast.iter() {
		if let ASTNode::Function(func) = node {
			for statement in func.statements.iter() {
				if let Statement::FnCall(call) = statement {
					let call_id = call.id.expect("call ids should be found by now");
					let calling = &ast[call_id as usize];
					let params = match calling {
						ASTNode::Function(f) => &f.signature.parameters,
						ASTNode::ExternFn(f) => &f.signature.parameters,
					};
					for (i, arg) in call.arguments.iter().enumerate() {
						let param = &params[i];
						let given_type = expression_type(arg);
						if types_match(given_type, param.id_type) == Some(false) {
							panic!("expected type {:?} but got {:?}", param.id_type, given_type);
						}
					}
				}
			}
		}
	}
}


pub fn parse_extern_fn(mut rtokens: &mut Vec<Token>) -> ExternFn {
	match rtokens.pop() {
		Some(Token::ExternFn) => (),
		Some(_) => panic!("extern fn didn't start with @fn"),
		None => panic!("expected signature after @fn"),
	}
	// An extern function that serves only as a typecheck might use the
	// @ in the name. The lexer misinterprets this as ExternFnCall despite
	// not being a call
	let mut includes_at;
	let name = match rtokens.pop() {
		Some(Token::ExternFnCall(name)) => { includes_at = true; name },
		Some(Token::Identifier(name)) => { includes_at = false; name },
		Some(_) => panic!("expected name of function, with or without leading @"),
		None => panic!("unexpected EOF parsing fn"),
	};
	let signature = parse_signature(&mut rtokens);
	ExternFn {
		name,
		signature,
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
				ast.push(ASTNode::Function(parse_fn(&mut rtokens)));
			},
			Token::ExternFn => {
				ast.push(ASTNode::ExternFn(parse_extern_fn(&mut rtokens)));
			},
			Token::Newline => {},
			_ => {
				panic!("expected Fn in global space, got {:?}", t);
			},
		};
		rtokens.pop();
	}
	fill_out_ast(&mut ast);
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
			ASTNode::Function(Function {
				name: "main".to_string(),
				signature: Signature {
					parameters: vec![],
					return_type: None,
				},
				statements: vec![
					Statement::FnCall(
						FnCall {
							name: "log".to_string(),
							id: None,
							arguments: vec![Expression::Literal(
								Literal::String("hi".to_string())
							)],
						}
					)
				],
			})
		]);
	}
}

