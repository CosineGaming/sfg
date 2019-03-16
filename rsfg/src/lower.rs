// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{Type, ast::*, llr};

fn expression_type(expr: &Expression) -> Type {
	match expr {
		Expression::Literal(lit) => {
			match lit {
				Literal::String(_) => Type::Str,
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

pub fn lower(ast: AST) -> llr::LLR {
	use std::collections::HashMap;
	use indexmap::IndexMap;
	let mut out = llr::LLR::new();
	// Kept separate to enforce order of lookup
	// and maintain indices of fns because externs aren't stored
	let mut gen_fn_map = IndexMap::new();
	let mut fn_map = HashMap::new();
	// Add all functions to the map
	for node in ast.iter() {
		match node {
			ASTNode::Function(func) => {
				fn_map.insert(func.name.clone(), func);
			},
			ASTNode::ExternFn(func) => {
				extern_map.insert(func.name.clone(), func);
			},
		};
	}
	// Find all function calls and set their ID to the map's id
	for node in ast.iter() {
		if let ASTNode::Function(func) = node {
			let mut out_statements = Vec::<llr::Statement>::new();
			for statement in func.statements.iter() {
				out_statements.push(match statement {
					Statement::FnCall(call) => {
						let (index, calling) = match fn_map.get_full(&*call.name) {
							Some((i, _, func)) => (i, func),
							None => {
								match extern_map.get(&*call.name) {
									Some(func) => (-1, func), // Index unimportant
									None => panic!("could not find function {}", call.name),
								}
							}
						};
						// Typecheck
						let params = match calling {
							ASTNode::Function(f) => &f.signature.parameters,
							ASTNode::ExternFn(f) => &f.signature.parameters,
						};
						assert_eq!(call.arguments.len(), params.len(),
							"{} expected {} arguments, got {}",
							call.name, params.len(), call.arguments.len());
						for (i, arg) in call.arguments.iter().enumerate() {
							let param = &params[i];
							let given_type = expression_type(arg);
							if types_match(given_type, param.id_type) == Some(false) {
								panic!("expected type {:?} but got {:?}", param.id_type, given_type);
							}
						}
						// Generate lowered call
							llr::Statement::FnCall(llr::FnCall {
							index,
							arguments: vec![],//TODO
						})
					}
				});
			}
			let out_f = llr::Fn {
				name: func.name.clone(),
				statements: out_statements,
				signature: llr::Signature {
					parameters: vec![], // TODO
					return_type: func.signature.return_type,
				},
				namespace: llr::Namespace::new(),
			};
			out.push(out_f);
		}
	}
	// Typecheck all function calls with their found IDs
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

				}
			}
		}
	}
	out
}

