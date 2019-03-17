// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{Type, ast::*, llr};
use indexmap::IndexMap;

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

/// requires mutable reference to llr's strings so strings that come up can be added
fn lower_fn(func: &Fn, fn_map: &IndexMap<String, &ASTNode>, out_strings: &mut Vec<String>) -> llr::Fn {
	let mut instructions = Vec::<llr::Instruction>::new();
	for statement in func.statements.iter() {
		match statement {
			Statement::FnCall(call) => {
				let (index, node) = match fn_map.get_full(&*call.name) {
					Some((i, _, func)) => (i, func),
					None => panic!("could not find function {}", call.name),
				};
				// Typecheck
				let params = match node {
					ASTNode::Fn(f) => &f.signature.parameters,
					ASTNode::ExternFn(f) => &f.signature.parameters,
				};
				assert_eq!(call.arguments.len(), params.len(),
					"{} expected {} arguments, got {}",
					call.name, params.len(), call.arguments.len());
				// Typecheck all arguments calls with their found IDs
				for (i, arg) in call.arguments.iter().enumerate() {
					let param = &params[i];
					let given_type = expression_type(arg);
					if types_match(given_type, param.id_type) == Some(false) {
						panic!("expected type {:?} but got {:?}", param.id_type, given_type);
					}
					// Otherwise our types are just fine
					// Now we just have to evaluate it
					let push = match arg {
						Expression::Literal(Literal::String(string)) => {
							out_strings.push(string.to_string());
							llr::Instruction::PushStringLit((out_strings.len()-1) as u8)
						},
						_ => panic!("not yet implemented: variables or non-string literals"),
					};
					instructions.push(push);
				}
				// Generate lowered call
				let fn_call = llr::FnCall {
					index,
					arg_count: call.arguments.len() as u8,
				};
				let call = match node {
					ASTNode::Fn(_) => llr::Instruction::FnCall(fn_call),
					ASTNode::ExternFn(_) => llr::Instruction::ExternFnCall(fn_call),
				};
				instructions.push(call);
			}
		}
	}
	// TODO: Instead, check if something was returned and if not (and typed) throw error
	if func.signature.return_type == None {
		instructions.push(llr::Instruction::Return);
	}
	let mut parameters = vec![];
	for param in &func.signature.parameters {
		parameters.push(param.id_type);
	}
	llr::Fn {
		instructions,
		signature: llr::Signature {
			name: func.name.clone(),
			parameters: parameters,
			return_type: func.signature.return_type,
		},
	}
}

pub fn lower(ast: AST) -> llr::LLR {
	let mut out = llr::LLR::new();
	// IndexMap maintains indices of fns
	let mut fn_map = IndexMap::new();
	// Add all functions to the map
	for node in ast.iter() {
		let name = match node {
			ASTNode::Fn(func) => func.name.clone(),
			ASTNode::ExternFn(func) => func.name.clone(),
		};
		fn_map.insert(name, node);
	}
	// Find all function calls and set their ID to the map's id
	for node in ast.iter() {
		match node {
			ASTNode::Fn(func) => {
				let out_f = lower_fn(&func, &fn_map, &mut out.strings);
				out.fns.push(out_f);
			},
			ASTNode::ExternFn(func) => {
				let mut parameters = vec![];
				for param in &func.signature.parameters {
					parameters.push(param.id_type);
				}
				let out_f = llr::Signature {
					name: func.name.clone(),
					parameters,
					return_type: func.signature.return_type,
				};
				out.extern_fns.push(out_f);
			}
		}
	}
	out
}

