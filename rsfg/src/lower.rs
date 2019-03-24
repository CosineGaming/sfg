// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{Type, ast::*, llr};
use indexmap::IndexMap;

// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(expr: &Expression, fn_map: &IndexMap<String, &ASTNode>) -> Type {
	match expr {
		Expression::Literal(lit) => match lit {
			Literal::String(_) => Type::Str,
			Literal::Int(_) => Type::Int,
		},
		Expression::Identifier(id) => id.id_type,
		Expression::FnCall(func) => {
			let node = match fn_map.get(&*func.name) {
				Some(func) => func,
				None => panic!("could not find function {}", func.name),
			};
			let return_type = match node {
				ASTNode::Fn(f) => &f.signature.return_type,
				ASTNode::ExternFn(f) => &f.signature.return_type,
			};
			match return_type {
				Some(what) => *what,
				None => panic!("ERROR: function used as expression is void"),
			}
		},
		Expression::Binary(expr) => {
			match expr.op {
				// TODO: Use Bool type when present
				BinaryOp::Equals => Type::Int,
			}
		},
	}
}

fn type_size(id_type: Type) -> u8 {
	use Type::*;
	match id_type {
		Int | Str => 32,
		Infer => panic!("type not yet inferred by size check"),
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

fn i_as_u(what: i32) -> u32 {
	unsafe { std::mem::transmute::<i32, u32>(what) }
}

fn expression_to_push(expr: &Expression, fn_map: &IndexMap<String, &ASTNode>, strings: &mut Vec<String>) -> Vec<llr::Instruction> {
	match expr {
		Expression::Literal(Literal::String(string)) => {
			strings.push(string.to_string());
			vec![llr::Instruction::Push32((strings.len()-1) as u32)]
		},
		Expression::Literal(Literal::Int(int)) => {
			vec![llr::Instruction::Push32(i_as_u(*int))]
		},
		// fn call leaves result on the stack which is exactly what we need
		Expression::FnCall(call) => lower_fn_call(call, fn_map, strings, false),
		Expression::Identifier(_) => unimplemented!(),
		Expression::Binary(expr) => {
			match expr.op {
				BinaryOp::Equals => {
					let mut insts = vec![];
					insts.append(&mut expression_to_push(&expr.left, fn_map, strings));
					insts.append(&mut expression_to_push(&expr.right, fn_map, strings));
					insts.push(llr::Instruction::Equals);
					insts
				},
			}
		},
	}
}

fn lower_fn_call(call: &FnCall, fn_map: &IndexMap<String, &ASTNode>, strings: &mut Vec<String>, is_statement: bool) -> Vec<llr::Instruction> {
	let (index, node) = match fn_map.get_full(&*call.name) {
		Some((i, _, func)) => (i, func),
		None => panic!("could not find function {}", call.name),
	};
	// Typecheck
	// Sig needed for later op
	let signature = match node {
		ASTNode::Fn(f) => &f.signature,
		ASTNode::ExternFn(f) => &f.signature,
	};
	let params = &signature.parameters;
	assert_eq!(call.arguments.len(), params.len(),
		"{} expected {} arguments, got {}",
		call.name, params.len(), call.arguments.len());
	let mut instructions = vec![];
	// Typecheck all arguments calls with their found IDs
	for (i, arg) in call.arguments.iter().enumerate() {
		let param = &params[i];
		let given_type = expression_type(arg, fn_map);
		if types_match(given_type, param.id_type) == Some(false) {
			panic!("expected type {:?} but got {:?}", param.id_type, given_type);
		}
		// Otherwise our types are just fine
		// Now we just have to evaluate it
		let mut push = expression_to_push(arg, fn_map, strings);
		instructions.append(&mut push);
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
	if is_statement {
		// Return value is unused if so it needs to be popped for balance
		// TODO: Support non u32 return types
		match signature.return_type {
			Some(rt) => match type_size(rt) {
				32 => instructions.push(llr::Instruction::Pop32),
				_ => panic!("non-u32 return types unsupported"),
			},
			None => (),
		}
	}
	instructions
}

fn lower_return(expr: &Option<Expression>, fn_map: &IndexMap<String, &ASTNode>, expected_return: Option<Type>, strings: &mut Vec<String>) -> Vec<llr::Instruction> {
	// Typecheck return value
	// None == None -> return == void
	assert_eq!(expr.as_ref().map(|x| expression_type(x, fn_map)), expected_return);
	let mut insts = vec![];
	if let Some(expr) = expr {
		insts.append(&mut expression_to_push(&expr, fn_map, strings));
	}
	insts.push(llr::Instruction::Return);
	insts
}

fn lower_statement(statement: &Statement, fn_map: &IndexMap<String, &ASTNode>, func_return_type: Option<Type>, strings: &mut Vec<String>) -> Vec<llr::Instruction> {
	match statement {
		Statement::FnCall(call) => {
			lower_fn_call(call, fn_map, strings, true)
		}
		Statement::Return(expr) => {
			lower_return(expr, fn_map, func_return_type, strings)
		}
		Statement::If(if_stmt) => {
			let mut push_condition = expression_to_push(&if_stmt.condition, fn_map, strings);
			let mut block = vec![];
			for statement in &if_stmt.statements {
				block.append(&mut lower_statement(statement, fn_map, func_return_type, strings))
			}
			let mut lowered = vec![];
			lowered.append(&mut push_condition);
			lowered.push(llr::Instruction::JumpZero(block.len() as u8));
			lowered.append(&mut block);
			lowered
		}
	}
}

/// requires mutable reference to llr's strings so strings that come up can be added
fn lower_statements(func: &Fn, fn_map: &IndexMap<String, &ASTNode>, strings: &mut Vec<String>) -> Vec<llr::Instruction> {
	let mut instructions = Vec::<llr::Instruction>::new();
	// TODO: YOU HAVE TO REMOVE THIS BLOCK after you add identifiers
	// This pops every parameter. Once we can use identifiers (params are IDs)
	// we'll ONLY want to pop "unused identifiers"
	for _param in &func.signature.parameters {
		// TODO: Don't assume params are 32s
		instructions.push(llr::Instruction::Pop32);
	}
	// Lower every statement
	for statement in func.statements.iter() {
		instructions.append(&mut lower_statement(statement, fn_map, func.signature.return_type, strings));
	}
	// Add implied returns
	match instructions.last() {
		// If the final command was a proper return, no need to clean it up
		Some(&llr::Instruction::Return) => (),
		// If the function is empty or didn't end in return we need to add one
		_ => if func.signature.return_type == None {
			// We can add the implicit void return
			instructions.append(&mut lower_return(&None, fn_map, None, strings));
		} else {
			// We can't add an implicit return because () != the function type
			panic!("function with type may not return");
		}
	}
	instructions
}

fn lower_signature(signature: &Signature) -> llr::Signature {
	// Lower parameters
	let mut parameters = vec![];
	for param in &signature.parameters {
		parameters.push(param.id_type);
	}
	llr::Signature {
		name: signature.name.clone(),
		parameters: parameters,
		return_type: signature.return_type,
	}
}

fn lower_fn(func: &Fn, fn_map: &IndexMap<String, &ASTNode>, strings: &mut Vec<String>) -> llr::Fn {
	llr::Fn {
		instructions: lower_statements(func, fn_map, strings),
		signature: lower_signature(&func.signature),
	}
}

pub fn lower(ast: AST) -> llr::LLR {
	let mut out = llr::LLR::new();
	// IndexMap maintains indices of fns
	let mut fn_map = IndexMap::new();
	// Add all functions to the map
	for node in ast.iter() {
		let name = match node {
			ASTNode::Fn(func) => func.signature.name.clone(),
			ASTNode::ExternFn(func) => func.signature.name.clone(),
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
				let out_s = lower_signature(&func.signature);
				out.extern_fns.push(out_s);
			}
		}
	}
	out
}

mod test {
	#[test]
	fn non_branching_stack_balance() {
		use super::lower;
		use crate::{llr::Instruction};
		use crate::{parser::parse, lexer::lex};
		let script_string = std::fs::read_to_string("tests/scripts/non-branching.sfg")
			.expect("could not load given file");
		let lexed = lex(&script_string);
		let parsed = parse(lexed);
		let lowered = lower(parsed);
		let fns = lowered.fns;
		// TODO: This should be a usize so it'll panic if pop EVER exceeds push
		// However, this requires STARTING at a no-parameter function
		// because the function calls do pushing that should be popped
		// by the callee
		let mut balance: isize = 0;
		for func in fns {
			println!("{:x?}", func.instructions);
			for inst in func.instructions {
				match inst {
					Instruction::Push32(_) => balance += 8,
					Instruction::Pop32 => balance -= 8,
					_ => (),
				}
			}
		}
		assert_eq!(balance, 0);
	}
}

