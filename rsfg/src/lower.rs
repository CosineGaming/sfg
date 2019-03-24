// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{Type, ast::*, llr};
use indexmap::IndexMap;

// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(state: &mut LowerState, expr: &Expression) -> Type {
	match expr {
		Expression::Literal(lit) => match lit {
			Literal::String(_) => Type::Str,
			Literal::Int(_) => Type::Int,
		},
		Expression::Identifier(id) => {
			match state.locals.get(&id.name) {
				Some(id_type) => *id_type,
				None => panic!("unknown local variable {}", id.name),
			}
		}
		Expression::FnCall(func) => {
			let node = match state.fn_map.get(&*func.name) {
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

struct LowerState<'a> {
	fn_map: IndexMap<String, &'a ASTNode>,
	locals: IndexMap<String, Type>, // String / Index / Type
	strings: &'a mut Vec<String>,
}
impl<'a> LowerState<'a> {
	fn new(ast: &'a AST, strings: &'a mut Vec<String>) -> Self {
		// IndexMap maintains indices of fns
		let mut fn_map = IndexMap::new();
		// Add all functions to the map
		for node in ast.iter() {
			let name = match node {
				ASTNode::Fn(func) => func.signature.name.clone(),
				ASTNode::ExternFn(_) => continue,
			};
			fn_map.insert(name, node);
		}
		// In order to keep numbers consistent, we keep externs after interns at all times
		for node in ast.iter() {
			let name = match node {
				ASTNode::Fn(_) => continue,
				ASTNode::ExternFn(func) => func.signature.name.clone(),
			};
			fn_map.insert(name, node);
		}
		Self {
			fn_map,
			locals: IndexMap::new(),
			strings: strings,
		}
	}
}

fn expression_to_push(state: &mut LowerState, expr: &Expression) -> Vec<llr::Instruction> {
	let LowerState { strings, .. } = state;
	match expr {
		Expression::Literal(Literal::String(string)) => {
			strings.push(string.to_string());
			vec![llr::Instruction::Push32((strings.len()-1) as u32)]
		},
		Expression::Literal(Literal::Int(int)) => {
			vec![llr::Instruction::Push32(i_as_u(*int))]
		},
		// fn call leaves result on the stack which is exactly what we need
		Expression::FnCall(call) => lower_fn_call(state, call, false),
		Expression::Identifier(var) => {
			let mut insts = vec![];
			match state.locals.get_full(&var.name) {
				Some((i,_,_)) => {
					let rindex = (state.locals.len() - i - 1) as u8;
					insts.push(llr::Instruction::Dup(rindex));
				}
				None => panic!("unknown local variable {}", var.name),
			}
			insts
		},
		Expression::Binary(expr) => {
			match expr.op {
				BinaryOp::Equals => {
					let mut insts = vec![];
					insts.append(&mut expression_to_push(state, &expr.left));
					insts.append(&mut expression_to_push(state, &expr.right));
					insts.push(llr::Instruction::Equals);
					insts
				},
			}
		},
	}
}

fn lower_panic(state: &mut LowerState, call: &FnCall) -> Vec<llr::Instruction> {
	let mut insts = vec![];
	for arg in &call.arguments {
		let given_type = expression_type(state, arg);
		if types_match(given_type, Type::Int) == Some(false) {
			panic!("expected type Int but got {:?}", given_type);
		}
		// TODO: storing the line/col in code rather than stack would be:
		// 1. Simpler to compile
		// 2. Safer (eg panic on stack overflow possible)
		// But requires rewriting assert in Rust as well
		// Also: this code is corrently fairly duplicated with lower_fn_call
		let mut push = expression_to_push(state, arg);
		insts.append(&mut push);
	}
	insts.push(llr::Instruction::Panic);
	insts
}

fn lower_fn_call(state: &mut LowerState, call: &FnCall, is_statement: bool) -> Vec<llr::Instruction> {
	let (index, node) = match state.fn_map.get_full(&*call.name) {
		Some((i, _, func)) => (i, func),
		None => match &call.name[..] {
			"panic" => return lower_panic(state, call),
			_ => panic!("could not find function {}", call.name),
		}
	};
	// Typecheck
	// Sig needed for later op
	let is_extern;
	let signature = match node {
		ASTNode::Fn(f) => { is_extern = false; &f.signature },
		ASTNode::ExternFn(f) => { is_extern = true; &f.signature },
	};
	let params = &signature.parameters;
	assert_eq!(call.arguments.len(), params.len(),
		"{} expected {} arguments, got {}",
		call.name, params.len(), call.arguments.len());
	let mut instructions = vec![];
	// Typecheck all arguments calls with their found IDs
	for (i, arg) in call.arguments.iter().enumerate() {
		let param = &params[i];
		let given_type = expression_type(state, arg);
		if types_match(given_type, param.id_type) == Some(false) {
			panic!("expected type {:?} but got {:?}", param.id_type, given_type);
		}
		// Otherwise our types are just fine
		// Now we just have to evaluate it
		let mut push = expression_to_push(state, arg);
		instructions.append(&mut push);
	}
	// Generate lowered call
	let fn_call = llr::FnCall {
		index,
		arg_count: call.arguments.len() as u8,
	};
	let call = match is_extern {
		false => llr::Instruction::FnCall(fn_call),
		true => llr::Instruction::ExternFnCall(fn_call),
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

fn lower_return(state: &mut LowerState, expr: &Option<Expression>, signature: &Signature) -> Vec<llr::Instruction> {
	// Typecheck return value
	// None == None -> return == void
	assert_eq!(expr.as_ref().map(|x| expression_type(state, x)), signature.return_type);
	let mut insts = vec![];
	// TODO: YOU HAVE TO REMOVE THIS BLOCK after you add identifiers
	// This pops every parameter. Once we can use identifiers (params are IDs)
	// we'll ONLY want to pop "unused identifiers"
	// Correction: actually we're cheating and using Dup to make locals
	// without rearranging a DAG
	// So the real TODO is to optimize locals/the stack together
	for _param in &signature.parameters {
		// TODO: Don't assume params are 32s
		insts.push(llr::Instruction::Pop32);
	}
	if let Some(expr) = expr {
		insts.append(&mut expression_to_push(state, &expr));
	}
	insts.push(llr::Instruction::Return);
	insts
}

fn lower_statement(state: &mut LowerState, statement: &Statement, signature: &Signature) -> Vec<llr::Instruction> {
	match statement {
		Statement::FnCall(call) => {
			lower_fn_call(state, call, true)
		}
		Statement::Return(expr) => {
			lower_return(state, expr, signature)
		}
		Statement::If(if_stmt) => {
			let mut push_condition = expression_to_push(state, &if_stmt.condition);
			let mut block = vec![];
			for statement in &if_stmt.statements {
				block.append(&mut lower_statement(state, statement, signature))
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
fn lower_statements(state: &mut LowerState, func: &Fn) -> Vec<llr::Instruction> {
	// We can't keep track of the stack perfectly, but we can assume
	// the stack is clean and then look at what we expect to be there
	//let mut locals = IndexMap::new();
	let mut instructions = Vec::<llr::Instruction>::new();
	// Lower every statement
	for statement in func.statements.iter() {
		instructions.append(&mut lower_statement(state, statement, &func.signature));
	}
	let last_statement_return = instructions.last() == Some(&llr::Instruction::Return);
	// Add implied returns
	// If the final command was a proper return, no need to clean it up
	if !last_statement_return {
		// If the function is empty or didn't end in return we need to add one
		// We can add the implicit void return but not implicit typed return
		// However the error will be handlede properly by lower_return by passing the signature
		instructions.append(&mut lower_return(state, &None, &func.signature));
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

fn lower_fn(state: &mut LowerState, func: &Fn) -> llr::Fn {
	for param in &func.signature.parameters {
		state.locals.insert(param.name.clone(), param.id_type);
	}
	llr::Fn {
		instructions: lower_statements(state, func),
		signature: lower_signature(&func.signature),
	}
}

pub fn lower(ast: AST) -> llr::LLR {
	let mut out = llr::LLR::new();
	// Find all function calls and set their ID to the map's id
	for node in ast.iter() {
		match node {
			ASTNode::Fn(func) => {
				// We generate state for each function to keep locals scoped
				// This might be cleaner if we used a real scoping system
				let mut state = LowerState::new(&ast, &mut out.strings);
				let out_f = lower_fn(&mut state, &func);
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

