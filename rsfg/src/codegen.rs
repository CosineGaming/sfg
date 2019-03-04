use crate::{Type, ast};

enum Command {
	Sep,
	Call,
}

enum Serializable {
	Command(Command),
	Type(Type),
}
fn serialize(what: Serializable) -> u8 {
	use Type::*;
	use Serializable as S;
	use Command::*;
	let typier = match what {
		S::Type(Int) => 'i' as u8,
		S::Type(Str) => 'f' as u8,
		S::Type(Infer) => panic!("type not yet inferred by printing"),
		S::Command(Sep) => 0,
		S::Command(Call) => 'c' as u8,
	};
	typier as u8
}

fn type_size(id_type: Type) -> u8 {
	use Type::*;
	match id_type {
		Int => 4,
		// Location and length?
		Str => 8,
		Infer => panic!("type not yet inferred by size check"),
	}
}

// DOESN'T include the code_loc part of the header
fn gen_fn_header(func: &ast::Function) -> Vec<u8> {
	// We require the code location but until generation of all
	// the code we can't know where that is
	let mut no_code_loc = Vec::new();
	let mut size = 0;
	for param in &func.signature.parameters {
		size += type_size(param.id_type);
	}
	// stack size
	no_code_loc.push(size);
	// return type
	no_code_loc.push(match func.signature.return_type {
		Some(rt) => serialize(Serializable::Type(rt)),
		None => 'v' as u8,
	});
	// number of parameters
	no_code_loc.push(func.signature.parameters.len() as u8);
	// the type of each parameter
	for param in &func.signature.parameters {
		no_code_loc.push(serialize(Serializable::Type(param.id_type)));
	}
	// name
	no_code_loc.append(&mut func.name.as_bytes().to_vec());
	// sep to finish name
	no_code_loc.push(Command::Sep as u8);
	no_code_loc
}

fn gen_fn_body(statements: &Vec<ast::Statement>) -> Vec<u8> {
	let mut code = Vec::new();
	for statement in statements {
		match statement {
			ast::Statement::FnCall(call) => {
				code.push(serialize(Serializable::Command(Command::Call)));
				match call.id {
					Some(id) => code.push(id),
					None => panic!("could not find function {}", call.name),
				}
			}
		}
	}
	code
}

fn usize_bytes(word: u32) -> [u8; 4] {
	use std::mem::transmute;
	unsafe { transmute(word.to_le()) }
}

pub fn gen(tree: ast::AST) -> Vec<u8> {
	println!("WARNING: codegen is incomplete!");
	let mut code = b"bcfg".to_vec();
	let mut fn_headers = Vec::new();
	let mut fn_bodies = Vec::new();
	for func in tree {
		fn_headers.push(gen_fn_header(&func));
		fn_bodies.push(gen_fn_body(&func.statements));
	}
	let mut code_loc = code.len(); // beginning of fn headers
	for header in &fn_headers { // finding the beginning by adding fn headers
		code_loc += header.len();
		code_loc += 4; // code_loc word
	}
	for (mut header, mut body) in fn_headers.iter_mut().zip(fn_bodies.iter_mut()) {
		header.append(&mut usize_bytes(code_loc as u32).to_vec());
		code.append(&mut header);
		code_loc += body.len();
		code.append(&mut body);
	}
	code
}

