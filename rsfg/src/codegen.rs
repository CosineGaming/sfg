use crate::{Type, llr::*};

enum Command {
	Sep,
	PushStringLit,
	ExternCall,
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
		// Types 1x
		S::Type(Int) => 10,
		S::Type(Str) => 11,
		S::Type(Infer) => panic!("type not yet inferred by printing"),
		// Commands 2x
		S::Command(Sep) => 20,
		S::Command(ExternCall) => 21,
		S::Command(PushStringLit) => 22,
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

fn command(command: Command) -> u8 {
	serialize(Serializable::Command(command))
}

// DOESN'T include the code_loc part of the header
fn gen_fn_header(func: &Fn) -> Vec<u8> {
	// We require the code location but until generation of all
	// the code we can't know where that is
	let mut no_code_loc = Vec::new();
	let mut size = 0;
	for param in &func.signature.parameters {
		let p_type = *param;
		size += type_size(p_type);
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
		let p_type = *param;
		no_code_loc.push(serialize(Serializable::Type(p_type)));
	}
	// name
	no_code_loc.append(&mut func.name.as_bytes().to_vec());
	// sep to finish name
	no_code_loc.push(Command::Sep as u8);
	no_code_loc
}

fn gen_fn_body(function: &Fn) -> Vec<u8> {
	let mut code = Vec::new();
	for statement in &function.statements {
		match statement {
			Statement::PushStringLit(string_key) => {
				code.push(command(Command::PushStringLit));
				code.push(*string_key);
			}
			Statement::ExternFnCall(call) => {
				code.push(command(Command::ExternCall));
				code.extend_from_slice(&usize_bytes(call.index as u32));
			}
		}
	}
	code
}

fn usize_bytes(word: u32) -> [u8; 4] {
	use std::mem::transmute;
	unsafe { transmute(word.to_le()) }
}

pub fn gen(tree: LLR) -> Vec<u8> {
	println!("WARNING: codegen is incomplete!");
	let mut code = b"bcfg".to_vec();
	let mut fn_headers = Vec::new();
	let mut fn_bodies = Vec::new();
	for func in tree.fns {
		fn_headers.push(gen_fn_header(&func));
		fn_bodies.push(gen_fn_body(&func));
	}
	let mut code_loc = code.len(); // beginning of fn headers
	for header in &fn_headers { // finding the beginning by adding fn headers
		code_loc += header.len();
		code_loc += 4; // code_loc word
	}
	// Add the headers with the proper code locations given body sizes
	for (mut header, body) in fn_headers.iter_mut().zip(fn_bodies.iter_mut()) {
		header.append(&mut usize_bytes(code_loc as u32).to_vec());
		code.append(&mut header);
		code_loc += body.len();
	}
	// Actually add the bodies, after *all* the headers
	for mut body in fn_bodies.iter_mut() {
		code.append(&mut body);
	}
	code
}

