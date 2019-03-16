use crate::{Type, llr::*};

enum Serializable {
	Type(Type),
	Instruction(Instruction),
	Sep,
	Void,
}
fn serialize(what: Serializable) -> u8 {
	use Type::*;
	use Serializable as S;
	use Instruction as I;
	let typier = match what {
		// Sep is traditionally 00
		S::Sep => 00,
		// Types 1x
		S::Type(Int) => 10,
		S::Type(Str) => 11,
		S::Type(Infer) => panic!("type not yet inferred by printing"),
		// Other 2x
		S::Void => 21,
		// Instructions 3x
		S::Instruction(I::PushStringLit(_)) => 30,
		S::Instruction(I::ExternFnCall(_)) => 31,
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

/// fn_header:
/// `stack size|return type|number of params|type|*|[name]|0
/// DOESN'T include the code_loc part of the header, which looks like
/// |code loc|.|.|.
/// Which isn't included because not until all headers and bodies are
/// generated can it be computed
fn gen_fn_header(func: &Signature) -> Vec<u8> {
	// We require the code location but until generation of all
	// the code we can't know where that is
	let mut no_code_loc = Vec::new();
	let mut size = 0;
	for param in &func.parameters {
		let p_type = *param;
		size += type_size(p_type);
	}
	// stack size
	no_code_loc.push(size);
	// return type
	no_code_loc.push(match func.return_type {
		Some(rt) => serialize(Serializable::Type(rt)),
		None => serialize(Serializable::Void),
	});
	// number of parameters
	no_code_loc.push(func.parameters.len() as u8);
	// the type of each parameter
	for param in &func.parameters {
		let p_type = *param;
		no_code_loc.push(serialize(Serializable::Type(p_type)));
	}
	// name
	no_code_loc.append(&mut func.name.as_bytes().to_vec());
	// sep to finish name
	no_code_loc.push(serialize(Serializable::Sep));
	no_code_loc
}

fn gen_fn_body(function: &Fn) -> Vec<u8> {
	let mut code = Vec::new();
	for instr in &function.instructions {
		match instr {
			Instruction::PushStringLit(string_key) => {
				code.push(serialize(Serializable::Instruction(*instr)));
				code.push(*string_key);
			}
			Instruction::ExternFnCall(call) => {
				code.push(serialize(Serializable::Instruction(*instr)));
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

/// fn_headers / sep | strings / fn_bodies
pub fn gen(tree: LLR) -> Vec<u8> {
	println!("WARNING: codegen is incomplete!");
	let mut code = b"bcfg".to_vec();
	let mut fn_headers = Vec::new();
	let mut fn_bodies = Vec::new();
	for func in tree.fns {
		println!("{}", func.signature.name.clone());
		fn_headers.push(gen_fn_header(&func.signature));
		fn_bodies.push(gen_fn_body(&func));
	}
	let mut extern_fn_headers = Vec::new();
	for signature in tree.extern_fns {
		println!("{}", signature.name);
		extern_fn_headers.push(gen_fn_header(&signature));
	}
	let mut code_loc = code.len(); // beginning of fn headers
	// Add the fn headers...
	code_loc += fn_headers.iter().fold(0, |t,h| t+h.len());
	// And the externs too....
	code_loc += extern_fn_headers.iter().fold(0, |t,h| t+h.len());
	// fn headers have a code_loc (4 bytes) but externs don't
	code_loc += 4 * fn_headers.len();
	code_loc += 2; // Two separators: fn|externs|strings
	// Add the headers with the proper code locations given body sizes
	for (mut header, body) in fn_headers.iter_mut().zip(fn_bodies.iter_mut()) {
		header.append(&mut usize_bytes(code_loc as u32).to_vec());
		code.append(&mut header);
		code_loc += body.len();
	}
	// Separate fns with extern fns
	code.push(serialize(Serializable::Sep));
	// Add the extern fn headers with no bodies
	for mut header in extern_fn_headers.iter_mut() {
		code.append(&mut header);
	}
	// Separate fns with strings
	code.push(serialize(Serializable::Sep));
	code_loc += 1;
	for string in tree.strings {
		code_loc += string.len() + 1; // for the \0 at end of string
		code.append(&mut string.into_bytes());
		code.push(serialize(Serializable::Sep));
	}
	// Actually add the bodies, after *all* the headers
	for mut body in fn_bodies.iter_mut() {
		code.append(&mut body);
	}
	code
}

