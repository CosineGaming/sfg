use crate::{Type, llr::*};

enum Serializable {
	Type(Type),
	Instruction(Instruction),
	Sep,
	Void,
	StringLit,
	FnHeader,
	ExternFnHeader,
}
fn serialize(what: Serializable) -> u8 {
	use Type::*;
	use Serializable as S;
	use Instruction as I;
	let typier = match what {
		// Sep is traditionally 00
		S::Sep => 0x00,
		// Types 1x
		S::Type(Int) => 0x10,
		S::Type(Str) => 0x11,
		S::Type(Infer) => panic!("type not yet inferred by printing"),
		// Other 2x
		S::Void => 0x21,
		// Instructions 3x
		// If we have to use 4x I want to do some deep thinking
		S::Instruction(I::Push32(_)) => 0x30,
		S::Instruction(I::ExternFnCall(_)) => 0x31,
		S::StringLit => 0x32,
		S::FnHeader => 0x33,
		S::ExternFnHeader => 0x34,
		S::Instruction(I::Return) => 0x35,
		S::Instruction(I::FnCall(_)) => 0x36,
		S::Instruction(I::Pop32) => 0x37,
		S::Instruction(I::Equals) => 0x38,
		S::Instruction(I::JumpZero(_)) => 0x39,
		S::Instruction(I::Dup(_)) => 0x3a,
		S::Instruction(I::Panic) => 0x3b,
		S::Instruction(I::Add) => 0x3c,
		S::Instruction(I::Sub) => 0x3d,
		S::Instruction(I::Sub) => 0x3d,
	};
	typier as u8
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
		code.push(serialize(Serializable::Instruction(*instr)));
		use Instruction::*;
		match instr {
			Push32(what) => {
				code.extend_from_slice(&u32_bytes(*what as u32));
			},
			// Besides instruction, the procedure for generating
			// FnCall and ExternFnCall is the same
			FnCall(call) | ExternFnCall(call) => {
				code.extend_from_slice(&u32_bytes(call.index as u32));
			},
			// u8 argument
			Dup(what) => code.push(*what),
			// i8 argument
			JumpZero(what) => code.push(*what),
			// As simple as serializing the instruction
			| Return
			| Pop32
			| Equals
			| Panic
			| Add
			| Sub
			=> {},
		}
	}
	code
}

fn u32_bytes(word: u32) -> [u8; 4] {
	use std::mem::transmute;
	unsafe { transmute(word.to_le()) }
}

fn i8_as_u8(what: i8) -> u8 {
	use std::mem::transmute;
	unsafe { transmute(what) }
}

/// fn_headers / sep | strings / fn_bodies
pub fn gen(tree: LLR) -> Vec<u8> {
	let mut code = b"bcfg".to_vec();
	let mut fn_headers = Vec::new();
	let mut fn_bodies = Vec::new();
	for func in tree.fns {
		fn_headers.push(gen_fn_header(&func.signature));
		fn_bodies.push(gen_fn_body(&func));
	}
	let mut extern_fn_headers = Vec::new();
	for signature in tree.extern_fns {
		extern_fn_headers.push(gen_fn_header(&signature));
	}
	// Calculate the beginning of the bodies
	let mut code_loc = code.len();
	// Add the fn headers...
	code_loc += fn_headers.iter().fold(0, |t,h| t+h.len());
	// And the externs too....
	code_loc += extern_fn_headers.iter().fold(0, |t,h| t+h.len());
	// fn headers have a code_loc (4 bytes) but externs don't
	code_loc += 4 * fn_headers.len();
	code_loc += fn_headers.len() + extern_fn_headers.len(); // Instructions
	// Add strings headers
	for string in &tree.strings {
		code_loc += string.len() + 2; // for the instruction and the \0 at end of string
	}
	// Add the headers with the proper code locations given body sizes
	for (mut header, body) in fn_headers.iter_mut().zip(fn_bodies.iter_mut()) {
		code.push(serialize(Serializable::FnHeader));
		header.append(&mut u32_bytes(code_loc as u32).to_vec());
		code.append(&mut header);
		code_loc += body.len();
	}
	// Add the extern fn headers with no bodies
	for mut header in extern_fn_headers.iter_mut() {
		code.push(serialize(Serializable::ExternFnHeader));
		code.append(&mut header);
	}
	for string in tree.strings {
		code.push(serialize(Serializable::StringLit));
		code.append(&mut string.into_bytes());
		code.push(serialize(Serializable::Sep));
	}
	// Actually add the bodies, after *all* the headers
	for mut body in fn_bodies.iter_mut() {
		code.append(&mut body);
	}
	code
}

