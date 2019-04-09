use crate::{Type, llr::*};

static DEBUG: bool = true;

enum Serializable {
	Type(Type),
	Instruction(Instruction),
	Sep,
	Void,
	StringLit,
	FnHeader,
	ExternFnHeader,
	Placeholder,
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
		S::Instruction(I::Push(_)) => 0x30,
		S::Instruction(I::ExternFnCall(_)) => 0x31,
		S::StringLit => 0x32,
		S::FnHeader => 0x33,
		S::ExternFnHeader => 0x34,
		S::Instruction(I::Return) => 0x35,
		S::Instruction(I::FnCall(_)) => 0x36,
		S::Instruction(I::Pop) => 0x37,
		S::Instruction(I::Equals) => 0x38,
		S::Instruction(I::JumpZero(_)) => 0x39,
		S::Instruction(I::Dup(_)) => 0x3a,
		S::Instruction(I::Panic) => 0x3b,
		S::Instruction(I::Add) => 0x3c,
		S::Instruction(I::Sub) => 0x3d,
		S::Instruction(I::Swap(_)) => 0x3e,
		// This should never be actually kept in the end
		S::Placeholder => 0x50,
		// Should never be serialized. TODO: type this better?
		S::Instruction(I::LabelMark(_)) => panic!("tried to serialize unresolved label"),
	};
	typier as u8
}

type Label = usize;
type Location = usize;
#[derive(Default)]
struct Labels {
	marks: std::collections::HashMap<Label, Location>,
	refs: Vec<(Label, Location)>,
}
#[derive(Default)]
struct LabeledCode {
	code: Vec<u8>,
	labels: Labels,
}

fn append_labeled(base: &mut LabeledCode, mut new: LabeledCode) {
	for (_, location) in &mut new.labels.marks {
		*location += base.code.len();
	}
	for (_, location) in &mut new.labels.refs {
		*location += base.code.len();
	}
	base.code.append(&mut new.code);
	base.labels.refs.append(&mut new.labels.refs);
	base.labels.marks.extend(new.labels.marks);
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

fn gen_fn_body(function: &Fn) -> LabeledCode {
	let mut code = Vec::new();
	let mut labels = Labels::default();
	for instr in &function.instructions {
		// Label marks aren't serialized at all
		// Not sure how to do NOT so....
		if let LabelMark(_) = instr {} else {
			code.push(serialize(Serializable::Instruction(*instr)));
		}
		use Instruction::*;
		match instr {
			Push(what) => {
				code.extend_from_slice(&u32_bytes(*what as u32));
			},
			// Besides instruction, the procedure for generating
			// FnCall and ExternFnCall is the same
			FnCall(call) | ExternFnCall(call) => {
				code.extend_from_slice(&u32_bytes(call.index as u32));
			},
			// u8 argument
			Dup(what) | Swap(what) => code.push(*what),
			// is label
			LabelMark(label) => {
				if DEBUG { println!("label marked, \"{}\" refers to {}", label, code.len()) }
				labels.marks.insert(*label, code.len());
			}
			// label argument
			JumpZero(label) => {
				if DEBUG { println!("label referred, at {} references \"{}\"", code.len(), label) }
				labels.refs.push((*label, code.len()));
				code.push(serialize(Serializable::Placeholder));
			}
			// As simple as serializing the instruction
			| Return
			| Pop
			| Equals
			| Panic
			| Add
			| Sub
			=> {},
		}
	}
	LabeledCode {
		code,
		labels
	}
}

/// Should only be called ONCE, when everything has been generated
fn resolve_labels(labeled: &mut LabeledCode) {
	for (label, location) in &labeled.labels.refs {
		if DEBUG { println!("trying to look up {}", label) }
		let refers = labeled.labels.marks.get(&label).expect("referred to non-existent label");
		// Ensure that the ref location is in fact a placeholder byte
		assert_eq!(labeled.code[*location],
		          serialize(Serializable::Placeholder), "tried to write label to non-placeholder");
		// TODO: non-relative labels as well
		// Why do we add 1? because we've already popped location, so relative to the NEXT inst
		labeled.code[*location] = i8_as_u8((*refers as isize - (*location + 1) as isize) as i8);
	}
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
	let mut labeled = LabeledCode::default();
	labeled.code = b"bcfg".to_vec();
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
	let mut code_loc = labeled.code.len();
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
		labeled.code.push(serialize(Serializable::FnHeader));
		header.append(&mut u32_bytes(code_loc as u32).to_vec());
		labeled.code.append(&mut header);
		code_loc += body.code.len();
	}
	// Add the extern fn headers with no bodies
	for mut header in extern_fn_headers.iter_mut() {
		labeled.code.push(serialize(Serializable::ExternFnHeader));
		labeled.code.append(&mut header);
	}
	for string in tree.strings {
		labeled.code.push(serialize(Serializable::StringLit));
		labeled.code.append(&mut string.into_bytes());
		labeled.code.push(serialize(Serializable::Sep));
	}
	// Actually add the bodies, after *all* the headers
	for body in fn_bodies {
		append_labeled(&mut labeled, body);
	}
	resolve_labels(&mut labeled);
	labeled.code.to_vec()
}

#[cfg(test)]
mod test {
	#[test]
	fn resolves_labels() {
		use super::{resolve_labels, LabeledCode, Labels, serialize, Serializable};
		use std::collections::HashMap;
		// Initial:
		// ref byte 0: Placeholder
		// mark byte 1: 'm'
		// Expected:
		// ref byte 0: 0
		// mark byte 1: 'm'
		let mut marks = HashMap::new();
		let label = 2;
		marks.insert(label, 1);
		let mut labeled = LabeledCode {
			code: vec![serialize(Serializable::Placeholder), 'm' as u8],
			labels: Labels {
				marks,
				refs: vec![(label, 0)],
			},
		};
		let expected = vec![0, 'm' as u8];
		resolve_labels(&mut labeled);
		assert_eq!(labeled.code, expected);
	}
}

