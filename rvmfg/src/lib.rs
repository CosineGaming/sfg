use indexmap::IndexMap;

// Should be small enough to make small scripts low-RAM, but high enough
// that startup doesn't take forever with 1000s of incremental allocs
const INIT_STACK_SIZE: usize = 50;

#[derive(PartialEq, Debug)]
pub struct Thread {
	stack: Vec<u8>,
	// Stack pointer
	sp: usize,
	code: Vec<u8>,
	// Code pointer
	cp: usize,
	fns: IndexMap<Vec<u8>, Fn>,
}

#[derive(PartialEq, Debug)]
struct Fn {
	cp: u32,
	stack_size: u8,
	return_type: Option<Type>,
	parameters: Vec<Type>,
}

#[derive(PartialEq, Debug)]
enum Type {
	Str,
}

enum Deser {
	Type(Type),
	Void,
	FnHeader,
}

fn deser(what: u8) -> Option<Deser> {
	use Deser as D;
	use Type::*;
	match what {
		// Types 1x
		//0x10 => D::Type(Int),
		0x11 => Some(D::Type(Str)),
		// Other 2x
		0x21 => Some(D::Void),
		// Instructions 3x
		//0x30 => D::Instruction(I::PushStringLit(_)),
		//0x31 => D::Instruction(I::ExternFnCall(_)),
		0x33 => Some(D::FnHeader),
		_ => None,
	}
}
fn deser_strong(what: u8) -> Deser {
	deser(what).expect(&format!("tried to match invalid u8 0x{:X}", what))
}

fn expect(code: &Vec<u8>, mut cp: &mut usize, what: u8, message: &str) {
	if next(&code, &mut cp) != what {
		panic!("{}", message);
	}
}

fn next(code: &Vec<u8>, cp: &mut usize) -> u8 {
	let rv = code[*cp];
	*cp += 1;
	rv
}

fn read_u32(code: &Vec<u8>, cp: &mut usize) -> u32 {
	use std::mem::transmute;
	let mut four: [u8; 4] = Default::default();
	four.copy_from_slice(&code[*cp..*cp+4]);
	let rv = u32::from_le(unsafe { transmute::<[u8; 4], u32>(four) });
	*cp += 4;
	rv
}

/// Returns (name, function)
fn read_fn_header(code: &Vec<u8>, mut cp: &mut usize) -> (Vec<u8>, Fn) {
	let stack_size = next(code, &mut cp);
	let return_type_u8 = next(code, &mut cp);
	let return_type = match deser_strong(return_type_u8) {
		Deser::Type(t) => Some(t),
		Deser::Void => None,
		_ => panic!("expected type or void, got {}", return_type_u8),
	};
	let param_count = next(code, &mut cp);
	let mut parameters = vec![];
	for _ in 0..param_count {
		let type_u8 = next(code, &mut cp);
		let param = match deser_strong(type_u8) {
			Deser::Type(t) => t,
			_ => panic!("expected type, got {}", type_u8),
		};
		parameters.push(param);
	}
	let mut name = vec![];
	loop {
		let b = next(code, &mut cp);
		if b == 0 {
			break;
		}
		name.push(b);
	}
	let codeloc = read_u32(code, &mut cp);
	let func = Fn {
		stack_size,
		return_type,
		parameters,
		cp: codeloc,
	};
	(name, func)
}

impl Thread {
	pub fn new(code: Vec<u8>) -> Self {
		let mut cp = 0;
		expect(&code, &mut cp, 'b' as u8, "expected bcfg");
		expect(&code, &mut cp, 'c' as u8, "expected bcfg");
		expect(&code, &mut cp, 'f' as u8, "expected bcfg");
		expect(&code, &mut cp, 'g' as u8, "expected bcfg");
		let mut fns = IndexMap::new();
		loop {
			println!("0x{:X}", code[cp]);
			match deser(code[cp]) {
				Some(Deser::FnHeader) => {
					cp += 1;
					let (name, func) = read_fn_header(&code, &mut cp);
					fns.insert(name, func);
				},
				_ => break,
			}
		}
		Self {
			stack: Vec::with_capacity(INIT_STACK_SIZE),
			sp: 0,
			code,
			cp,
			fns,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::{Thread, Fn};
	use indexmap::indexmap;
	fn load_file(filename: &str) -> Thread {
		let code = std::fs::read(filename)
			.expect("could not load given file");
		Thread::new(code)
	}
	#[test]
	fn hello_world_fns() {
		let thread = load_file("tests/binaries/hello-world.bcfg");
		assert_eq!(thread.fns,
			indexmap!{
				b"main".to_vec() => Fn {
					cp: 26,
					stack_size: 0,
					return_type: None,
					parameters: vec![],
				},
			}
		);
	}
}

