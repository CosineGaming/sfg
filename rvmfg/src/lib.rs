mod sfg_std;

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
	strings: Vec<String>,
	fns: IndexMap<String, Fn>,
}

#[derive(PartialEq, Clone, Debug)]
struct Fn {
	// cp will be 0 for externs (TODO: make this safer)
	cp: u32,
	stack_size: u8,
	return_type: Option<Type>,
	parameters: Vec<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Type {
	Str,
}

#[derive(PartialEq, Eq, Debug)]
enum Deser {
	Type(Type),
	Void,
	FnHeader,
	ExternFnHeader,
	StringLit,
	Return,
	PushStringLit,
	ExternFnCall,
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
		0x30 => Some(D::PushStringLit),
		0x31 => Some(D::ExternFnCall),
		0x32 => Some(D::StringLit),
		0x33 => Some(D::FnHeader),
		0x34 => Some(D::ExternFnHeader),
		0x35 => Some(D::Return),
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

fn read_to_zero(code: &Vec<u8>, mut cp: &mut usize) -> Vec<u8> {
	let mut rv = Vec::new();
	loop {
		let b = next(code, &mut cp);
		if b == 0 {
			break;
		}
		rv.push(b);
	}
	rv
}

fn read_string(code: &Vec<u8>, mut cp: &mut usize) -> String {
	let bytes = read_to_zero(&code, &mut cp);
	match String::from_utf8(bytes) {
		Ok(string) => string,
		Err(e) => panic!("invalid string {}", e),
	}
}

/// Returns (name, function)
fn read_fn_header(code: &Vec<u8>, mut cp: &mut usize, is_extern: bool) -> (String, Fn) {
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
	let name = read_string(&code, &mut cp);
	let codeloc = if is_extern {
		0
	} else {
		read_u32(code, &mut cp)
	};
	let func = Fn {
		stack_size,
		return_type,
		parameters,
		cp: codeloc,
	};
	(name, func)
}

// This function may be a little redundant, but I wanna keep it for a bit
// in case StringLit gets more complex
fn read_string_lit(code: &Vec<u8>, mut cp: &mut usize) -> String {
	read_string(&code, &mut cp)
}

impl Thread {
	pub fn new(code: Vec<u8>) -> Self {
		let mut cp = 0;
		expect(&code, &mut cp, 'b' as u8, "expected bcfg");
		expect(&code, &mut cp, 'c' as u8, "expected bcfg");
		expect(&code, &mut cp, 'f' as u8, "expected bcfg");
		expect(&code, &mut cp, 'g' as u8, "expected bcfg");
		let mut fns = IndexMap::new();
		let mut strings = Vec::new();
		loop {
			println!("0x{:X}", code[cp]);
			match deser(code[cp]) {
				Some(Deser::FnHeader) => {
					cp += 1;
					let (name, func) = read_fn_header(&code, &mut cp, false);
					fns.insert(name, func);
				},
				_ => break,
			}
			match deser(code[cp]) {
				Some(Deser::ExternFnHeader) => {
					cp += 1;
					let (name, func) = read_fn_header(&code, &mut cp, true);
					fns.insert(name, func);
				},
				_ => break,
			}
			match deser(code[cp]) {
				Some(Deser::StringLit) => {
					cp += 1;
					let string = read_string_lit(&code, &mut cp);
					strings.push(string);
				},
				_ => break,
			}
		}
		Self {
			stack: Vec::with_capacity(INIT_STACK_SIZE),
			sp: 0,
			code,
			cp,
			strings,
			fns,
		}
	}
	fn exec_next(&mut self) {
		match deser_strong(next(&self.code, &mut self.cp)) {
			Deser::PushStringLit => {
				println!("PushStringLit not implemented");
				// Next the string location
				next(&self.code, &mut self.cp);
			},
			Deser::ExternFnCall => {
				println!("ExternFnCall not yet implemented");
				let index = read_u32(&self.code, &mut self.cp);
				let name = match self.fns.get_index(index as usize) {
					Some((name, _func)) => name,
					_ => panic!("could not find extern function {}", index),
				};
				// TODO: assert function is cp=0 (extern)
				match &name[..] {
					"log" => sfg_std::log("arguments not yet supported TODO"),
					_ => panic!("special reflection business not yet supported"),
				}
			},
			// This would be the code for a proper function call
			//Deser::FnCall => {
				//let index = next(&self.code, &mut self.cp);
				//self.call_index(index);
			//},
			// TODO: Split deser into categories
			_ => panic!("expected instruction, got unsupported {:x}", self.code[self.cp-1]),
		}
	}
	fn call_fn(&mut self, func: Fn) {
		assert_ne!(func.cp, 0, "tried to call extern function");
		self.cp = func.cp as usize;
		loop {
			if deser_strong(self.code[self.cp]) == Deser::Return {
				// TODO: Push some return value, decrement stack, etc
				break;
			}
			self.exec_next();
		}
	}
	fn call_index(&mut self, index: u8) {
		let func = match self.fns.get_index(index as usize) {
			Some((_name, func)) => func.clone(),
			_ => panic!("could not find function at {}", index),
		};
		self.call_fn(func);
	}
	/// This ONLY calls the function, does NOT push to stack
	/// use the c! macro to perform a call. It's only public because
	/// it has to be
	#[doc(hidden)]
	pub fn call_name(&mut self, name: &str) {
		let func = match self.fns.get(name) {
			Some(func) => func.clone(),
			None => panic!("could not find function {}", name),
		};
		self.call_fn(func);
	}
}

#[cfg(test)]
mod tests {
	use super::{Thread, Fn, Type};
	use indexmap::indexmap;
	fn load_file(filename: &str) -> Thread {
		let code = std::fs::read(filename)
			.expect("could not load given file");
		Thread::new(code)
	}
	#[test]
	fn hello_world_init() {
		let thread = load_file("tests/binaries/hello-world.bcfg");
		assert_eq!(thread.fns,
			indexmap!{
				"main".to_string() => Fn {
					cp: 30,
					stack_size: 0,
					return_type: None,
					parameters: vec![],
				},
				"log".to_string() => Fn {
					cp: 0,
					stack_size: 8,
					return_type: None,
					parameters: vec![Type::Str],
				},
			}
		);
		assert_eq!(thread.stack, vec![]);
		assert_eq!(thread.sp, 0);
		// cp doesn't matter
		// code is hard but it's immutable so prob fine
	}
}
