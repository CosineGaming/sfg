use crate::sfg_std;

use indexmap::IndexMap;

/// Should be small enough to make small scripts low-RAM, but high enough
/// that startup doesn't take forever with 1000s of incremental allocs
const INIT_STACK_SIZE: usize = 50;
/// Similarly chosen for the expected call stack size
const INIT_CALL_STACK_SIZE: usize = 6;
/// Prints the stack and each instruction
const DEBUG: bool = false;

#[derive(PartialEq, Debug)]
pub struct Thread {
	pub stack: Vec<u32>,
	/// The call stack is managed by the VM, containing calls only
	/// It's kept separate as opposed to machine architectures because
	/// the use of the data stack is made harder by combining them.
	/// Each usize is a ip
	pub call_stack: Vec<usize>,
	code: Vec<u8>,
	// Code pointer
	ip: usize,
	pub strings: Vec<String>,
	fns: IndexMap<String, Fn>,
}

#[derive(PartialEq, Clone, Debug)]
struct Fn {
	// ip will be 0 for externs (TODO: make this safer)
	ip: u32,
	return_type: Option<Type>,
	parameters: Vec<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Type {
	Str,
	Int,
}

#[derive(PartialEq, Eq, Debug)]
enum Deser {
	Dup,
	Equals,
	ExternFnCall,
	ExternFnHeader,
	FnCall,
	FnHeader,
	Panic,
	JumpZero,
	Pop32,
	Push32,
	Return,
	StringLit,
	Type(Type),
	Void,
}

fn deser(what: u8) -> Option<Deser> {
	use Deser as D;
	use Type::*;
	match what {
		// Types 1x
		0x10 => Some(D::Type(Int)),
		0x11 => Some(D::Type(Str)),
		// Other 2x
		0x21 => Some(D::Void),
		// Instructions 3x
		0x30 => Some(D::Push32),
		0x31 => Some(D::ExternFnCall),
		0x32 => Some(D::StringLit),
		0x33 => Some(D::FnHeader),
		0x34 => Some(D::ExternFnHeader),
		0x35 => Some(D::Return),
		0x36 => Some(D::FnCall),
		0x37 => Some(D::Pop32),
		0x38 => Some(D::Equals),
		0x39 => Some(D::JumpZero),
		0x3a => Some(D::Dup),
		0x3b => Some(D::Panic),
		_ => None,
	}
}
fn deser_strong(what: u8) -> Deser {
	deser(what).expect(&format!("tried to match invalid u8 0x{:X}", what))
}

fn expect(code: &Vec<u8>, mut ip: &mut usize, what: u8, message: &str) {
	if next(&code, &mut ip) != what {
		panic!("{}", message);
	}
}

fn next(code: &Vec<u8>, ip: &mut usize) -> u8 {
	let rv = code[*ip];
	*ip += 1;
	rv
}

fn read_u32(code: &Vec<u8>, ip: &mut usize) -> u32 {
	use std::mem::transmute;
	let mut four: [u8; 4] = Default::default();
	four.copy_from_slice(&code[*ip..*ip+4]);
	let rv = u32::from_le(unsafe { transmute::<[u8; 4], u32>(four) });
	*ip += 4;
	rv
}

fn from_u32(what: u32) -> [u8; 4] {
	use std::mem::transmute;
	let le = what.to_le();
	unsafe { transmute(le) }
}

fn read_to_zero(code: &Vec<u8>, mut ip: &mut usize) -> Vec<u8> {
	let mut rv = Vec::new();
	loop {
		let b = next(code, &mut ip);
		if b == 0 {
			break;
		}
		rv.push(b);
	}
	rv
}

fn read_string(code: &Vec<u8>, mut ip: &mut usize) -> String {
	let bytes = read_to_zero(&code, &mut ip);
	match String::from_utf8(bytes) {
		Ok(string) => string,
		Err(e) => panic!("invalid string {}", e),
	}
}

/// Returns (name, function)
fn read_fn_header(code: &Vec<u8>, mut ip: &mut usize, is_extern: bool) -> (String, Fn) {
	let return_type_u8 = next(code, &mut ip);
	let return_type = match deser_strong(return_type_u8) {
		Deser::Type(t) => Some(t),
		Deser::Void => None,
		_ => panic!("expected type or void, got {}", return_type_u8),
	};
	let param_count = next(code, &mut ip);
	let mut parameters = vec![];
	for _ in 0..param_count {
		let type_u8 = next(code, &mut ip);
		let param = match deser_strong(type_u8) {
			Deser::Type(t) => t,
			_ => panic!("expected type, got {}", type_u8),
		};
		parameters.push(param);
	}
	let name = read_string(&code, &mut ip);
	let codeloc = if is_extern {
		0
	} else {
		read_u32(code, &mut ip)
	};
	let func = Fn {
		return_type,
		parameters,
		ip: codeloc,
	};
	(name, func)
}

// This function may be a little redundant, but I wanna keep it for a bit
// in case StringLit gets more complex
fn read_string_lit(code: &Vec<u8>, mut ip: &mut usize) -> String {
	read_string(&code, &mut ip)
}

impl Thread {
	pub fn new(code: Vec<u8>) -> Self {
		let mut ip = 0;
		expect(&code, &mut ip, 'b' as u8, "expected bcfg");
		expect(&code, &mut ip, 'c' as u8, "expected bcfg");
		expect(&code, &mut ip, 'f' as u8, "expected bcfg");
		expect(&code, &mut ip, 'g' as u8, "expected bcfg");
		let mut fns = IndexMap::new();
		let mut strings = Vec::new();
		loop {
			match deser(code[ip]) {
				Some(Deser::FnHeader) => {
					ip += 1;
					let (name, func) = read_fn_header(&code, &mut ip, false);
					fns.insert(name, func);
				},
				_ => break,
			}
		}
		loop {
			match deser(code[ip]) {
				Some(Deser::ExternFnHeader) => {
					ip += 1;
					let (name, func) = read_fn_header(&code, &mut ip, true);
					fns.insert(name, func);
				},
				_ => break,
			}
		}
		loop {
			match deser(code[ip]) {
				Some(Deser::StringLit) => {
					ip += 1;
					let string = read_string_lit(&code, &mut ip);
					strings.push(string);
				},
				_ => break,
			}
		}
		Self {
			stack: Vec::with_capacity(INIT_STACK_SIZE),
			call_stack: Vec::with_capacity(INIT_CALL_STACK_SIZE),
			code,
			ip,
			strings,
			fns,
		}
	}
	fn exec_next(&mut self) {
		match deser_strong(next(&self.code, &mut self.ip)) {
			Deser::Push32 => {
				self.stack.push(read_u32(&self.code, &mut self.ip));
			},
			Deser::Pop32 => {
				self.stack.pop();
			},
			Deser::ExternFnCall => {
				let index = read_u32(&self.code, &mut self.ip);
				let (name, func) = match self.fns.get_index(index as usize) {
					Some(tuple) => tuple,
					_ => panic!("could not find extern function at {}", index),
				};
				assert_eq!(func.ip, 0, "extern fn call calling non-extern function");
				match &name[..] {
					"log" => sfg_std::log(self),
					_ => panic!("special reflection business not yet supported and stdlib not found"),
				};
			},
			Deser::FnCall => {
				let index = read_u32(&self.code, &mut self.ip);
				let func = match self.fns.get_index(index as usize) {
					Some((_name, func)) => func.clone(),
					_ => panic!("could not find function at {}", index),
				};
				// We do we push ip here and not in
				// call_fn? because call_fn by user should not
				// push to stack, it should allow exit
				self.call_stack.push(self.ip);
				self.call_fn(func);
			},
			Deser::Equals => {
				let a = self.stack.pop();
				let b = self.stack.pop();
				self.stack.push((a == b) as u32);
			},
			Deser::JumpZero => {
				// TODO: i8 / jump backwards
				let amount = next(&self.code, &mut self.ip);
				let test = self.stack.pop().unwrap();
				if test == 0 {
					self.ip += amount as usize;
				}
			}
			Deser::Dup => {
				let count = next(&self.code, &mut self.ip) as usize;
				// -1 because 0 means last but len() means last+1
				let stack_elem = self.stack.get(self.stack.len()-count-1).unwrap();
				self.stack.push(*stack_elem);
			}
			Deser::Panic => {
				let col = self.stack.pop().unwrap();
				let line = self.stack.pop().unwrap();
				panic!("sfg code panicked at line {}:{}", line, col);
			}
			// TODO: Split deser into categories
			_ => panic!("expected instruction, got unsupported {:x}", self.code[self.ip-1]),
		}
	}
	pub fn push_string(&mut self, string: &String) {
		let as_number = string as *const String as u32;
		self.stack.push(as_number);
	}
	fn call_fn(&mut self, func: Fn) {
		assert_ne!(func.ip, 0, "tried to call extern function");
		self.ip = func.ip as usize;
		loop {
			if DEBUG {
				println!("stack {:x?}| next {:x?}", self.stack, deser_strong(self.code[self.ip]));
			}
			if deser_strong(self.code[self.ip]) == Deser::Return {
				self.ip = match self.call_stack.pop() {
					Some(ip) => ip,
					None => break,
				};
				break;
			}
			self.exec_next();
		}
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

/// call![thread.main()]
#[macro_export]
macro_rules! call {
	( $thread:ident.$function:ident($( $push:expr )*) ) => {
		{
			$(
				$push.push_to(&mut $thread);
			)*
			$thread.call_name(stringify!($function));
		}
	};
}

/// This trait serves only to make the macro work easily
/// relevant push_[type]()s and call_name() are just as effective
pub trait Pushable {
	fn push_to(&self, thread: &mut Thread);
}
impl Pushable for String {
	fn push_to(&self, thread: &mut Thread) {
		thread.push_string(self);
	}
}
impl Pushable for str {
	fn push_to(&self, thread: &mut Thread) {
		thread.push_string(&self.to_string());
	}
}

