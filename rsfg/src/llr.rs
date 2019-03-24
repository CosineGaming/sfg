// Low-level representation, this is the final goal form before converting
// to bytecode

// Note that this representation is meant to mirror BYTECODE not SYNTAX. as
// such, until it can be generated, it doesn't belong in this representation
// (so some AST constructs are simply lost in lower())

// We're racing to hello world so there's a lot commented out

use super::Type;

pub struct LLR {
	pub fns: Vec<Fn>,
	pub extern_fns: Vec<Signature>,
	pub strings: Vec<String>,
}
#[derive(PartialEq, Eq, Debug)]
pub struct Fn {
	pub instructions: Vec<Instruction>,
	pub signature: Signature,
	//pub namespace: Namespace,// my idea is to use the namespace as where variables are placed
}
#[derive(PartialEq, Eq, Debug)]
pub struct Signature {
	pub parameters: Vec<TypedVar>, // When namespace is added, this should be Vec<NameKey> and point to the namespace
	pub return_type: Option<Type>, // Can be void (None)
	pub name: String, // All fns are public and may need to interact with ABI
}
pub type NameKey = usize;
//pub type Namespace = Vec<TypedVar>;
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Instruction {
	//pub Assignment(Assignment),
	FnCall(FnCall),
	ExternFnCall(FnCall),
	Push32(u32),
	Pop32,
	Return,
	Equals,
	JumpZero(u8),
}
pub type TypedVar = Type;
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct FnCall {
	pub index: NameKey,
	pub arg_count: u8,
}

impl LLR {
	pub fn new() -> Self {
		Self {
			fns: vec![],
			extern_fns: vec![],
			strings: vec![],
		}
	}
}

