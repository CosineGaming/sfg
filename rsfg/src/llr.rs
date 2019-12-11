// Low-level representation, this is the final goal form before converting
// to bytecode

// Note that this representation is meant to mirror BYTECODE not SYNTAX. as
// such, until it can be generated, it doesn't belong in this representation
// (so some AST constructs are simply lost in lower())

// We're racing to hello world so there's a lot commented out

use crate::{fmt_vec, Type};

#[derive(PartialEq, Eq, Debug)]
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
    pub name: String,              // All fns are public and may need to interact with ABI
}
pub type NameKey = u16;
//pub type Namespace = Vec<TypedVar>;
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Instruction {
    FnCall(NameKey),
    ExternFnCall(NameKey),
    Push(u32),
    Dup,
    Pop,
    Return,
    Not,
    Less,
    JumpZero(Label),
    Jump(Label),
    //VarAlloc(u8),// this is a perf improvement to add later TODO
    // It pre-allocates for all the locals that are to be added in the procedure call
    DeVars(u8),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    FAdd,
    FSub,
    FLess,
    FMul,
    FDiv,
    Panic(u32, u32),
    LabelMark(Label),
    Xor,
    Decl,
    Store(u8),
    DeclLit(u32), // opt
    StoreLit(u8, u32), // opt
    Load(u8),
}
pub type TypedVar = Type;
pub type Label = usize;

impl LLR {
    pub fn new() -> Self {
        Self { fns: vec![], extern_fns: vec![], strings: vec![] }
    }
}
impl std::fmt::Display for LLR {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO find a way to display where in the code these are
        write!(
            f,
            "\nFNS:\n{}\nEXTERNS:\n{}\nSTRINGS:\n{:?}",
            fmt_vec(&self.fns),
            fmt_vec(&self.extern_fns),
            fmt_vec(&self.strings)
        )
    }
}
impl std::fmt::Display for Fn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.signature)?;
        for inst in &self.instructions {
            // eh don't bother with display the names are perfect
            writeln!(f, "{:?}", inst)?;
        }
        Ok(())
    }
}
impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for param in &self.parameters {
            write!(f, "{}, ", param)?;
        }
        write!(f, ") {:?}", self.return_type)
    }
}
