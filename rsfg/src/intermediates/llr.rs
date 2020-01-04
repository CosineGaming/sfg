//! Low-level representation, the [LLR] (root node) is practically bytecode.
//!
//! the only real difference is we're using enums and labels. You'd be surprised
//! how much easier that makes it both to create and translate to bytecode.
//! Because llr is practically bytecode, it's composed exclusively of
//! quasi-bytecode [Instruction].

use crate::{fmt_vec, fmt_vec_with, Type};

/// Root node, this is what you're getting out of lower and passing around
#[derive(PartialEq, Eq, Debug)]
pub struct LLR {
    pub fns: Vec<Fn>,
    pub extern_fns: Vec<Signature>,
    /// String literals need to be stored somehow right
    pub strings: Vec<String>,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Fn {
    pub instructions: Vec<Instruction>,
    pub signature: Signature,
}
#[derive(PartialEq, Eq, Clone, Debug, Default)]
pub struct Signature {
    pub parameters: Vec<TypedVar>,
    /// Can be void (None)
    pub return_type: Option<Type>,
    /// All fns are public and may need to interact with ABI
    // (CHECK: might wanna change that)
    pub name: String,
}
/// NameKey is actually just an index into the fns / extern_fns vectors,
/// but it's 16-bit because that's how it's serialized
pub type NameKey = u16;
/// An instruction is one bytecode instruction, except it includes its
/// subsequent *in-code* operands. in-code means it follows in the bytecode
/// (ie in a literal push, or a jump instruction), not in data like the stack.
/// When serialized, it'll pretty much exactly match the data structure:  
/// Instruction opcode, tuple data as bytes, rinse and repeat.
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
    DeclLit(u32),      // opt
    StoreLit(u8, u32), // opt
    Load(u8),
}
/// i've been really indecisive about whether parameters should have names
/// attached, so i made a type to enable my own flip-flopping
pub type TypedVar = Type;
/// Labels are one of the more special parts of the LLR. a jump instruction
/// needs to know where it's going, in bytes. but we are still working abstractly,
/// we have no idea what the generated code will look like! so our solution is
/// to generate a label (using [lower](crate::passes::lower::lower) utilities) and then add
/// two kinds of instructions: Jump*(Label), and LabelMark(Label). LabelMark
/// describes a place in code. A jump will be generated to refer to where
/// the labelmark ends up when generated
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
            fmt_vec_with(&self.strings, "", ",")
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
