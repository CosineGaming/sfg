//! i choose to expose the compiler as a library so you can run
//! one or two passes over the code, get the intermediate data structure
//! out of it, and do whatever the hell
//! you want with it. if all you want is to [compile], you can do that (or
//! you could just call on the CLI). Otherwise, we're getting a little nitty
//! gritty but i believe you can do great things
//!
//! Most of the code is split into [passes] and intermediate data structures.
//! Here's how it flows (data on the left, passes on the right):
//!
//! 1. source code &[str] => [passes::lex] =>
//! 2. [token::Token] list => [passes::parse] =>
//! 3. [ast] => [passes::lower](passes::lower::lower) =>
//! 4. [llr] (Low-Level Representation) => [passes::optimize_llr] =>
//! 5. [llr] again, but faster :P => [passes::gen] =>
//! 6. bcfg bytecode [Vec]\<[u8]\> => YOU!
//!
//! Now you might put this in a file or you might give it directly to [rvmfg]

#[macro_use]
extern crate log;
/// There's no reason to keep a compiler light, so we've conveniently bundled
/// rvmfg for your use. This helps if you're trying to interpret code, or for
/// testing (internally)
pub use rvmfg;

pub mod intermediates;
pub mod passes;

mod span;
pub use span::Span;

/// You probably want to pass this to compile
///
/// The sfg standard library is composed of two parts:
/// 1. The standard library provided by the compiler
/// 2. The standard library provided by the VM
///
/// (1) is further broken down into (a) the standard library /written in/ sfg,
/// and (b) that with special bytecode emitted by the compiler (written in rust).
///
/// This is implementations of section (a) of (1) and all the externs of (2)
///
/// You can find more details at src/sfg/std.sfg, which this is include!d from
pub const STDLIB: &str = include_str!("sfg/std.sfg");

// for internal convenience; not public
use intermediates::*;
use passes::*;

/// For all your error handling needs.
/// Tells you if it was a parse error or a lower error (lower errors are
/// usually type and nuanced errors, parse are just basic wrong token here).
/// You can also get spans out of here which tell you where, and maybe a
/// few other things. For the most part it's most useful just to print! them
#[derive(Debug)]
pub enum CompileError {
    Parse(Vec<ParseError>),
    Lower(Vec<LowerError>),
}
impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use CompileError::*;
        match self {
            Parse(errs) => write!(f, "{}", fmt_vec_with(errs, "[ERROR] ", "\n")),
            Lower(errs) => write!(f, "{}", fmt_vec_with(errs, "[ERROR] ", "\n")),
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for CompileError {}
type Result<T> = std::result::Result<T, CompileError>;

fn fmt_vec<T: std::fmt::Display>(vec: &[T]) -> String {
    fmt_vec_with(vec, "", "\n")
}
fn fmt_vec_with<T: std::fmt::Display>(vec: &[T], with: &str, sep: &str) -> String {
    vec.iter().map(|e| format!("{}{}", with, e)).collect::<Vec<String>>().join(sep)
}

fn vec_errs_to_res<T, E>(
    vec: Vec<std::result::Result<T, Vec<E>>>,
) -> std::result::Result<Vec<T>, Vec<E>> {
    let mut oks = vec![];
    let mut errs = vec![];
    for mut entry in vec {
        match entry {
            Ok(o) => oks.push(o),
            Err(ref mut e) => errs.append(e),
        }
    }
    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(oks)
    }
}

/// An sfg data-type that the VM understands
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Type {
    Int,
    Bool,
    Str,
    Float,
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
            Float => write!(f, "float"),
        }
    }
}

// TODO: add an actual import system so that we don't use this
// "#include-but-worse" hack for the stdlib
/// Compile a string, with the given stdlib appended to it. Recommend using
/// [STDLIB] for that part. Returns generated `bcfg` bytecode. If
/// you're not sure what to do with that, you should probly check out
/// [rvmfg].
pub fn compile(text: &str, stdlib: &str) -> Result<Vec<u8>> {
    let full_text = format!("{}\n{}", text, stdlib);
    let result = parse(lex(&full_text));
    let ast = match result {
        Ok(ast) => ast,
        Err(err) => return Err(CompileError::Parse(err)),
    };
    let result = lower(ast);
    let llr = match result {
        Ok(llr) => llr,
        Err(err) => return Err(CompileError::Lower(err)),
    };
    Ok(gen(llr))
}
