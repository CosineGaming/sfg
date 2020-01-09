//! The actual passes that compile / transform from one data structure
//! to the next.
//!
//! The compilation process goes in this order:
//!
//! 1. [lex]
//! 2. [parse]
//! 3. [lower](lower::lower)
//! 4. [optimize_llr]
//! 5. [gen]
//!
//! Also bundled here (because there's not a great way to put them elsewhere)
//! are the error types for parse + lower. lex panics on fail (TODO),
//! optimize never fails, and gen panics on fail (TODO)

mod codegen;
mod lexer;
mod lower;
mod optimizer;
mod parser;

pub use codegen::gen;
pub use lexer::lex;
pub use lower::lower;
pub use lower::LowerError;
pub use optimizer::optimize_llr;
pub use parser::parse;
pub use parser::ParseError;
