// all roads lead to lib.rs

#[macro_use]
extern crate log;

mod ast;
mod codegen;
mod lexer;
mod llr;
mod lower;
mod parser;

#[derive(Debug)]
pub enum CompileError {
    Parse(Vec<parser::ParseError>),
    Lower(Vec<lower::LowerError>),
}
impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use CompileError::*;
        match self {
            Parse(errs) => write!(f, "{}", fmt_vec_with(errs, "[ERROR] ")),
            Lower(errs) => write!(f, "{}", fmt_vec_with(errs, "[ERROR] ")),
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for CompileError {}
type Result<T> = std::result::Result<T, CompileError>;

pub fn fmt_vec<T: std::fmt::Display>(vec: &[T]) -> String {
    fmt_vec_with(vec, "")
}
pub fn fmt_vec_with<T: std::fmt::Display>(vec: &[T], with: &str) -> String {
    vec.iter().map(|e| format!("{}{}", with, e)).collect::<Vec<String>>().join("\n")
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

#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
    Identifier(String),
    Tab,
    StringLit(String),
    IntLit(i32),
    FloatLit(f32),
    Type(Type),
    Fn,
    ExternFn,
    ExternFnCall(String),
    Return,
    Comma,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Not,
    NotEqual,
    Or,
    And,
    Colon,
    Newline,
    LParen,
    RParen,
    If,
    Else,
    While,
    Declare,
    Assignment,
    Plus,
    Minus,
    Times,
    Mod,
    Divide,
    OpAssign(Box<TokenType>),
    True,
    False,
}
// TODO: is there a way to mix this with the lexer in healthier/DRYer way?
impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenType::*;
        let s = match self {
            Identifier(_) => "identifier",
            Tab => "tab",
            StringLit(_) => "string literal",
            IntLit(_) => "int literal",
            FloatLit(_) => "float literal",
            Type(_) => "type",
            Fn => "fn",
            ExternFn => "extern fn",
            ExternFnCall(_) => "extern fn call",
            Return => "return",
            Comma => ",",
            Equal => "==",
            Less => "<",
            LessEqual => "<=",
            Greater => ">",
            GreaterEqual => ">=",
            Not => "!",
            NotEqual => "!=",
            Or => "||",
            And => "&&",
            Colon => ":",
            Newline => "newline",
            LParen => "(",
            RParen => ")",
            If => "if",
            Else => "else",
            While => "while",
            Declare => "var",
            Assignment => "=",
            Plus => "+",
            Minus => "-",
            Times => "*",
            Divide => "/",
            Mod => "%",
            OpAssign(of) => match **of {
                Plus => "+=",
                Minus => "-=",
                Times => "*=",
                Divide => "/=",
                // TODO: this is an ugly convention for "any"
                False => "assignment",
                _ => panic!("unsupported opassign stringified"),
            },
            True => "true",
            False => "false",
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    kind: TokenType,
    span: Span,
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.span)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Type {
    Int,
    Bool,
    Str,
    Float,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Default)]
pub struct Span {
    lo: (usize, usize),
    hi: (usize, usize),
    // TODO: file
}
impl Span {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn set(mut spans: Vec<Span>) -> Span {
        let first = spans.pop().expect("cannot form set of less than one span");
        let mut lo = first.lo;
        let mut hi = first.hi;
        for span in spans {
            // if lower, go lower
            if span.lo.0 < lo.0 || (span.lo.0 == lo.0 && span.lo.1 < lo.1) {
                lo = span.lo;
            }
            // if higher go higher
            if span.hi.0 > hi.0 || (span.hi.0 == hi.0 && span.hi.1 > hi.1) {
                hi = span.hi;
            }
        }
        Span { lo, hi }
    }
}
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if *self == Span::new() {
            write!(f, "internal")
        } else {
            write!(f, "{}:{}", self.lo.0, self.lo.1)
        }
    }
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
pub fn compile(text: &str, stdlib: &str) -> Result<Vec<u8>> {
    let full_text = format!("{}\n{}", text, stdlib);
    let result = parser::parse(lexer::lex(&full_text));
    let ast = match result {
        Ok(ast) => ast,
        Err(err) => return Err(CompileError::Parse(err)),
    };
    let result = lower::lower(ast);
    let llr = match result {
        Ok(llr) => llr,
        Err(err) => return Err(CompileError::Lower(err)),
    };
    Ok(codegen::gen(llr))
}

pub fn compile_or_print(text: &str, stdlib: &str) -> Vec<u8> {
    match compile(text, stdlib) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod test {
    use super::Span;
    #[test]
    fn test_span_set() {
        let set =
            Span::set(vec![Span { lo: (4, 4), hi: (5, 5) }, Span { lo: (4, 2), hi: (4, 10) }]);
        assert_eq!(set, Span { lo: (4, 2), hi: (5, 5) });
    }
}
