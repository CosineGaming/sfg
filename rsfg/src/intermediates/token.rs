//! A list of Tokens is the result of lexing, just chuncked program data
//! with no regard for syntax. You're probably looking for [Token], not
//! TokenType, since you want that sweet location (span) data

use crate::{span::Span, Type};

/// The kind of token (If, IntLit, Comma, etc), and any additional data
/// associated (for example IntLit: the value, as an i32)
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

/// All tokens have a location ([Token::span]) and a type ([Token::kind]),
/// but only some kinds of tokens have additional data (literal, type, string,
/// etc) so THAT is encoded in the TokenType enum
#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.span)
    }
}
