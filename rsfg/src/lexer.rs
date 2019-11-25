// Hello, welcome to my lexer. Please like and subscribe

use crate::{Span, Token, TokenType};

// A-Z or 0-9
fn is_id(c: char) -> bool {
    match c {
        '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' => true,
        _ => false,
    }
}

enum NextTokenType {
    EOF,
    SymbolOrId(char),
    Space(char),
    Digit(char),
    ExternFnOrExternCall,
    CommentOrDivision,
    AssignmentOrEquals,
    NotOrEquals,
    LessOrEquals,
    GreaterOrEquals,
    Or,
    And,
    Newline,
    LParen,
    RParen,
    Colon,
    Comma,
    Quote,
    Plus,
    Minus,
    Times,
    Unknown(char),
}

#[derive(Debug)]
struct Lexer<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    spaces_count: usize,
    rchars: Vec<char>,
    line: usize,
    col: usize,
    col_begin: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source,
            tokens: Vec::new(),
            spaces_count: 0,
            rchars: source.chars().rev().collect(),
            line: 1,
            col: 1,
            col_begin: source.len(),
        }
    }

    fn reinterpret(&mut self, plain: TokenType, change: char, changed: TokenType) -> TokenType {
        match self.rchars.last() {
            Some(what) => {
                if *what == change {
                    self.rchars.pop();
                    changed
                } else {
                    plain
                }
            }
            _ => plain,
        }
    }

    fn next_symbol_type(&mut self) -> NextTokenType {
        use NextTokenType::*;

        match self.rchars.pop() {
            None => EOF,
            Some(c) => match c {
                'A'..='Z' | 'a'..='z' => SymbolOrId(c),
                '@' => ExternFnOrExternCall,
                '=' => AssignmentOrEquals,
                '!' => NotOrEquals,
                '>' => GreaterOrEquals,
                '<' => LessOrEquals,
                '|' => Or,
                '&' => And,
                ' ' | '\t' => Space(c),
                '0'..='9' => Digit(c),
                '/' => CommentOrDivision,
                '\n' => Newline,
                '(' => LParen,
                ')' => RParen,
                ':' => Colon,
                ',' => Comma,
                '"' => Quote,
                '+' => Plus,
                '-' => Minus,
                '*' => Times,
                o => Unknown(o),
            },
        }
    }

    fn update_col(&mut self) {
        // The number of characters removed is the number that were parsed
        self.col += self.col_begin - self.rchars.len();
        // Yes, this happens *after*. The *start* of a token (col) is the *end* of all previous
        self.col_begin = self.rchars.len();
    }
}

pub fn lex(text: &str) -> Vec<Token> {
    use TokenType::*;
    let mut lexer = Lexer::new(text);
    loop {
        lexer.update_col();
        // Set these now so Newline is on the right line
        let lo = (lexer.line, lexer.col);
        let token = match lexer.next_symbol_type() {
            NextTokenType::EOF => {
                // This is the end of the file, which is OK, as we are not in the middle
                // of matching a token
                break;
            }
            NextTokenType::SymbolOrId(c) => {
                let mut text = c.to_string();
                while let Some(x) = lexer.rchars.last() {
                    if is_id(*x) {
                        text.push(*x);
                    } else {
                        break;
                    }
                    lexer.rchars.pop();
                }
                match text.as_ref() {
                    "fn" => Fn,
                    // These names clash, it sucks
                    "int" => TokenType::Type(crate::Type::Int),
                    "str" => TokenType::Type(crate::Type::Str),
                    "bool" => TokenType::Type(crate::Type::Bool),
                    "float" => TokenType::Type(crate::Type::Float),
                    "true" => True,
                    "false" => False,
                    "return" => Return,
                    "if" => If,
                    "else" => Else,
                    "while" => While,
                    "var" => Declare,
                    _ => Identifier(text),
                }
            }
            NextTokenType::Space(c) => {
                match lexer.tokens.last() {
                    // Only count if we don't come after something
                    Some(&Token { kind: Newline, .. }) | Some(&Token { kind: Tab, .. }) => {
                        if c == '\t' {
                            Tab
                        } else {
                            // Figure out how many spaces we're using
                            let mut count = 1;
                            while let Some(' ') = lexer.rchars.last() {
                                lexer.rchars.pop();
                                count += 1;
                                if lexer.spaces_count != 0 && count == lexer.spaces_count {
                                    break;
                                }
                            }
                            if lexer.spaces_count == 0 {
                                lexer.spaces_count = count;
                            }
                            if count == lexer.spaces_count {
                                Tab
                            } else {
                                panic!("expected {} spaces, got {}", lexer.spaces_count, count);
                            }
                        }
                    }
                    _ => continue,
                }
            }
            NextTokenType::Digit(c) => {
                enum NumberLitString {
                    Int(String),
                    Float(String),
                };
                let mut string = NumberLitString::Int(c.to_string());
                loop {
                    match lexer.rchars.last() {
                        Some('0'..='9') => match string {
                            NumberLitString::Int(ref mut s) | NumberLitString::Float(ref mut s) => {
                                s.push(lexer.rchars.pop().unwrap())
                            }
                        },
                        Some('.') => match string {
                            NumberLitString::Int(mut s) | NumberLitString::Float(mut s) => {
                                s.push(lexer.rchars.pop().unwrap());
                                string = NumberLitString::Float(s);
                            }
                        },
                        Some('f') => match string {
                            NumberLitString::Int(s) | NumberLitString::Float(s) => {
                                // ignore the f
                                lexer.rchars.pop();
                                string = NumberLitString::Float(s);
                            }
                        },
                        _ => break,
                    }
                }
                match string {
                    NumberLitString::Int(s) => match s.parse() {
                        Ok(number) => IntLit(number),
                        Err(err) => panic!("{}", err),
                    },
                    NumberLitString::Float(s) => match s.parse() {
                        Ok(number) => FloatLit(number),
                        Err(err) => panic!("{}", err),
                    },
                }
            }
            NextTokenType::CommentOrDivision => {
                match lexer.rchars.last() {
                    Some('/') => {
                        // Comment
                        while lexer.rchars.last() != None && lexer.rchars.last() != Some(&'\n') {
                            lexer.rchars.pop();
                        }
                        continue;
                    }
                    Some('=') => {
                        lexer.rchars.pop();
                        OpAssign(Box::new(Divide))
                    }
                    _ => {
                        // Division
                        Divide
                    }
                }
            }
            NextTokenType::ExternFnOrExternCall => {
                // Don't include the @
                let mut text = String::new();
                while let Some(x) = lexer.rchars.last() {
                    if is_id(*x) {
                        text.push(*x);
                    } else {
                        break;
                    }
                    lexer.rchars.pop();
                }
                match text.as_ref() {
                    "fn" => ExternFn,
                    _ => ExternFnCall(text),
                }
            }
            NextTokenType::AssignmentOrEquals => lexer.reinterpret(Assignment, '=', Equal),
            NextTokenType::LessOrEquals => lexer.reinterpret(Less, '=', LessEqual),
            NextTokenType::GreaterOrEquals => lexer.reinterpret(Greater, '=', GreaterEqual),
            NextTokenType::NotOrEquals => lexer.reinterpret(Not, '=', NotEqual),
            NextTokenType::Or => match lexer.rchars.last() {
                Some('|') => {
                    lexer.rchars.pop();
                    Or
                }
                _ => panic!("unknown token |, did you mean ||"),
            },
            NextTokenType::And => match lexer.rchars.last() {
                Some('&') => {
                    lexer.rchars.pop();
                    And
                }
                _ => panic!("unknown token &, did you mean &&"),
            },
            NextTokenType::Newline => {
                lexer.line += 1;
                lexer.col = 1;
                lexer.col_begin = lexer.rchars.len();
                Newline
            }
            NextTokenType::Plus => lexer.reinterpret(Plus, '=', OpAssign(Box::new(Plus))),
            NextTokenType::Minus => lexer.reinterpret(Minus, '=', OpAssign(Box::new(Minus))),
            NextTokenType::Times => lexer.reinterpret(Times, '=', OpAssign(Box::new(Times))),
            NextTokenType::LParen => LParen,
            NextTokenType::RParen => RParen,
            NextTokenType::Colon => Colon,
            NextTokenType::Comma => Comma,
            NextTokenType::Quote => {
                let mut text = String::new();
                // Don't include literal quote
                loop {
                    // Pop immediately because don't include literal quote anyway
                    match lexer.rchars.pop() {
                        Some('"') => break StringLit(text),
                        Some(x) => text.push(x),
                        None => panic!("unexpected EOF parsing string literal"),
                    }
                }
            }
            NextTokenType::Unknown(c) => {
                panic!("lexer doesn't know what to do with character {}", c);
            }
        };
        // Update col again for proper hi reading
        lexer.update_col();
        let hi = if lexer.col == 1 {
            // newline fix (hacky)
            lo
        } else {
            (lexer.line, lexer.col - 1) // -1 inclusive span
        };
        lexer.tokens.push(Token { kind: token.clone(), span: Span { hi, lo } });
    }
    lexer.tokens
}

#[cfg(test)]
mod test {
    use super::{lex, Span, Token, TokenType::*};
    #[test]
    fn hello_world() {
        use super::TokenType;
        let lexed = lex(r#"fn main() // hello world
	log("hi")"#);
        let kinds: Vec<TokenType> = lexed.iter().map(|x| x.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                Fn,
                Identifier("main".to_string()),
                LParen,
                RParen,
                Newline,
                Tab,
                Identifier("log".to_string()),
                LParen,
                StringLit("hi".to_string()),
                RParen,
            ]
        );
    }
    #[test]
    fn digit() {
        let lexed = lex(r#"578 980 4.2 6f 0.0001"#);
        assert_eq!(
            lexed,
            vec![
                Token { kind: IntLit(578), span: Span { lo: (1, 1), hi: (1, 3) } },
                Token { kind: IntLit(980), span: Span { lo: (1, 5), hi: (1, 7) } },
                Token { kind: FloatLit(4.2), span: Span { lo: (1, 9), hi: (1, 11) } },
                Token { kind: FloatLit(6.0), span: Span { lo: (1, 13), hi: (1, 14) } },
                Token { kind: FloatLit(0.0001), span: Span { lo: (1, 16), hi: (1, 21) } },
            ]
        );
    }
    #[test]
    fn multitabs() {
        let lexed = lex("\n\t\t5");
        assert_eq!(
            lexed,
            vec![
                Token { kind: Newline, span: Span { lo: (1, 1), hi: (1, 1) } },
                Token { kind: Tab, span: Span { lo: (2, 1), hi: (2, 1) } },
                Token { kind: Tab, span: Span { lo: (2, 2), hi: (2, 2) } },
                Token { kind: IntLit(5), span: Span { lo: (2, 3), hi: (2, 3) } },
            ]
        );
    }
}
