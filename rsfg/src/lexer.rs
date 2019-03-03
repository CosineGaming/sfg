// Hello, welcome to my lexer. Please like and subscribe

use crate::Token;

static SPACE: &str = " \t";

// Rules: A-Z,a-z
fn is_id_1st(c: char) -> bool {
    c >= 'A' && c <= 'z'
}

// A-Z or 0-9
fn is_id(c: char) -> bool {
    is_id_1st(c) || (c >= '0' && c <= '9')
}

enum NextSymbolType {
    None,
    SymbolOrId(char),
    Space(char),
    Digit(char),
    Slash,
    Newline,
    LParen,
    RParen,
    Colon,
    Comma,
    Quote,
    Unknown(char),
}

#[derive(Debug)]
struct Lexer<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    spaces_count: usize,
    rchars: Vec<char>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source,
            tokens: Vec::new(),
            spaces_count: 0,
            rchars: source.chars().rev().collect(),
        }
    }

    fn is_eof(&self) -> bool {
        self.rchars.len() == 0
    }

    fn next_symbol_type(&mut self) -> NextSymbolType {
        use NextSymbolType::*;

        if self.is_eof() {
            return None;
        }

        let c = self.rchars.pop().expect("not supposed to be at EOF");

        if is_id_1st(c) {
            SymbolOrId(c)
        } else if SPACE.contains(c) {
            Space(c)
        } else if c >= '0' && c <= '9' {
            Digit(c)
        } else if c == '/' {
            Slash
        } else if c == '\n' {
            Newline
        } else if c == '(' {
            LParen
        } else if c == ')' {
            RParen
        } else if c == ':' {
            Colon
        } else if c == ',' {
            Comma
        } else if c == '"' {
            Quote
        } else {
            Unknown(c)
        }
    }
}

pub fn lex(text: &str) -> Vec<Token> {
    use Token::*;
    let mut lexer = Lexer::new(text);
    loop {
        println!("{:?}", lexer);
        let token = match lexer.next_symbol_type() {
            NextSymbolType::None => {
                // This is the end of the file, which is OK, as we are not in the middle
                // of matching a token
                break;
            }
            NextSymbolType::SymbolOrId(c) => {
                let mut text = c.to_string();
                loop {
                    let x = match lexer.rchars.last() {
                        Some(x) => *x,
                        None => break, // End of ID is fine
                    };
                    if is_id(x) {
                        text.push(x);
                    } else {
                        break;
                    }
                    lexer.rchars.pop();
                }
                let symbol_or_id = match text.as_ref() {
                    "fn" => Fn,
                    // These names clash, it sucks
                    "int" => Token::Type(crate::Type::Int),
                    _ => Identifier(text),
                };
                symbol_or_id
            }
            NextSymbolType::Space(c) => {
                if lexer.tokens.last() == Some(&Newline) {
                    if c == '\t' {
                        Tab
                    } else {
                        // Figure out how many spaces we're using
                        let mut count = 0;
                        loop {
                            match c {
                                ' ' => {
                                    count += 1;
                                    if lexer.spaces_count != 0 && count == lexer.spaces_count {
                                        break;
                                    }
                                }
                                _ => break, // Ending on whitespace is fine
                            };
                        }
                        if lexer.spaces_count == 0 && count != 0 {
                            lexer.spaces_count = count;
                        }
                        if count == lexer.spaces_count {
                            Tab
                        } else {
                            panic!("expected {} spaces, got {}", lexer.spaces_count, count);
                        }
                    }
                } else {
                    continue;
                }
            }
            NextSymbolType::Digit(c) => {
                let mut string = c.to_string();
                loop {
                    match lexer.rchars.last() {
                        Some('0'...'9') => string.push(lexer.rchars.pop().unwrap()),
                        Some('.') | Some('f') => panic!("floats not yet implemented"), // TODO
                        _ => break,
                    }
                }
                match string.parse() {
                    Ok(number) => IntLit(number),
                    Err(err) => panic!("{}", err),
                }
            }
            NextSymbolType::Slash => {
                match lexer.rchars.pop() {
                    Some('/') => {
                        // Comment
                        while lexer.rchars.last() != None && lexer.rchars.last() != Some(&'\n') {
                            lexer.rchars.pop();
                        }
                        continue;
                    }
                    _ => {
                        // Division
                        panic!("division operator not yet supported");
                    }
                }
            }
            NextSymbolType::Newline => Newline,
            NextSymbolType::LParen => LParen,
            NextSymbolType::RParen => RParen,
            NextSymbolType::Colon => Colon,
            NextSymbolType::Comma => Comma,
            NextSymbolType::Quote => {
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
            NextSymbolType::Unknown(c) => {
                // TODO: How to make error show these automatically like rust?
                panic!("lexer doesn't know what to do with character {}", c);
            }
        };
        lexer.tokens.push(token);
    }
    lexer.tokens
}

#[cfg(test)]
mod test {
    #[test]
    fn hello_world() {
        use super::lex;
        use super::Token::*;
        let lexed = lex(r#"fn main() // hello world
	log("hi")"#);
        assert_eq!(
            lexed,
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
        use super::lex;
        use super::Token::*;
        let lexed = lex(r#"578 9"#);
        assert_eq!(lexed, vec![IntLit(578), IntLit(9),]);
    }
}
