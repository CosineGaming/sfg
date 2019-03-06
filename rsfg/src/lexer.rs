// Hello, welcome to my lexer. Please like and subscribe

use crate::Token;

// A-Z or 0-9
fn is_id(c: char) -> bool {
	match c {
		'0'..='9'|'A'..='Z'|'a'..='z' => true,
		_ => false
	}
}

enum NextTokenType {
	EOF,
	SymbolOrId(char),
	Space(char),
	Digit(char),
	CommentOrDivision,
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

	fn next_symbol_type(&mut self) -> NextTokenType {
		use NextTokenType::*;

		match self.rchars.pop() {
			None => EOF,
			Some(c) => match c {
				'A'..='Z'|'a'..='z' => SymbolOrId(c),
				' ' | '\t' => Space(c),
				'0'..='9' => Digit(c),
				'/' => CommentOrDivision,
				'\n' => Newline,
				'(' => LParen,
				')' => RParen,
				':' => Colon,
				',' => Comma,
				'"' => Quote,
				o => Unknown(o)
			}
		}
	}
}

pub fn lex(text: &str) -> Vec<Token> {
	use Token::*;
	let mut lexer = Lexer::new(text);
	loop {
		println!("lexerr {:?}", lexer);
		let token = match lexer.next_symbol_type() {
			NextTokenType::EOF => {
				// This is the end of the file, which is OK, as we are not in the middle
				// of matching a token
				break;
			}
			NextTokenType::SymbolOrId(c) => {
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
			NextTokenType::Space(c) => {
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
			NextTokenType::Digit(c) => {
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
			NextTokenType::CommentOrDivision => {
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
			NextTokenType::Newline => Newline,
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
