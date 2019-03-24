// Hello, welcome to my lexer. Please like and subscribe

use crate::{TokenType, Token};

static DEBUG_LEXER: bool = false;

// A-Z or 0-9
fn is_id(c: char) -> bool {
	match c {
		'0'..='9'|'A'..='Z'|'a'..='z'|'_' => true,
		_ => false
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
			line: 0,
			col: 0,
			col_begin: source.len(),
		}
	}

	fn next_symbol_type(&mut self) -> NextTokenType {
		use NextTokenType::*;

		match self.rchars.pop() {
			None => EOF,
			Some(c) => match c {
				'A'..='Z'|'a'..='z' => SymbolOrId(c),
				'@' => ExternFnOrExternCall,
				'=' => AssignmentOrEquals,
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
	use TokenType::*;
	let mut lexer = Lexer::new(text);
	loop {
		if DEBUG_LEXER {
			println!("lexer status is {:?}", lexer);
		}
		// The number of characters removed is the number that were parsed
		lexer.col += lexer.col_begin - lexer.rchars.len();
		// Yes, this happens *after*. The *start* of a token (col) is the *end* of all previous
		lexer.col_begin = lexer.rchars.len();
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
					"int" => TokenType::Type(crate::Type::Int),
					"str" => TokenType::Type(crate::Type::Str),
					"return" => Return,
					"if" => If,
					_ => Identifier(text),
				};
				symbol_or_id
			}
			NextTokenType::Space(c) => {
				match lexer.tokens.last() {
					Some(&Token { kind: Newline, .. }) => {
						if c == '\t' {
							Tab
						} else {
							// Figure out how many spaces we're using
							let mut count = 1;
							loop {
								match lexer.rchars.last() {
									Some(' ') => {
										lexer.rchars.pop();
										count += 1;
										if lexer.spaces_count != 0 && count == lexer.spaces_count {
											break;
										}
									}
									_ => break, // Ending on whitespace is fine
								};
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
				let mut string = c.to_string();
				loop {
					match lexer.rchars.last() {
						Some('0'...'9') => string.push(lexer.rchars.pop().unwrap()),
						Some('.')|Some('f') => panic!("floats not yet implemented"), // TODO
						_ => break,
					}
				}
				match string.parse() {
					Ok(number) => IntLit(number),
					Err(err) => panic!("{}", err),
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
					_ => {
						// Division
						panic!("division operator not yet supported");
					}
				}
			}
			NextTokenType::ExternFnOrExternCall => {
				// Don't include the @
				let mut text = String::new();
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
					"fn" => ExternFn,
					_ => ExternFnCall(text),
				};
				symbol_or_id
			},
			NextTokenType::AssignmentOrEquals => {
				match lexer.rchars.last() {
					Some('=') => { lexer.rchars.pop(); Equals },
					_ => unimplemented!(),//Assignment,
				}
			},
			NextTokenType::Newline => {
				lexer.line += 1;
				lexer.col = 0;
				Newline
			},
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
		lexer.tokens.push(Token {
			             kind: token.clone(),
			             line: lexer.line,
			             col: lexer.col as usize,
			             });
	}
	lexer.tokens
}

#[cfg(test)]
mod test {
	#[test]
	fn hello_world() {
		use super::lex;
		use super::TokenType::*;
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
		use super::lex;
		use super::TokenType::*;
		use super::Token;
		let lexed = lex(r#"578 980"#);
		assert_eq!(lexed, vec![
			Token {
				kind: IntLit(578),
				line: 0,
				col: 0,
			},
			Token {
				kind: IntLit(980),
				line: 0,
				col: 4,
			}]);
	}
}
