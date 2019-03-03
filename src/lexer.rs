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

pub fn lex(text: String) -> Vec<Token> {
	use Token::*;
	// Start with a newline. We could probly find a better way but meh
	let mut tokens = Vec::<Token>::new();
	let mut spaces_count = 0;
	let mut rchars: Vec<char> = text.chars().collect();
	rchars.reverse();
	loop {
		let c = match rchars.last() {
			Some(c) => *c,
			None => break, // We weren't in the middle of anything so successful EOF
		};
		let token = if is_id_1st(c) {
			let mut text = c.to_string();
			rchars.pop();
			loop {
				let x = match rchars.last() {
					Some(x) => *x,
					None => break, // End of ID is fine
				};
				if is_id(x) {
					text.push(x);
				} else {
					break;
				}
				rchars.pop();
			}
			let symbol_or_id = match text.as_ref() {
				"fn" => Fn,
				// These names clash, it sucks
				"int" => Token::Type(crate::Type::Int),
				_ => Identifier(text),
			};
			symbol_or_id
		} else if SPACE.contains(c) {
			if tokens.last() == Some(&Newline) {
				if c == '\t' {
					rchars.pop();
					Tab
				} else {
					// Figure out how many spaces we're using
					let mut count = 0;
					loop {
						match rchars.last() {
							Some(' ') =>  {
								rchars.pop();
								count += 1;
								if spaces_count != 0 && count == spaces_count {
									break;
								}
							},
							_ => break, // Ending on whitespace is fine
						};
					}
					if spaces_count == 0 && count != 0 {
						spaces_count = count;
					}
					if count == spaces_count {
						Tab
					} else {
						panic!("expected {} spaces, got {}", spaces_count, count);
					}
				}
			} else {
				rchars.pop();
				continue
			}
		} else if c == '\n' {
			rchars.pop();
			Newline
		} else if c == '(' {
			rchars.pop();
			LParen
		} else if c == ')' {
			rchars.pop();
			RParen
		} else if c == ':' {
			rchars.pop();
			Colon
		} else if c == ',' {
			rchars.pop();
			Comma
		} else if c == '"' {
			let mut text = String::new();
			// Don't include literal quote
			rchars.pop();
			loop {
				// Pop immediately because don't include literal quote anyway
				match rchars.pop() {
					Some('"') => break StringLit(text),
					Some(_) => text.push(c),
					None => panic!("unexpected EOF parsing string literal"),
				}
			}
		} else {
			// TODO: How to make error show these automatically like rust?
			panic!("lexer doesn't know what to do with character {}", c);
		};
		tokens.push(token);
	}
	tokens
}
