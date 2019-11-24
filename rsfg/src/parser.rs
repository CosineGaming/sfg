// This is the parser. yay.

use crate::{ast::*, Token, TokenType, Type};

#[derive(Debug)]
pub enum ParseError {
    // Expected, got
    Expected(Vec<TokenType>, Token),
    CouldNotConstruct(Vec<ParseError>),
    EOF(String),
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ParseError::*;
        match self {
            Expected(expected, got) => {
                let expected_strings: Vec<String> =
                    expected.iter().map(|e| format!("{:?}", e)).collect();
                let expected_str = expected_strings.join(" or ");
                write!(
                    f,
                    "expected {}, got {:?} at {}:{}",
                    expected_str, got.kind, got.line, got.col
                )
            }
            CouldNotConstruct(errs) => {
                let error_strs: Vec<String> = errs.iter().map(Self::to_string).collect();
                let error_str = error_strs.join("\n\n");
                write!(f, "could not construct any possible variant expected here. the following errors were returned:\n\n{}", error_str)
            }
            EOF(parsing) => write!(f, "unexpected EOF parsing {}", parsing),
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

/// There is a different calling convention for a parse result because of token handling
/// Rolls back tokens if try fails
macro_rules! rb {
    ( $tokens:ident, $call:expr ) => {{
        // Allow rollback on error
        let saved_tokens = $tokens.clone();
        // Rollback on any error, regardless of intended use
        let res = $call;
        if res.is_err() {
            *$tokens = saved_tokens;
        }
        res
    }};
}
/// Combines rb with try/?
/// Not strictly necessary
macro_rules! rb_try {
    ( $tokens:ident, $call:expr ) => {
        rb!($tokens, $call)?
    };
}
/// Combines rb with ok_or, which is a hypothetical macro
/// ok_or returns result on ok, or on err continues on as normal
macro_rules! rb_ok_or {
    ( $tokens:ident, $call:expr ) => {
        match rb!($tokens, $call) {
            Ok(what) => return Ok(what),
            Err(err) => err,
        }
    };
}

macro_rules! expect_any {
    ( $during:literal, $to_match:expr => { $($token_type:ident$(($subordinate:pat,$literal:expr))? => $expr:expr $(,)?)* } ) => {
        match $to_match {
            $(Some(Token { kind: TokenType::$token_type$(($subordinate))?, .. }) => {Ok($expr)})*,
            Some(got) => Err(ParseError::Expected(vec![
                $(
                    // This should be illegal, may be illegal, is hacky, but it's necessary
                    TokenType::$token_type
                    $(($literal))?
                ),*
            ], got.clone())),
            None => Err(ParseError::EOF($during.to_string()))
        }
    }
}

fn pop_no_eof(from: &mut Tokens, parsing_what: &str) -> Result<Token> {
    match from.pop() {
        Some(token) => Ok(token),
        None => Err(ParseError::EOF(parsing_what.to_string())),
    }
}
/// Only pops if the next token is expected, then returns that token (otherwise Err::EOF)
fn expect_token(rtokens: &mut Tokens, what: TokenType, during: &str) -> Result<Token> {
    match rtokens.pop() {
        Some(token) => {
            if token.kind == what {
                Ok(token)
            } else {
                Err(ParseError::Expected(vec![what], token))
            }
        }
        None => Err(ParseError::EOF(during.to_string())),
    }
}

fn parse_id(rtokens: &mut Tokens, type_required: bool) -> Result<TypedId> {
    let name = match rb_try!(rtokens, pop_no_eof(rtokens, "identifier")).kind {
        TokenType::Identifier(name) => name,
        _ => panic!("identifier wasn't identifier (compiler bug)"),
    };
    // Can't use expect_any! because not Some(got) has special semantics
    match rtokens.last() {
        Some(Token { kind: TokenType::Colon, .. }) => {
            rtokens.pop();
            let id_type = expect_any!("identifier type", rtokens.pop() => {
                Type(id_type,Type::Infer) => id_type,
            })?;
            return Ok(TypedId { name, id_type });
        }
        Some(got) => {
            if type_required {
                return Err(ParseError::Expected(
                    vec![TokenType::Type(Type::Str), TokenType::Type(Type::Int)],
                    got.clone(),
                ));
            }
        }
        None => return Err(ParseError::EOF("identifier".to_string())),
    }
    Ok(TypedId { name, id_type: Type::Infer })
}

fn token_to_binary_op(token: Option<Token>) -> Result<BinaryOp> {
    expect_any!("binary expression", token => {
        Equal => BinaryOp::Equal,
        Greater => BinaryOp::Greater,
        GreaterEqual => BinaryOp::GreaterEqual,
        Less => BinaryOp::Less,
        LessEqual => BinaryOp::LessEqual,
        NotEqual => BinaryOp::NotEqual,
        And => BinaryOp::And,
        Or => BinaryOp::Or,
        Plus => BinaryOp::Plus,
        Minus => BinaryOp::Minus,
        Times => BinaryOp::Times,
        Divide => BinaryOp::Divide,
    })
}

fn parse_binary(rtokens: &mut Tokens, left: Expression) -> Result<BinaryExpr> {
    let op = token_to_binary_op(rtokens.pop())?;
    let right = rb_try!(rtokens, parse_expression(rtokens));
    Ok(BinaryExpr { left, op, right })
}

fn parse_expression(rtokens: &mut Tokens) -> Result<Expression> {
    // First we parse the left side of a binary expression which COULD be the whole expression
    let left = expect_any!("expression", rtokens.last() => {
        // In order to give binary operator precedence to parenthesis
        LParen => {
            rtokens.pop();
            let res = parse_expression(rtokens);
            expect_any!("parenthesized expression", rtokens.pop() => {
                RParen => res,
            })?
        }
        // Not
        Not => {
            rtokens.pop();
            let not_of = parse_expression(rtokens)?;
            Ok(Expression::Not(Box::new(not_of)))
        }
        // Literals
        StringLit(string,String::from("")) => {
            rtokens.pop();
            Ok(Expression::Literal(Literal::String(string.clone())))
        }
        IntLit(number,0) => {
            rtokens.pop();
            Ok(Expression::Literal(Literal::Int(*number)))
        }
        // This minus, because we're parsing an expression, is part of an int literal
        Minus => {
            rtokens.pop();
            expect_any!("expression", rtokens.pop() => {
                IntLit(number,0) => {
                    Expression::Literal(Literal::Int(-number))
                }
            })
        }
        // Builtin literals
        True => {
            rtokens.pop();
            Ok(Expression::Literal(Literal::Bool(true)))
        }
        False => {
            rtokens.pop();
            Ok(Expression::Literal(Literal::Bool(false)))
        }
        // And finally identifiers
        Identifier(name,String::from("")) => {
            // An identifier can start a call or just an identifier
            // It can be a call...
            let call_res =
                rb!(rtokens, parse_call(rtokens).and_then(|x| Ok(Expression::FnCall(x))));
            if call_res.is_err() {
                rtokens.pop();
                // Otherwise just reference the identifier
                Ok(Expression::Identifier(TypedId { name: name.clone(), id_type: Type::Infer }))
            } else {
                call_res
            }
        }
    })??;
    println!("{:?}", left);
    // Then we try to parse a binary expression with it
    rb_ok_or!(
        rtokens,
        parse_binary(rtokens, left.clone()).and_then(|x| Ok(Expression::Binary(Box::new(x))))
    );
    // If not, it's just a unary one
    Ok(left)
}

fn parse_call(rtokens: &mut Tokens) -> Result<FnCall> {
    let first_token = rtokens.last();
    let name = expect_any!("call", first_token => {
        Identifier(_n,String::from("")) => match rtokens.pop() {
            Some(Token { kind: TokenType::Identifier(name), .. }) => name,
            _ => unreachable!(),
        },
    })?;
    // Arguments
    rb_try!(rtokens, expect_token(rtokens, TokenType::LParen, "fn call"));
    let mut arguments = vec![];
    loop {
        arguments.push(match rtokens.last() {
            Some(Token { kind: TokenType::RParen, .. }) => {
                rtokens.pop();
                break;
            }
            Some(Token { kind: TokenType::Comma, .. }) => {
                rtokens.pop();
                continue;
            }
            _ => rb_try!(rtokens, parse_expression(rtokens)),
        });
    }
    // Panic/assert has special handling because of line/col args
    match &name[..] {
        // ==1 => Don't do it if line/col explicit
        "panic" | "assert" if arguments.len() <= 1 => {
            // Safe because we wouldn't be here without a token
            arguments.push(Expression::Literal(Literal::Int(first_token.unwrap().line as i32)));
            arguments.push(Expression::Literal(Literal::Int(first_token.unwrap().col as i32)));
        }
        _ => (),
    }
    Ok(FnCall { name, arguments })
}

fn parse_args(rtokens: &mut Tokens) -> Result<Vec<TypedId>> {
    let mut args = vec![];
    rb_try!(rtokens, expect_token(rtokens, TokenType::LParen, "fn parameters"));
    loop {
        // TODO: Why unreachable code warning here? Tests pass
        expect_any!("parameters", rtokens.last() => {
            Identifier(__,String::from("")) => {
                args.push(rb_try!(rtokens, parse_id(rtokens, true)));
            }
            RParen => {
                rtokens.pop();
                break;
            }
        })?;
        expect_any!("fn params", rtokens.pop().as_ref() => {
            Comma => (),
            RParen => break,
        })?;
    }
    Ok(args)
}

fn parse_return(rtokens: &mut Tokens) -> Result<Option<Expression>> {
    rb_try!(rtokens, expect_token(rtokens, TokenType::Return, "return statement"));
    Ok(match parse_expression(rtokens) {
        Ok(expr) => Some(expr),
        Err(_) => None,
    })
}

fn parse_if(rtokens: &mut Tokens, tabs: usize) -> Result<If> {
    debug!("if");
    rb_try!(rtokens, expect_token(rtokens, TokenType::If, "if statement"));
    let condition = rb_try!(rtokens, parse_expression(rtokens));
    let statements = rb_try!(rtokens, parse_indented_block(rtokens, tabs + 1));
    // Remember, rb! JUST rolls back on error, but doesn't necessarily return!
    // rb is still necessary to not eat up following non-else code and first tab
    let else_or_err = rb!(rtokens, {
        match safe_expect_indent(rtokens, tabs) {
            Ok(()) => {
                // Indent exists. Check for else (still optional)
                let else_result = expect_token(rtokens, TokenType::Else, "if statement");
                match else_result {
                    Ok(_) => {
                        let else_statements = expect_any!("if-else", rtokens.last() => {
                            If => {
                                // else if
                                vec![Statement::If(parse_if(rtokens, tabs)?)]
                            }
                            Newline => {
                                // else
                                // 	stuff
                                parse_indented_block(rtokens, tabs + 1)?
                            }
                        })?;
                        debug!("there IS an else!");
                        Ok(else_statements)
                    }
                    Err(_) => {
                        // There's no else, but there was tab. rollback tab eating with an error
                        debug!("indent, but not else");
                        Err(())
                    }
                }
            }
            Err(_) => {
                // No indent exists. It's long since time to cede back (end if SURROUNDING block)
                debug!("no indent at all");
                Err(())
            }
        }
    });
    match else_or_err {
        Ok(else_statements) => {
            Ok(If { condition, statements, else_statements })
        }
        Err(()) => {
            Ok(If { condition, statements, else_statements: vec![] })
        }
    }
}

fn parse_loop(rtokens: &mut Tokens, tabs: usize) -> Result<WhileLoop> {
    rb_try!(rtokens, expect_token(rtokens, TokenType::While, "if statement"));
    let condition = rb_try!(rtokens, parse_expression(rtokens));
    let statements = rb_try!(rtokens, parse_indented_block(rtokens, tabs + 1));
    Ok(WhileLoop { condition, statements })
}

fn parse_assignment(rtokens: &mut Tokens) -> Result<Assignment> {
    let name = expect_any!("assignment", rtokens.pop() => {
        Identifier(name,String::from("")) => name,
    })?;
    let op = rtokens.pop();
    let rhs = parse_expression(rtokens)?;
    expect_any!("assignment", op => {
        Assignment => {
            Assignment { lvalue: name, rvalue: rhs }
        }
        OpAssign(ref _op, Box::new(TokenType::Identifier(String::from("any binary operator")))) => {
            let dummy_op_token = match op {
                Some(Token { kind: TokenType::OpAssign(right), line, col }) => Token {
                    kind: *right, line, col
                },
                _ => unreachable!()
            };
            let op = Expression::Binary(Box::new(BinaryExpr {
                // TODO: It is at this moment that i realize it should be id_type: Option<Type>, not Type::Infer
                left: Expression::Identifier(TypedId { name: name.clone(), id_type: Type::Infer }),
                op: token_to_binary_op(Some(dummy_op_token))?,
                right: rhs,
            }));
            Assignment { lvalue: name, rvalue: op }
        }
    })
}
/// Declaration is just an assignment starting with var
fn parse_declaration(rtokens: &mut Tokens) -> Result<Assignment> {
    expect_token(rtokens, TokenType::Declare, "declaration")?;
    parse_assignment(rtokens)
}

fn parse_statement(rtokens: &mut Tokens, tabs: usize) -> Result<Statement> {
    let mut errors = vec![];
    errors.push(rb_ok_or!(rtokens, parse_call(rtokens).and_then(|x| Ok(Statement::FnCall(x)))));
    errors.push(rb_ok_or!(rtokens, parse_return(rtokens).and_then(|x| Ok(Statement::Return(x)))));
    errors.push(rb_ok_or!(rtokens, parse_if(rtokens, tabs).and_then(|x| Ok(Statement::If(x)))));
    errors.push(rb_ok_or!(
        rtokens,
        parse_loop(rtokens, tabs).and_then(|x| Ok(Statement::WhileLoop(x)))
    ));
    errors.push(rb_ok_or!(
        rtokens,
        parse_assignment(rtokens).and_then(|x| Ok(Statement::Assignment(x)))
    ));
    errors.push(rb_ok_or!(
        rtokens,
        parse_declaration(rtokens).and_then(|x| Ok(Statement::Declaration(x)))
    ));
    Err(ParseError::CouldNotConstruct(errors))
}

fn parse_signature(rtokens: &mut Tokens) -> Result<Signature> {
    // An extern function that serves only as a typecheck might use the
    // @ in the name. The lexer misinterprets this as ExternFnCall despite
    // not being a call
    let name = expect_any!("fn", rtokens.pop() => {
        Identifier(name,String::from("")) => name,
        ExternFnCall(name,String::from("")) => name,
    })?;
    let parameters = rb_try!(rtokens, parse_args(rtokens));
    let return_type = expect_any!("assignment", rtokens.last() => {
        Type(__,Type::Infer) => match rtokens.pop() {
            Some(Token { kind: TokenType::Type(r_type), .. }) => Some(r_type),
            _ => unreachable!(),
        },
        Newline => None,
    })?;
    expect_any!("assignment", rtokens.pop() => {
        Newline => (),
    })?;
    Ok(Signature { name, parameters, return_type })
}

/// Strips empty/tab/comment lines, does nothing if no empty lines, rolls back on error state
fn strip_white_lines(rtokens: &mut Tokens) {
    loop {
        if rb!(rtokens, {
            // Consider the following program:
            // fn main()
            //     return 5
            //     //if 5
            //         //something else
            // We want this commenting style to work, so we must:
            // - allow *at least* n tabs
            // - allow a newline again with no statement
            // To allow n tabs:
            match rtokens.last() {
                // If there's an extra tab, get ALL the extra tabs
                Some(Token { kind: TokenType::Tab, .. }) => {
                    debug!("PARSER: extra tab found");
                    while let Some(Token { kind: TokenType::Tab, .. }) = rtokens.last() {
                        rtokens.pop();
                    }
                    // And *demand* there's no expression (otherwise it's an unexpected indent)
                    match expect_token(rtokens, TokenType::Newline, "unexpected indented block") {
                        // There should be an easier way to destroy insides
                        Ok(_) => Ok(()),
                        Err(_) => Err(()),
                    }
                }
                // Otherwise, *allow* no expression
                Some(Token { kind: TokenType::Newline, .. }) => {
                    rtokens.pop();
                    Ok(())
                }
                // Not tab or newline, we've come to our end (will rollback our non-changes)
                _ => Err(())
            }
        }).is_err() {
            // Already cleaned up with rb!, and we found the end of empty lines
            break;
        }
        // Otherwise may be more empty lines ahead, continue
    }
}

/// If an indent of `tabs` count exists, then pop them all, and return Ok(())
/// Otherwise, return Err(()) and RTOKENS IS IN ERROR STATE
fn expect_indent(rtokens: &mut Tokens, tabs: usize) -> Result<()> {
    for _ in 0..tabs {
        if let Err(err) = expect_token(rtokens, TokenType::Tab, "indented block") {
            // Failed to satisfy an indent
            return Err(err);
        }
    }
    Ok(())
}

/// Correctly strip all empty lines. Then expect the indent. Return Ok(()) on success
/// If indent is incorrect, rollback check for indent, but NOT STRIPPING LINES
fn safe_expect_indent(rtokens: &mut Tokens, tabs: usize) -> Result<()> {
    strip_white_lines(rtokens);
    rb!(rtokens, expect_indent(rtokens, tabs))
}

fn parse_indented_block(rtokens: &mut Tokens, expect_tabs: usize) -> Result<Vec<Statement>> {
    let mut statements = vec![];
    loop {
        // Allow empty lines amongst function an indented statement
        if let Some(Token { kind: TokenType::Newline, .. }) = rtokens.last() {
            rtokens.pop();
            continue;
        }
        // If we can't satisfy the indent, return immediately with the statements we've collected
        if safe_expect_indent(rtokens, expect_tabs).is_err() {
            return Ok(statements);
        }
        statements.push(rb_try!(rtokens, parse_statement(rtokens, expect_tabs)));
    }
}

fn parse_fn(rtokens: &mut Tokens) -> Result<Fn> {
    rb_try!(rtokens, expect_token(rtokens, TokenType::Fn, "fn"));
    // Parse signature
    let signature = rb_try!(rtokens, parse_signature(rtokens));
    let statements = rb_try!(rtokens, parse_indented_block(rtokens, 1));
    Ok(Fn { signature, statements })
}

fn parse_extern_fn(rtokens: &mut Tokens) -> Result<ExternFn> {
    expect_token(rtokens, TokenType::ExternFn, "extern fn")?;
    let signature = rb_try!(rtokens, parse_signature(rtokens));
    Ok(ExternFn { signature })
}

pub fn parse(mut tokens: Vec<Token>) -> Result<AST> {
    tokens.reverse();
    // This is just for clarity
    let mut rtokens = NoPop::new(&tokens);
    let mut t;
    let mut ast = vec![];
    // Every token
    loop {
        t = match rtokens.last() {
            Some(t) => t,
            None => break,
        };
        expect_any!("global space", Some(t) => {
            // Parse a function
            Fn => ast.push(ASTNode::Fn(parse_fn(&mut rtokens)?)),
            ExternFn => ast.push(ASTNode::ExternFn(parse_extern_fn(&mut rtokens)?)),
            Newline => {rtokens.pop();}
            Tab => {rtokens.pop();}
        })?
    }
    Ok(ast)
}

#[derive(Clone, Copy)]
struct NoPop<'a, T: Clone> {
    vec: &'a [T],
    sp: usize,
}
impl<'a, T: Clone> NoPop<'a, T> {
    fn new(vec: &'a [T]) -> Self {
        Self { vec, sp: vec.len() }
    }
    fn pop(&mut self) -> Option<T> {
        if self.sp > 0 {
            self.sp -= 1;
            Some(self.vec[self.sp].clone())
        } else {
            None
        }
    }
    fn last(&mut self) -> Option<&'a T> {
        if self.sp > 0 {
            self.vec.get(self.sp - 1)
        } else {
            None
        }
    }
}
type Tokens<'a> = NoPop<'a, Token>;

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    // This is NOT meant to test recursion, it's meant as a hello-world
    // that only uses function definition and calling, which means we have
    // no one to call but ourselves
    fn recurse() {
        use super::TokenType::*;
        let ast = parse(
            vec![
                Fn,
                Identifier("main".to_string()),
                LParen,
                RParen,
                Newline,
                Tab,
                Identifier("main".to_string()),
                LParen,
                RParen,
            ]
            .iter()
            .map(|t| Token { kind: t.clone(), line: 0, col: 0 })
            .collect(),
        ).expect("test program parse error");
        assert_eq!(
            ast,
            vec![ASTNode::Fn(crate::ast::Fn {
                signature: Signature {
                    name: "main".to_string(),
                    parameters: vec![],
                    return_type: None,
                },
                statements: vec![Statement::FnCall(FnCall {
                    name: "main".to_string(),
                    arguments: vec![],
                })],
            })]
        );
    }
    #[test]
    fn parse_expression_rollback() {
        // The code is listed in order FnCall, then return
        // We need to test to make sure it can roll back properly
        use super::TokenType::*;
        let ast = parse(
            vec![Fn, Identifier("main".to_string()), LParen, RParen, Newline, Tab, Return]
                .iter()
                .map(|t| Token { kind: t.clone(), line: 0, col: 0 })
                .collect(),
        ).expect("test program parse error");
        if let ASTNode::Fn(func) = &ast[0] {
            assert_eq!(func.statements, vec![Statement::Return(None),]);
        } else {
            unreachable!()
        }
    }
}
