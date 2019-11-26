// This is the parser. yay.

use crate::{ast::*, Span, Token, TokenType, Type};

#[derive(Debug)]
pub enum ParseError {
    // Expected, got
    Expected(Vec<TokenType>, Token),
    EOF(String),
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ParseError::*;
        match self {
            Expected(expected, got) => {
                let expected_strings: Vec<String> =
                    expected.iter().map(|e| format!("{}", e)).collect();
                let expected_str = expected_strings.join(", ");
                write!(f, "[ERROR] expected {}, got {} at {}", expected_str, got.kind, got.span)
            }
            EOF(parsing) => write!(f, "[ERROR] unexpected EOF parsing {}", parsing),
        }
    }
}
pub fn fmt_vec<T: std::fmt::Display>(vec: &Vec<T>) -> String {
    vec.iter().map(|e| format!("{}", e)).collect::<Vec<String>>().join("\n")
}
// All relevant details in Display and Debug
impl std::error::Error for ParseError {}
impl ParseError {
    fn v(self) -> Vec<Self> {
        vec![self]
    }
}

#[cfg(test)]
fn dexpect<T>(res: Result<T>) -> T {
    match res {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{}", fmt_vec(&e));
            panic!("unexpected parse error");
        }
    }
}

type Result<T> = std::result::Result<T, Vec<ParseError>>;

static PANIC_ON_ERROR: bool = false;

static IDENT: &str = "identifier";

macro_rules! expect_any {
    ( $during:literal, $to_match:expr => { $($token_type:ident$(($subordinate:pat,$literal:expr))? => $expr:expr $(,)?)* } ) => {
        match $to_match {
            $(Some(Token { kind: TokenType::$token_type$(($subordinate))?, .. }) => {Ok($expr)})*,
            Some(got) => {
                if PANIC_ON_ERROR {
                    panic!("PANIC_ON_ERROR enabled");
                }
                Err(ParseError::Expected(vec![
                    $(
                        // This should be illegal, may be illegal, is hacky, but it's necessary
                        TokenType::$token_type
                        $(($literal.to_owned()))?
                    ),*
                ], got.clone()).v())
            }
            None => Err(ParseError::EOF($during.to_string()).v())
        }
    }
}

macro_rules! known {
    ( $what:expr, $known:ident ) => {
        match $what {
            TokenType::$known(val) => val,
            _ => unreachable!(),
        }
    };
}

macro_rules! resolve1 {
    ( $($ident:ident),+ ) => {
        let mut errs = vec![];
        $(
            match $ident {
                Ok(_) => (),
                Err(ref mut e) => errs.append(e),
            };
        )+
        if !errs.is_empty() {
            return Err(errs);
        }
        // we now know these aren't errors
        $(
            #[allow(unused_variables, unused_mut)]
            let mut $ident = $ident.unwrap();
        )+
    }
}

macro_rules! resolve {
    ( $($ident:ident),+$(;)?$($resarray:ident),* ) => {
        $(
            let mut $resarray = vec_to_res($resarray);
        )*
        resolve1!($($ident),+$(,$resarray)*)
    }
}

fn vec_to_res<T>(vec: Vec<Result<T>>) -> Result<Vec<T>> {
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

fn pop_no_eof(from: &mut Tokens, parsing_what: &str) -> Result<Token> {
    match from.pop() {
        Some(token) => Ok(token),
        None => Err(ParseError::EOF(parsing_what.to_string()).v()),
    }
}
/// Only pops if the next token is expected, then returns that token (otherwise Err::EOF)
fn expect_token(rtokens: &mut Tokens, what: TokenType, during: &str) -> Result<Token> {
    match rtokens.pop() {
        Some(token) => {
            if token.kind == what {
                Ok(token)
            } else {
                Err(ParseError::Expected(vec![what], token).v())
            }
        }
        None => Err(ParseError::EOF(during.to_string()).v()),
    }
}

fn parse_id(rtokens: &mut Tokens, type_required: bool) -> Result<Id> {
    let token = pop_no_eof(rtokens, "identifier")?;
    let mut id_span = token.span;
    let mut expected_id = expect_any!("identifier", Some(&token) => {
        Identifier(_, String::new()) => (),
    });
    let mut id_type = None;
    // Can't use expect_any! because not Some(got) has special semantics
    match rtokens.last() {
        Some(Token { kind: TokenType::Colon, .. }) => {
            let t = rtokens.pop().unwrap();
            let mut certain_type = expect_any!("identifier type", rtokens.pop() => {
                Type(id_type,Type::Int) => id_type,
            });
            resolve!(expected_id, certain_type);
            id_type = Some(certain_type);
            id_span = Span::set(vec![t.span, id_span]);
        }
        Some(got) => {
            if type_required {
                let mut no_type: Result<()> = Err(ParseError::Expected(
                    vec![TokenType::Type(Type::Str), TokenType::Type(Type::Int)],
                    got.clone(),
                )
                .v());
                resolve!(expected_id, no_type);
            } else {
                resolve!(expected_id);
            }
        }
        None => {
            let mut eof: Result<()> = Err(ParseError::EOF("identifier".to_string()).v());
            resolve!(expected_id, eof);
        }
    }
    let name = match token.kind {
        TokenType::Identifier(name) => name,
        _ => panic!("identifier wasn't identifier (compiler bug)"),
    };
    Ok(Id { name, id_type: id_type, span: id_span })
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
    let token = rtokens.pop();
    let span = token.as_ref().unwrap().span;
    let mut op = token_to_binary_op(token);
    let mut right = parse_expression(rtokens);
    resolve!(op, right);
    Ok(BinaryExpr { left, op, right, span })
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
            }).and_then(|v| v)
        }
        // Not
        Not => {
            rtokens.pop();
            let not_of = parse_expression(rtokens)?;
            Ok(Expression::Not(Box::new(not_of)))
        }
        // Literals
        StringLit(_,String::from("")) => {
            let t = rtokens.pop().unwrap();
            let string = known!(t.kind, StringLit);
            Ok(Expression::Literal(Literal {
                data: LiteralData::String(string),
                span: t.span,
            }))
        }
        IntLit(_,0) => {
            let t = rtokens.pop().unwrap();
            let number = known!(t.kind, IntLit);
            Ok(Expression::Literal(Literal {
                data: LiteralData::Int(number),
                span: t.span,
            }))
        }
        FloatLit(_,0.0) => {
            let t = rtokens.pop().unwrap();
            let number = known!(t.kind, FloatLit);
            Ok(Expression::Literal(Literal {
                data: LiteralData::Float(number),
                span: t.span,
            }))
        }
        // This minus, because we're parsing an expression, is part of an int literal
        Minus => {
            let t = rtokens.pop();
            expect_any!("expression", rtokens.pop() => {
                IntLit(number,0) => {
                    Expression::Literal(Literal {
                        data: LiteralData::Int(-number),
                        span: t.unwrap().span,
                    })
                }
                FloatLit(number,0.0) => {
                    Expression::Literal(Literal {
                        data: LiteralData::Float(-number),
                        span: t.unwrap().span,
                    })
                }
            })
        }
        // Builtin literals
        True => {
            let t = rtokens.pop();
            Ok(Expression::Literal(Literal {
                data: LiteralData::Bool(true),
                span: t.unwrap().span,
            }))
        }
        False => {
            let t = rtokens.pop();
            Ok(Expression::Literal(Literal {
                data: LiteralData::Bool(false),
                span: t.unwrap().span,
            }))
        }
        // And finally identifiers
        Identifier(_,IDENT) => {
            // An identifier can start a call or just an identifier
            match rtokens.n(1) {
                Some(Token { kind: TokenType::LParen, .. }) => parse_call(rtokens).and_then(|x| Ok(Expression::FnCall(x))),
                // Otherwise just reference the identifier
                Some(_) => {
                    let t = rtokens.pop().unwrap();
                    let name = known!(t.kind, Identifier);
                    Ok(Expression::Identifier(Id {
                        name: name,
                        id_type: None,
                        span: t.span
                    }))
                }
                None => return Err(ParseError::EOF("expression".to_string()).v()),
            }
        }
    })??;
    // Then we try to parse a binary expression with it
    match token_to_binary_op(rtokens.last().and_then(|t| Some(t.clone()))) {
        Ok(_) => Ok(Expression::Binary(Box::new(parse_binary(rtokens, left)?))),
        // If not, it's just a unary one
        Err(_) => Ok(left),
    }
}

fn parse_call(rtokens: &mut Tokens) -> Result<FnCall> {
    let mut token = expect_any!("call", rtokens.last() => {
        Identifier(_,IDENT) => rtokens.pop().unwrap(),
    });
    // Arguments
    let mut paren = expect_token(rtokens, TokenType::LParen, "fn call");
    let mut arguments = vec![];
    // we need token and paren to exit early to prevent infinite loop
    resolve!(token, paren);
    let final_span;
    loop {
        arguments.push(match rtokens.last() {
            Some(Token { kind: TokenType::RParen, .. }) => {
                final_span = rtokens.pop().unwrap().span;
                break;
            }
            Some(Token { kind: TokenType::Comma, .. }) => {
                rtokens.pop();
                continue;
            }
            Some(_) => {
                let e = parse_expression(rtokens);
                // force to move on if error to prevent infinite loop
                if e.is_err() {
                    rtokens.pop();
                }
                e
            }
            _ => return Err(ParseError::EOF("call".to_string()).v()),
        });
    }
    // can't continue without name, must resolve errors
    let mut arguments = vec_to_res(arguments);
    resolve!(arguments);
    let name = match token.kind {
        TokenType::Identifier(s) => s,
        _ => unreachable!(),
    };
    // Panic/assert has special handling because of line/col args
    match &name[..] {
        // ==1 => Don't do it if line/col explicit
        "panic" | "assert" if arguments.len() <= 1 => {
            // Safe because we wouldn't be here without a token
            arguments.push(Expression::Literal(Literal {
                data: LiteralData::Int(token.span.lo.0 as i32),
                // span immediatly following token
                span: Span { lo: token.span.hi, hi: token.span.hi },
            }));
            arguments.push(Expression::Literal(Literal {
                data: LiteralData::Int(token.span.lo.1 as i32),
                span: Span { lo: token.span.hi, hi: token.span.hi },
            }));
        }
        _ => (),
    }
    let total_span = Span::set(vec![token.span, final_span]);
    Ok(FnCall { name: Id { name, id_type: None, span: token.span }, arguments, span: total_span })
}

fn parse_args(rtokens: &mut Tokens) -> Result<Vec<Id>> {
    let mut args = vec![];
    let mut paren = expect_token(rtokens, TokenType::LParen, "fn parameters");
    let mut arg_errors = vec![];
    loop {
        // TODO: Why unreachable code warning here? Tests pass
        #[allow(unreachable_code)]
        arg_errors.push(expect_any!("parameters", rtokens.last() => {
            Identifier(__,String::from("")) => {
                args.push(parse_id(rtokens, true));
            }
            RParen => {
                rtokens.pop();
                break;
            }
        }));
        // TODO: and here
        #[allow(unreachable_code)]
        arg_errors.push(expect_any!("fn params", rtokens.pop().as_ref() => {
            Comma => (),
            RParen => break,
        }));
    }
    resolve!(paren; arg_errors, args);
    Ok(args)
}

fn parse_return(rtokens: &mut Tokens) -> Result<Option<Expression>> {
    // no valid return without return, no point delaying
    expect_token(rtokens, TokenType::Return, "return statement")?;
    Ok(match parse_expression(rtokens) {
        Ok(expr) => Some(expr),
        Err(_) => None,
    })
}

fn parse_if(rtokens: &mut Tokens, tabs: usize) -> Result<If> {
    // if is necessary
    let span = expect_token(rtokens, TokenType::If, "if statement")?.span;
    let mut condition = parse_expression(rtokens);
    let statements = parse_indented_block(rtokens, tabs + 1);
    // Remember, rb! JUST rolls back on error, but doesn't necessarily return!
    // rb is still necessary to not eat up following non-else code and first tab
    strip_white_lines(rtokens);
    let mut else_statements = if check_indent(rtokens, tabs).is_ok() {
        // we have an indent, check for else (still optional)
        match expect_any!("if-else", rtokens.n(tabs) => {
            Else => {
                debug!("there IS an else!");
                // delete tabs to catch up
                expect_indent(rtokens, tabs).unwrap();
                rtokens.pop(); // pop the else too
                // expect_any! returns a result of whether we matched
                // contained in Ok is a vec of results
                expect_any!("if-else", rtokens.last() => {
                    If => {
                        // else if
                        let if_res = parse_if(rtokens, tabs);
                        vec![if_res.and_then(|i| Ok(Statement::If(i)))]
                    }
                    Newline => {
                        // else
                        // 	stuff
                        parse_indented_block(rtokens, tabs + 1)
                    }
                })
            }
        }) {
            // convert Result<Vec<Result<>>> to Result<Vec<>>
            Ok(else_statements) => else_statements.and_then(vec_to_res),
            // indent but no else
            Err(_) => Ok(vec![]),
        }
    } else {
        // no indent
        Ok(vec![])
    };
    resolve!(condition, else_statements; statements);
    Ok(If { condition, statements, else_statements, span })
}

fn parse_loop(rtokens: &mut Tokens, tabs: usize) -> Result<WhileLoop> {
    let mut span =
        expect_token(rtokens, TokenType::While, "while statement").and_then(|t| Ok(t.span));
    let mut condition = parse_expression(rtokens);
    let statements = parse_indented_block(rtokens, tabs + 1);
    resolve!(span, condition; statements);
    Ok(WhileLoop { condition, statements, span })
}

fn parse_assignment(rtokens: &mut Tokens) -> Result<Assignment> {
    let mut lvalue = parse_id(rtokens, false);
    let op = rtokens.pop();
    let mut rvalue = parse_expression(rtokens);
    resolve!(lvalue, rvalue);
    expect_any!("assignment", op => {
        Assignment => {
            Assignment {
                span: Span::set(vec![lvalue.span, rvalue.full_span()]),
                lvalue,
                rvalue,
            }
        }
        OpAssign(ref _op, Box::new(TokenType::False)) => {
            let span = op.as_ref().unwrap().span;
            let dummy_op_token = match op {
                Some(Token { kind: TokenType::OpAssign(right), span }) => Token {
                    kind: *right, span
                },
                _ => unreachable!()
            };
            let r_op = Expression::Binary(Box::new(BinaryExpr {
                left: Expression::Identifier(lvalue.clone()),
                op: token_to_binary_op(Some(dummy_op_token))?,
                right: rvalue,
                span,
            }));
            Assignment { lvalue, rvalue: r_op, span }
        }
    })
}
/// Declaration is just an assignment starting with var
fn parse_declaration(rtokens: &mut Tokens) -> Result<Assignment> {
    expect_token(rtokens, TokenType::Declare, "declaration")?;
    parse_assignment(rtokens)
}

fn parse_statement(rtokens: &mut Tokens, tabs: usize) -> Result<Statement> {
    Ok(expect_any!("statement", rtokens.last() => {
        Return => Statement::Return(parse_return(rtokens)?),
        If => Statement::If(parse_if(rtokens, tabs)?),
        While => Statement::WhileLoop(parse_loop(rtokens, tabs)?),
        Declare => Statement::Declaration(parse_declaration(rtokens)?),
        // only disambiguating is FnCall vs Assignment. we can use LR(2) for that
        Identifier(_,String::new()) => expect_any!("after identifier", rtokens.n(1) => {
            LParen => Statement::FnCall(parse_call(rtokens)?),
            Assignment => Statement::Assignment(parse_assignment(rtokens)?),
            // TODO: lots of duplication going on here
            OpAssign(_,Box::new(TokenType::False))
                => Statement::Assignment(parse_assignment(rtokens)?)
        })?
    })?)
}

fn parse_signature(rtokens: &mut Tokens) -> Result<Signature> {
    // An extern function that serves only as a typecheck might use the
    // @ in the name. The lexer misinterprets this as ExternFnCall despite
    // not being a call
    let token = rtokens.pop().unwrap();
    let span = token.span;
    let mut name = expect_any!("fn", Some(token) => {
        Identifier(name,String::from("")) => name,
        ExternFnCall(name,String::from("")) => name,
    });
    let mut parameters = parse_args(rtokens);
    let mut final_char_expect = expect_any!("signature", rtokens.last() => {
        Type(__,Type::Int) => match rtokens.pop() {
            Some(Token { kind: TokenType::Type(r_type), span }) => (Some(r_type), span),
            _ => unreachable!(),
        },
        Newline => (None, rtokens.last().unwrap().span) // TODO: subtract one / don't include \n
    });
    let mut final_newline = expect_any!("signature", rtokens.pop() => {
        Newline => (),
    });
    resolve!(name, parameters, final_char_expect, final_newline);
    let (return_type, final_span) = final_char_expect;
    let id = Id { name, id_type: return_type, span };
    let span = Span::set(vec![span, final_span]);
    Ok(Signature { id, parameters, span })
}

/// Strips empty/tab/comment lines, does nothing if no empty lines, rolls back on error state
fn strip_white_lines(rtokens: &mut Tokens) {
    // Consider the following program:
    // fn main()
    //     return 5
    //     //if 5
    //         //something else
    // We want this commenting style to work, so we strip entirely empty lines
    // For each line
    loop {
        let mut count = 0;
        // count whitespace and check if empty
        let empty = loop {
            match rtokens.n(count) {
                // If there's an extra tab, get ALL the extra tabs
                Some(Token { kind: TokenType::Tab, .. }) => count += 1,
                // Otherwise, *allow* no expression
                Some(Token { kind: TokenType::Newline, .. }) => break true,
                // Not tab or newline, this line can't be stripped
                _ => break false,
            }
        };
        if empty {
            // = include newline
            for _ in 0..=count {
                rtokens.pop();
            }
        } else {
            // no longer contiguous
            break;
        }
    }
}

/// Return Ok(()) if indent of `tabs` exists, otherwise Expected/EOF error
fn check_indent(rtokens: &mut Tokens, tabs: usize) -> Result<()> {
    for i in 0..tabs {
        expect_any!("indent", rtokens.n(i) => {
            Tab => (),
        })?
    }
    Ok(())
}

/// If an indent of `tabs` count exists, then pop them all, and return Ok(())
/// Otherwise, do nothing and return Err
fn expect_indent(rtokens: &mut Tokens, tabs: usize) -> Result<()> {
    strip_white_lines(rtokens);
    check_indent(rtokens, tabs)?;
    // once we know we can, pop em right off
    for _ in 0..tabs {
        rtokens.pop();
    }
    Ok(())
}

fn parse_indented_block(rtokens: &mut Tokens, expect_tabs: usize) -> Vec<Result<Statement>> {
    let mut statements = vec![];
    loop {
        // Allow empty lines amongst function an indented statement
        if let Some(Token { kind: TokenType::Newline, .. }) = rtokens.last() {
            rtokens.pop();
            continue;
        }
        // If we can't satisfy the indent, return immediately with the statements we've collected
        if expect_indent(rtokens, expect_tabs).is_err() {
            return statements;
        }
        let statement = parse_statement(rtokens, expect_tabs);
        // on error we want to skip to the next statement because we know this isn't valid
        if statement.is_err() {
            // delete until newline
            loop {
                match expect_token(rtokens, TokenType::Newline, "") {
                    Ok(_) => break,
                    Err(_) => (),
                }
            }
        }
        statements.push(statement);
    }
}

fn parse_fn(rtokens: &mut Tokens) -> Result<Fn> {
    let mut fn_token = expect_token(rtokens, TokenType::Fn, "fn");
    // Parse signature
    let mut signature = parse_signature(rtokens);
    let statements = parse_indented_block(rtokens, 1);
    resolve!(fn_token, signature; statements);
    Ok(Fn { signature, statements })
}

fn parse_extern_fn(rtokens: &mut Tokens) -> Result<ExternFn> {
    let mut fn_token = expect_token(rtokens, TokenType::ExternFn, "extern fn");
    let mut signature = parse_signature(rtokens);
    resolve!(fn_token, signature);
    Ok(ExternFn { signature })
}

pub fn parse(mut tokens: Vec<Token>) -> Result<AST> {
    tokens.reverse();
    let mut rtokens = Tokens(tokens);
    let mut ast_res = vec![];
    // Every token
    loop {
        let t = match rtokens.last() {
            Some(t) => t,
            None => break,
        };
        let expect = expect_any!("global space", Some(t) => {
            // Parse a function
            Fn => ast_res.push(parse_fn(&mut rtokens).and_then(|f| Ok(ASTNode::Fn(f)))),
            ExternFn => ast_res.push(parse_extern_fn(&mut rtokens).and_then(|f| Ok(ASTNode::ExternFn(f)))),
            Newline => {rtokens.pop();}
            Tab => {rtokens.pop();}
        });
        match expect {
            Ok(()) => (),
            Err(e) => {
                // prevent infinite loop (read next token)
                rtokens.pop();
                ast_res.push(Err(e));
            }
        }
    }
    let mut ast = vec_to_res(ast_res);
    resolve!(ast);
    Ok(ast)
}

struct Tokens(Vec<Token>);
impl std::ops::Deref for Tokens {
    type Target = Vec<Token>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::ops::DerefMut for Tokens {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Tokens {
    fn n(&self, n: usize) -> Option<&Token> {
        if self.0.len() <= n {
            None
        } else {
            self.0.get(self.0.len() - n - 1)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    // This is NOT meant to test recursion, it's meant as a hello-world
    // that only uses function definition and calling, which means we have
    // no one to call but ourselves
    fn recurse() {
        use super::TokenType::*;
        let ast = dexpect(parse(
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
            .map(|t| Token { kind: t.clone(), span: Span::new() })
            .collect(),
        ));
        assert_eq!(
            ast,
            vec![ASTNode::Fn(crate::ast::Fn {
                signature: Signature {
                    id: Id::fake("main"),
                    parameters: vec![],
                    span: Span::new(),
                },
                statements: vec![Statement::FnCall(FnCall {
                    name: Id::fake("main"),
                    arguments: vec![],
                    span: Span::new(),
                })],
            })]
        );
    }
    #[test]
    fn parse_expression_rollback() {
        // The code is listed in order FnCall, then return
        // We need to test to make sure it can roll back properly
        use super::TokenType::*;
        let ast = dexpect(parse(
            vec![Fn, Identifier("main".to_string()), LParen, RParen, Newline, Tab, Return]
                .iter()
                .map(|t| Token { kind: t.clone(), span: Span::new() })
                .collect(),
        ));
        if let ASTNode::Fn(func) = &ast[0] {
            assert_eq!(func.statements, vec![Statement::Return(None),]);
        } else {
            unreachable!()
        }
    }
}
