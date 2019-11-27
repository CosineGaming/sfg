// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{ast::*, llr, vec_errs_to_res, Span, Type};
use indexmap::IndexMap;

#[derive(Debug, PartialEq)]
pub enum LowerError {
    // TODO: figure out how to mark positions / spans in AST
    MismatchedType(Type, Span, Type, Span),
    //CannotInfer(Id), // TODO??? idk
    MismatchedReturn(Id, Option<Type>, Span),
    ArgumentCount(Id, usize, usize, Span),
    NonLiteral(&'static str, Span),
    UnknownFn(Id),
    UnknownIdent(Id),
    NoOperation(BinaryOp, Type, Span),
}
impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use LowerError::*;
        match self {
            MismatchedType(a, a_s, b, b_s) => {
                write!(f, "mismatched type, expected {} at {}, got {} at {}", a, a_s, b, b_s)
            }
            MismatchedReturn(needed, id_type, span) => {
                write!(f, "{} expected ", needed.name)?;
                match needed.id_type {
                    Some(t) => write!(f, "return with type {}", t)?,
                    None => write!(f, "void or no return")?,
                };
                write!(f, " (defined at {}), got ", needed.span)?;
                match id_type {
                    Some(t) => write!(f, "explicit return with type {}", t)?,
                    None => write!(f, "void or no return")?,
                };
                write!(f, " at {}", span)
            }
            ArgumentCount(id, expected, given, span) => write!(
                f,
                "function {} ({}) expected {} arguments, got {} at {}",
                id.name, id.span, expected, given, span
            ),
            NonLiteral(name, span) => write!(f, "literal value required for {} at {}", name, span),
            UnknownFn(id) => write!(f, "called unknown function {} at {}", id.name, id.span),
            UnknownIdent(id) => {
                write!(f, "referenced unknown identifier {} at {}", id.name, id.span)
            }
            NoOperation(op, on_type, span) => {
                write!(f, "no operation {:?} for {} at {}", op, on_type, span)
            }
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for LowerError {}

fn vec_to_res<T, E>(vec: Vec<std::result::Result<T, E>>) -> std::result::Result<Vec<T>, Vec<E>> {
    let mut oks = vec![];
    let mut errs = vec![];
    for entry in vec {
        match entry {
            Ok(o) => oks.push(o),
            Err(e) => errs.push(e),
        }
    }
    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(oks)
    }
}

type Result<T> = std::result::Result<T, Vec<LowerError>>;
type OneResult<T> = std::result::Result<T, LowerError>;
type InstResult = Vec<std::result::Result<llr::Instruction, LowerError>>;

macro_rules! tryv {
    ( $what:expr ) => {
        match $what {
            Ok(o) => o,
            Err(e) => return vec![Err(e)],
        }
    };
}

// A helper that doesn't require state useful for panic lower
fn literal_type(lit: &Literal) -> Type {
    match lit.data {
        LiteralData::String(_) => Type::Str,
        LiteralData::Int(_) => Type::Int,
        LiteralData::Bool(_) => Type::Bool,
        LiteralData::Float(_) => Type::Float,
    }
}
// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(state: &mut LowerState, expr: &Expression) -> OneResult<Type> {
    Ok(match expr {
        Expression::Literal(lit) => literal_type(lit),
        Expression::Identifier(id) => match stack_search(state, id)? {
            (_, t) => t,
        },
        Expression::Not(_) => Type::Bool,
        Expression::FnCall(func) => {
            let node = match state.fn_map.get(&func.name.name) {
                Some(func) => func,
                None => return Err(LowerError::UnknownFn(func.name.clone())),
            };
            let return_type = match node {
                ASTNode::Fn(f) => &f.signature.id.id_type,
                ASTNode::ExternFn(f) => &f.signature.id.id_type,
            };
            match return_type {
                Some(what) => *what,
                None => unreachable!("function lowered as expression is void"),
            }
        }
        Expression::Binary(expr) => {
            use BinaryOp::*;
            let left = expression_type(state, &expr.left)?;
            let right = expression_type(state, &expr.right)?;
            if left != right {
                return Err(LowerError::MismatchedType(
                    left,
                    expr.left.full_span(),
                    right,
                    expr.right.full_span(),
                ));
            }
            let fail = Err(LowerError::NoOperation(expr.op.clone(), left, expr.span));
            match left {
                Type::Bool => match expr.op {
                    And | Or | Equal | NotEqual => Type::Bool,
                    _ => return fail,
                },
                Type::Int => match expr.op {
                    Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual => Type::Bool,
                    Plus | Minus | Times | Divide => left,
                    And | Or => return fail,
                },
                Type::Float => match expr.op {
                    Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual => Type::Bool,
                    Plus | Minus => left,
                    // TODO: add greater / less
                    Times | Divide => return fail,
                    And | Or => return fail,
                },
                Type::Str => return fail,
            }
        }
    })
}

// Some(true) is like (Int, Int) OR (Int, Infer)
// Some(false) is like (Int, String)
// None is (Infer, Infer)
fn types_match(a: Option<Type>, b: Option<Type>) -> Option<bool> {
    if a == None && b == None {
        None
    } else if a == b {
        Some(true)
    } else {
        Some(false)
    }
}

fn i_as_u(what: i32) -> u32 {
    unsafe { std::mem::transmute::<i32, u32>(what) }
}
fn f_as_u(what: f32) -> u32 {
    unsafe { std::mem::transmute::<f32, u32>(what) }
}

type ScopeStack = Vec<IndexMap<String, Type>>;

/// requires mutable reference to llr's strings so strings that come up can be added
struct LowerState<'a> {
    fn_map: IndexMap<String, &'a ASTNode>,
    locals: ScopeStack, // String / Index / Type
    strings: &'a mut Vec<String>,
    next_label: usize,
}
impl<'a> LowerState<'a> {
    #[allow(clippy::ptr_arg)]
    fn new(ast: &'a AST, strings: &'a mut Vec<String>) -> Self {
        // IndexMap maintains indices of fns
        let mut fn_map = IndexMap::new();
        // Add all functions to the map
        for node in ast.iter() {
            let name = match node {
                ASTNode::Fn(func) => func.signature.id.name.clone(),
                ASTNode::ExternFn(_) => continue,
            };
            fn_map.insert(name, node);
        }
        // In order to keep numbers consistent, we keep externs after interns at all times
        for node in ast.iter() {
            let name = match node {
                ASTNode::Fn(_) => continue,
                ASTNode::ExternFn(func) => func.signature.id.name.clone(),
            };
            fn_map.insert(name, node);
        }
        Self { fn_map, locals: vec![], strings, next_label: 0 }
    }
    fn get_label(&mut self) -> usize {
        self.next_label += 1;
        self.next_label
    }
}

fn lower_loop(
    state: &mut LowerState,
    loop_data: &WhileLoop,
    parent_signature: &Signature,
) -> InstResult {
    let mut insts = vec![];
    let begin = state.get_label();
    let end = state.get_label();
    insts.push(Ok(llr::Instruction::LabelMark(begin)));
    // TODO: allow multiple errors / avoid early return / recover
    insts.append(&mut expression_to_push(state, &loop_data.condition, 0));
    insts.push(Ok(llr::Instruction::JumpZero(end)));
    lower_scope_begin(state);
    {
        insts.append(&mut lower_statements(state, &loop_data.statements, parent_signature));
        // Immediately all go out of scope
    }
    insts.append(&mut lower_scope_end(state));
    // Jump back to conditional, regardless
    // Lacking a Jump command, we push zero and then JumpZero
    insts.push(Ok(llr::Instruction::Push(0)));
    insts.push(Ok(llr::Instruction::JumpZero(begin)));
    insts.push(Ok(llr::Instruction::LabelMark(end)));
    insts
}

/// stack_plus: Parsing dup requires knowing how much extra we've added to the stack
fn expression_to_push(
    state: &mut LowerState,
    expression: &Expression,
    stack_plus: u8,
) -> InstResult {
    let LowerState { strings, .. } = state;
    match expression {
        Expression::Literal(lit) => match lit.data {
            LiteralData::String(ref string) => {
                strings.push(string.to_string());
                vec![Ok(llr::Instruction::Push((strings.len() - 1) as u32))]
            }
            LiteralData::Int(int) => vec![Ok(llr::Instruction::Push(i_as_u(int)))],
            LiteralData::Bool(val) => vec![Ok(llr::Instruction::Push(i_as_u(val as i32)))],
            LiteralData::Float(val) => vec![Ok(llr::Instruction::Push(f_as_u(val)))],
        },
        Expression::Not(expr) => {
            let mut insts = vec![];
            // expr == false
            insts.append(&mut expression_to_push(state, expr, stack_plus));
            insts.push(Ok(llr::Instruction::Push(0)));
            insts.push(Ok(llr::Instruction::Equal));
            insts
        }
        // fn call leaves result on the stack which is exactly what we need
        Expression::FnCall(call) => lower_fn_call(state, call, false),
        Expression::Identifier(var) => {
            let mut insts = vec![];
            let rindex =
                stack_index(state, var).and_then(|i| Ok(llr::Instruction::Dup(i + stack_plus)));
            debug!("stack_plus {} i {:?} for var {}", stack_plus, rindex, var.name);
            insts.push(rindex);
            insts
        }
        Expression::Binary(expr) => {
            use BinaryOp::*;
            let mut insts = vec![];
            let left_type = tryv!(expression_type(state, &expr.left));
            // Special cases (most binary ops follow similar rules)
            match expr.op {
                Times | GreaterEqual | LessEqual | And | Divide => (),
                NotEqual => {
                    let mut as_equals = expr.clone();
                    as_equals.op = Equal;
                    let desugared = Expression::Not(Box::new(Expression::Binary(as_equals)));
                    insts.append(&mut expression_to_push(state, &desugared, stack_plus));
                }
                Equal if Type::Float == left_type => {
                    let desugared = Expression::FnCall(FnCall {
                        name: Id { name: "epsilon_eq".to_string(), id_type: None, span: expr.span },
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    });
                    insts.append(&mut expression_to_push(state, &desugared, stack_plus));
                }
                // Right, left, op-to-follow
                // r l < == l r >
                Greater => {
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus));
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus + 1));
                }
                // Left, right, op-to-follow
                _ => {
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus));
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus + 1));
                }
            }
            let type_error = Err(LowerError::NoOperation(expr.op.clone(), left_type, expr.span));
            match expr.op {
                Equal => match left_type {
                    Type::Int | Type::Bool => insts.push(Ok(llr::Instruction::Equal)),
                    Type::Float => (),
                    _ => insts.push(type_error),
                },
                // Arguments reversed previously
                Greater => insts.push(Ok(llr::Instruction::Less)),
                Less => insts.push(Ok(llr::Instruction::Less)),
                GreaterEqual | LessEqual => {
                    match expr.op {
                        LessEqual => {
                            insts.append(&mut expression_to_push(state, &expr.left, stack_plus));
                            insts.append(&mut expression_to_push(
                                state,
                                &expr.right,
                                stack_plus + 1,
                            ));
                        }
                        GreaterEqual => {
                            insts.append(&mut expression_to_push(state, &expr.right, stack_plus));
                            insts.append(&mut expression_to_push(
                                state,
                                &expr.left,
                                stack_plus + 1,
                            ));
                        }
                        _ => unreachable!(),
                    };
                    // Stack: l r
                    // (if > then it's r l but assume < for now)
                    // Duplicate left
                    insts.push(Ok(llr::Instruction::Dup(stack_plus + 1)));
                    // Stack: l r l
                    // Duplicate right (further forward now)
                    insts.push(Ok(llr::Instruction::Dup(stack_plus + 1)));
                    // Stack: l r l r
                    insts.push(Ok(llr::Instruction::Less));
                    // Stack: l r <
                    // Swap g to back
                    insts.push(Ok(llr::Instruction::Swap(stack_plus + 2)));
                    // Stack: > l r
                    insts.push(Ok(llr::Instruction::Equal));
                    // Stack: > =
                    // Or == Add
                    insts.push(Ok(llr::Instruction::Add));
                }
                Plus => match left_type {
                    Type::Int => insts.push(Ok(llr::Instruction::Add)),
                    Type::Float => insts.push(Ok(llr::Instruction::FAdd)),
                    _ => insts.push(type_error),
                },
                Minus => match left_type {
                    Type::Int => insts.push(Ok(llr::Instruction::Sub)),
                    Type::Float => insts.push(Ok(llr::Instruction::FSub)),
                    _ => insts.push(type_error),
                },
                Times => {
                    // Translate 4*5 to _times(4,5)
                    // This might be cleaner in a "sugar" / parser-side change
                    // TODO: obviously lacking Times instruction is slow af
                    // Also we could ditch the lower_fn_call and just add a FnCall
                    // instruction and then we could skip the push conditional up above
                    let call = FnCall {
                        name: Id::fake("_times"),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    };
                    insts.append(&mut lower_fn_call(state, &call, false))
                }
                Or => insts.push(Ok(llr::Instruction::Add)),
                And => {
                    // TODO: use multiply-generic? Or instruction?
                    let call = FnCall {
                        name: Id::fake("_and"),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    };
                    insts.append(&mut lower_fn_call(state, &call, false))
                }
                NotEqual => (),
                Divide => {
                    // Translate 10/2 to _divide(10, 2)
                    // TODO: implement real divide (?)
                    let call = FnCall {
                        name: Id::fake("_divide"),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    };
                    insts.append(&mut lower_fn_call(state, &call, false))
                }
            };
            insts
        }
    }
}

fn read_literal_int(expr: Expression) -> OneResult<u32> {
    match expr {
        Expression::Literal(ref lit) => match lit.data {
            LiteralData::Int(r) => Ok(r as u32),
            _ => Err(LowerError::MismatchedType(
                Type::Int,
                Span::new(),
                literal_type(&lit),
                expr.full_span(),
            )),
        },
        _ => Err(LowerError::NonLiteral("panic", expr.full_span())),
    }
}

fn lower_panic(call: &FnCall) -> InstResult {
    let mut insts = vec![];
    let line = read_literal_int(call.arguments[0].clone()).unwrap_or_else(|e| {
        // push error give garbage
        insts.push(Err(e));
        0
    });
    let col = read_literal_int(call.arguments[1].clone()).unwrap_or_else(|e| {
        // push error give garbage
        insts.push(Err(e));
        0
    });
    insts.push(Ok(llr::Instruction::Panic(line, col)));
    insts
}

fn lower_assert(state: &mut LowerState, call: &FnCall) -> InstResult {
    let condition = call.arguments[0].clone();
    let line = call.arguments[1].clone();
    let col = call.arguments[2].clone();
    // panic(line, col)
    let panic_statement = Statement::FnCall(FnCall {
        name: Id::fake("panic"),
        arguments: vec![line, col],
        span: call.span,
    });
    // if !condition
    // 	panic(line, col)
    let desugared = Statement::If(If {
        condition: Expression::Not(Box::new(condition)),
        statements: vec![panic_statement],
        else_statements: vec![],
        span: call.span,
    });
    // Completely arbitrary, but lower_statement expects it in case of return
    let dummy_sig = Signature { id: Id::fake(""), parameters: vec![], span: call.span };
    lower_statement(state, &desugared, &dummy_sig)
}

fn lower_fn_call(state: &mut LowerState, call: &FnCall, is_statement: bool) -> InstResult {
    let (index, node) = match state.fn_map.get_full(&call.name.name) {
        Some((i, _, func)) => (i, func),
        None => match &call.name.name[..] {
            "panic" => return lower_panic(call),
            "assert" => return lower_assert(state, call),
            _ => return vec![Err(LowerError::UnknownFn(call.name.clone()))],
        },
    };
    // Typecheck
    // Sig needed for later op
    let is_extern;
    let signature = match node {
        ASTNode::Fn(f) => {
            is_extern = false;
            &f.signature
        }
        ASTNode::ExternFn(f) => {
            is_extern = true;
            &f.signature
        }
    };
    let params = &signature.parameters;
    if call.arguments.len() != params.len() {
        return vec![Err(LowerError::ArgumentCount(
            signature.id.clone(),
            params.len(),
            call.arguments.len(),
            call.span,
        ))];
    }
    let mut instructions = vec![];
    // Typecheck all arguments calls with their found IDs
    for (i, arg) in call.arguments.iter().enumerate() {
        let param = &params[i];
        let type_r = expression_type(state, arg);
        match type_r {
            Ok(given_type) => {
                if types_match(Some(given_type), param.id_type) == Some(false) {
                    instructions.push(Err(LowerError::MismatchedType(
                        param.id_type.expect("type definitely given for mismatch"),
                        param.span,
                        given_type,
                        arg.full_span(),
                    )));
                }
            }
            Err(e) => instructions.push(Err(e)),
        }
        // Otherwise our types are just fine
        // Now we just have to evaluate it
        // The number of arguments we've pushed already is i which is also stack_plus
        let mut push = expression_to_push(state, arg, i as u8);
        instructions.append(&mut push);
    }
    // Generate lowered call
    let fn_call = llr::FnCall { index, arg_count: call.arguments.len() as u8 };
    let call = if is_extern {
        llr::Instruction::ExternFnCall(fn_call)
    } else {
        llr::Instruction::FnCall(fn_call)
    };
    instructions.push(Ok(call));
    if is_statement {
        // Return value is unused if so it needs to be popped for balance
        if signature.id.id_type.is_some() {
            instructions.push(Ok(llr::Instruction::Pop));
        }
    }
    instructions
}

/// These must match up exactly!!! Except return, maybe that's different not sure
fn lower_scope_begin(state: &mut LowerState) {
    state.locals.push(IndexMap::new());
}
fn lower_scope_end(state: &mut LowerState) -> InstResult {
    // This pops every local
    // a proper stack machine will consume locals when last used
    // in an expression, which would make this obsolete
    // Actually i'm not sure that's true, what if final use is in if statement?
    // Something about single-assignment form
    let mut insts = vec![];
    for _local in state.locals.pop().unwrap() {
        debug!("{:?}", _local);
        insts.push(Ok(llr::Instruction::Pop));
    }
    insts
}

fn lower_return(
    state: &mut LowerState,
    expr: &Option<Expression>,
    signature: &Signature,
) -> InstResult {
    let num_locals = state.locals.last().unwrap().len();
    let mut insts = vec![];
    // Typecheck return value
    // None == None -> return == void
    match (signature.id.id_type, expr) {
        (Some(_), None) => {
            insts.push(Err(LowerError::MismatchedReturn(signature.id.clone(), None, Span::new())))
        }
        (a, Some(b)) => match expression_type(state, b) {
            Ok(b_type) => {
                if a != Some(b_type) {
                    insts.push(Err(LowerError::MismatchedReturn(
                        signature.id.clone(),
                        Some(b_type),
                        b.full_span(),
                    )));
                }
            }
            Err(e) => insts.push(Err(e)),
        },
        (None, None) => (),
    }
    if let Some(expr) = expr {
        insts.append(&mut expression_to_push(state, &expr, 0));
        // We want to preserve value from coming pops by moving it to the bottom
        insts.push(Ok(llr::Instruction::Swap(num_locals as u8)))
    }
    // Return kills all scopes down to function
    // CHECK: when we implement globals, this'll have to have -1 trickery
    for scope in &state.locals {
        for _local in scope {
            insts.push(Ok(llr::Instruction::Pop));
        }
    }
    // We DON'T pop the *internal state scopes* because return may be mid-function
    // (non-lexical). Instead the popping occurs at the end of the function
    // lowering
    // Return only deals with the instruction pointer
    insts.push(Ok(llr::Instruction::Return));
    insts
}

fn stack_search(state: &mut LowerState, name: &Id) -> OneResult<(u8, Type)> {
    // We use shadowing so search in opposite order
    let mut more_local_total = 0;
    for scope in state.locals.iter().rev() {
        match scope.get_full(&name.name) {
            Some((i, _, id_type)) => {
                let i = more_local_total + scope.len() - i - 1;
                return Ok((i as u8, *id_type));
            }
            None => {
                more_local_total += scope.len();
            }
        }
    }
    // None found
    Err(LowerError::UnknownIdent(name.clone()))
}
fn stack_index(state: &mut LowerState, name: &Id) -> OneResult<u8> {
    let (i, _) = stack_search(state, name)?;
    Ok(i)
}

fn lower_statement(
    state: &mut LowerState,
    statement: &Statement,
    signature: &Signature,
) -> InstResult {
    match statement {
        Statement::FnCall(call) => lower_fn_call(state, call, true),
        Statement::Return(expr) => lower_return(state, expr, signature),
        Statement::If(if_stmt) => {
            let cond_type = expression_type(state, &if_stmt.condition);
            let mut insts = vec![];
            match cond_type {
                Ok(cond_type) => {
                    if cond_type != Type::Bool {
                        insts.push(Err(LowerError::MismatchedType(
                            Type::Bool,
                            signature.span,
                            cond_type,
                            if_stmt.condition.full_span(),
                        )));
                    }
                }
                Err(e) => insts.push(Err(e)),
            }
            // N.B. storing instructions considered harmful, believe it or not (scope issues possible)
            // CONDITION
            insts.append(&mut expression_to_push(state, &if_stmt.condition, 0));
            let else_start = state.get_label();
            insts.push(Ok(llr::Instruction::JumpZero(else_start)));
            // IF BLOCK
            lower_scope_begin(state);
            {
                insts.append(&mut lower_statements(state, &if_stmt.statements, signature));
            }
            insts.append(&mut lower_scope_end(state));
            // CHECK: does creating a label you might not use, fuck things up? So far, no
            let else_end = state.get_label();
            // Don't bother with jump if no statements in else
            if !if_stmt.else_statements.is_empty() {
                // if we executed if, don't execute else (jump to end of else)
                // TODO: unconditional jump, rather than push zero jmp0
                insts.push(Ok(llr::Instruction::Push(0)));
                insts.push(Ok(llr::Instruction::JumpZero(else_end)));
            }
            insts.push(Ok(llr::Instruction::LabelMark(else_start)));
            if !if_stmt.else_statements.is_empty() {
                // ELSE BLOCK
                lower_scope_begin(state);
                {
                    insts.append(&mut lower_statements(state, &if_stmt.else_statements, signature));
                }
                insts.append(&mut lower_scope_end(state));
                insts.push(Ok(llr::Instruction::LabelMark(else_end)));
            }
            insts
        }
        Statement::WhileLoop(loop_data) => lower_loop(state, loop_data, signature),
        Statement::Assignment(assign) => {
            let mut insts = vec![];
            // Compile rvalue first in case it depends on lvalue
            insts.append(&mut expression_to_push(state, &assign.rvalue, 0));
            insts.push(
                stack_index(state, &assign.lvalue)
                    // Swap the old value to the top, new value is in old spot
                    // + 1 is to account for rvalue sitting on top
                    .and_then(|i| Ok(llr::Instruction::Swap(i + 1))),
            );
            // Pop old value off, never to be seen again
            insts.push(Ok(llr::Instruction::Pop));
            insts
        }
        Statement::Declaration(decl) => {
            // Declaration is just a push where we change locals
            let mut insts = vec![];
            let mut rv = expression_to_push(state, &decl.rvalue, 0);
            insts.append(&mut rv);
            // have to change locals AFTER push ofc
            match expression_type(state, &decl.rvalue) {
                Ok(o) => {
                    state.locals.last_mut().unwrap().insert(decl.lvalue.name.clone(), o);
                }
                Err(e) => insts.push(Err(e)),
            }
            insts
        }
    }
}

fn lower_statements(
    state: &mut LowerState,
    statements: &[Statement],
    parent_signature: &Signature,
) -> InstResult {
    let mut insts = vec![];
    // Lower every statement
    for statement in statements.iter() {
        insts.append(&mut lower_statement(state, statement, &parent_signature));
    }
    insts
}

fn lower_fn_statements(state: &mut LowerState, func: &Fn) -> InstResult {
    let mut insts = vec![];
    insts.append(&mut lower_statements(state, &func.statements, &func.signature));
    let last_statement_return = match insts.last() {
        Some(Ok(inst)) => inst == &llr::Instruction::Return,
        None => false,   // no last statement, isn't return
        Some(_) => true, // error, give true for easier recovery
    };
    // Add implied returns
    // If the final command was a proper return, no need to clean it up
    if !last_statement_return {
        // If the function is empty or didn't end in return we need to add one
        // We can add the implicit void return but not implicit typed return
        // However the error will be handlede properly by lower_return by passing the signature
        insts.append(&mut lower_return(state, &None, &func.signature));
    }
    insts
}

fn lower_signature(signature: &Signature) -> llr::Signature {
    // Lower parameters
    let mut parameters = vec![];
    for param in &signature.parameters {
        parameters.push(param.id_type.expect("untyped parameter let through parser"));
    }
    llr::Signature {
        name: signature.id.name.clone(),
        parameters,
        return_type: signature.id.id_type,
    }
}

fn lower_fn(state: &mut LowerState, func: &Fn) -> Result<llr::Fn> {
    lower_scope_begin(state);
    {
        for param in &func.signature.parameters {
            state.locals.last_mut().unwrap().insert(
                param.name.clone(),
                param.id_type.expect("untyped parameter let through parser"),
            );
        }
        let rv = llr::Fn {
            instructions: vec_to_res(lower_fn_statements(state, func))?,
            signature: lower_signature(&func.signature),
        };
        // fn return doesn't pop locals
        state.locals.pop();
        Ok(rv)
    } // note missing scope end (must be careful about returns)
}

pub fn lower(ast: AST) -> Result<llr::LLR> {
    let mut strings = vec![];
    let mut state = LowerState::new(&ast, &mut strings);
    let mut fns = vec![];
    let mut externs = vec![];
    // Find all function calls and set their ID to the map's id
    // (And also their instructions lol)
    for node in ast.iter() {
        match node {
            ASTNode::Fn(func) => fns.push(lower_fn(&mut state, &func)),
            ASTNode::ExternFn(func) => externs.push(lower_signature(&func.signature)),
        }
    }
    let mut out = llr::LLR::new();
    out.strings = strings;
    out.fns = match vec_errs_to_res(fns) {
        Ok(o) => o,
        Err(mut e) => {
            e.dedup();
            return Err(e);
        }
    };
    out.extern_fns = externs;
    debug!("{}", out);
    Ok(out)
}

#[cfg(test)]
mod test {
    use crate::fmt_vec;
    #[test]
    fn non_branching_stack_balance() {
        use super::lower;
        use crate::llr::Instruction;
        use crate::{lexer::lex, parser::parse};
        let script_string = std::fs::read_to_string("tests/scripts/non-branching.sfg")
            .expect("could not load given file");
        let lexed = lex(&script_string);
        let parsed = parse(lexed)
            .map_err(|e| {
                println!("{}", fmt_vec(&e));
                panic!()
            })
            .unwrap();
        let lowered = lower(parsed)
            .map_err(|e| {
                println!("{}", fmt_vec(&e));
                panic!()
            })
            .unwrap();
        let fns = lowered.fns;
        let mut balance: isize = 0;
        for func in fns {
            for inst in func.instructions {
                match inst {
                    Instruction::Push(_) => balance += 1,
                    Instruction::Pop => balance -= 1,
                    _ => (),
                }
            }
        }
        assert_eq!(balance, 0);
    }
}
