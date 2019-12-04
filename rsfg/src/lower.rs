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
    Shadow(Id),
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
            Shadow(id) => {
                write!(f, "shadowing is illegal within the same scope with {} at {}", id.name, id.span)
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

macro_rules! tryv {
    ( $state:expr, $what:expr ) => {
        match $what {
            Ok(o) => o,
            Err(e) => return InstResults::from_res($state, Err(e)),
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
        Expression::Not(of) => {
            match expression_type(state, of) {
                Ok(Type::Bool) => Type::Bool,
                Ok(what) => return Err(LowerError::MismatchedType(Type::Bool, of.full_span(), what, of.full_span())),
                Err(e) => return Err(e),
            }
        }
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
    /// stack_length: Parsing dup requires knowing how much extra we've added to the stack
    stack_length: u8,
    /// stop checks and codegen when in error
    error_state: bool,
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
        Self {
            fn_map,
            locals: vec![],
            strings,
            next_label: 0,
            stack_length: 0,
            error_state: false,
        }
    }
    fn get_label(&mut self) -> usize {
        self.next_label += 1;
        self.next_label
    }
}
type UnsafeInstResults = Vec<std::result::Result<llr::Instruction, LowerError>>;
// A regular vec except it keeps track of stack safety
#[derive(Default)]
struct InstResults(UnsafeInstResults);
impl std::ops::Deref for InstResults {
    type Target = UnsafeInstResults;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::ops::DerefMut for InstResults {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl InstResults {
    fn push(&mut self, state: &mut LowerState, inst: OneResult<llr::Instruction>) {
        // obviously we don't use _ here because any new instruction added
        // has to be classified here
        let d: i8 = match inst {
            Ok(o) => inst_stack(o),
            Err(_) => {
                state.error_state = true; // if it's an error we're kinda boned
                0
            }
        };
        state.stack_length = (state.stack_length as i8 + d) as u8;
        self.0.push(inst);
    }
    fn push_unsafe(&mut self, inst: OneResult<llr::Instruction>) {
        debug!("warning: pushing unsafe");
        self.0.push(inst);
    }
    fn from_vec(state: &mut LowerState, nonsafe: UnsafeInstResults) -> Self {
        let mut s = Self::default();
        for res in nonsafe {
            s.push(state, res);
        }
        s
    }
    fn from_vec_unsafe(nonsafe: UnsafeInstResults) -> Self {
        debug!("warning: pushing unsafe vec");
        Self(nonsafe)
    }
    fn from_res(state: &mut LowerState, res: OneResult<llr::Instruction>) -> Self {
        Self::from_vec(state, vec![res])
    }
    fn from_inst(state: &mut LowerState, inst: llr::Instruction) -> Self {
        Self::from_res(state, Ok(inst))
    }
}
impl From<LowerError> for InstResults {
    fn from(e: LowerError) -> Self {
        Self(vec![Err(e)])
    }
}

fn inst_stack(i: llr::Instruction) -> i8 {
    use llr::Instruction::*;
    match i {
        | Push(_)
        | Dup(_)
            => 1,
        | FnCall(_) // ??? push happens before so.... but return???
        | ExternFnCall(_)
        | Swap(_)
        | Return // ???
        | Panic(_, _)
        | LabelMark(_)
            => 0,
        | Pop
        | Equal
        | Less
        | JumpZero(_)
        | Add
        | Sub
        | Mul
        | Div
        | FAdd
        | FSub
        | FLess
        | FMul
        | FDiv
            => -1,
    }
}

fn lower_loop(
    state: &mut LowerState,
    loop_data: &WhileLoop,
    parent_signature: &Signature,
) -> InstResults {
    let mut insts = InstResults::default();
    let begin = state.get_label();
    let end = state.get_label();
    insts.push(state, Ok(llr::Instruction::LabelMark(begin)));
    // TODO: allow multiple errors / avoid early return / recover
    insts.append(&mut expression_to_push(state, &loop_data.condition));
    insts.push(state, Ok(llr::Instruction::JumpZero(end)));
    lower_scope_begin(state);
    {
        insts.append(&mut lower_statements(state, &loop_data.statements, parent_signature));
        // Immediately all go out of scope
    }
    insts.append(&mut lower_scope_end(state));
    // Jump back to conditional, regardless
    // Lacking a Jump command, we push zero and then JumpZero
    insts.push(state, Ok(llr::Instruction::Push(0)));
    insts.push(state, Ok(llr::Instruction::JumpZero(begin)));
    insts.push(state, Ok(llr::Instruction::LabelMark(end)));
    insts
}

fn expression_to_push(state: &mut LowerState, expression: &Expression) -> InstResults {
    #[cfg(debug_assertions)]
    let old_plus = state.stack_length;
    let LowerState { strings, .. } = state;
    let insts = match expression {
        Expression::Literal(lit) => match lit.data {
            LiteralData::String(ref string) => {
                strings.push(string.clone());
                let string_id = strings.len() - 1;
                InstResults::from_inst(state, llr::Instruction::Push(string_id as u32))
            }
            LiteralData::Int(int) => InstResults::from_inst(state, llr::Instruction::Push(i_as_u(int))),
            LiteralData::Bool(val) => InstResults::from_inst(state, llr::Instruction::Push(i_as_u(val as i32))),
            LiteralData::Float(val) => InstResults::from_inst(state, llr::Instruction::Push(f_as_u(val))),
        },
        Expression::Not(expr) => {
            let mut insts = InstResults::default();
            // expr == false
            insts.append(&mut expression_to_push(state, expr));
            insts.push(state, Ok(llr::Instruction::Push(0)));
            insts.push(state, Ok(llr::Instruction::Equal));
            insts
        }
        // fn call leaves result on the stack which is exactly what we need
        Expression::FnCall(call) => lower_fn_call(state, call, false),
        Expression::Identifier(var) => {
            let mut insts = InstResults::default();
            let rindex =
                stack_index(state, var).and_then(|i| Ok(llr::Instruction::Dup(i)));
            insts.push(state, rindex);
            insts
        }
        Expression::Binary(expr) => {
            use BinaryOp::*;
            let mut insts = InstResults::default();
            let left_type = tryv!(state, expression_type(state, &expr.left));
            // Special cases (most binary ops follow similar rules)
            match expr.op {
                GreaterEqual | LessEqual | And => (),
                NotEqual => {
                    let mut as_equals = expr.clone();
                    as_equals.op = Equal;
                    let desugared = Expression::Not(Box::new(Expression::Binary(as_equals)));
                    insts.append(&mut expression_to_push(state, &desugared));
                }
                Equal if Type::Float == left_type => {
                    let desugared = Expression::FnCall(FnCall {
                        name: Id { name: "epsilon_eq".to_string(), id_type: None, span: expr.span },
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    });
                    insts.append(&mut expression_to_push(state, &desugared));
                }
                // Right, left, op-to-follow
                // r l < == l r >
                Greater => {
                    insts.append(&mut expression_to_push(state, &expr.right));
                    insts.append(&mut expression_to_push(state, &expr.left));
                }
                // Left, right, op-to-follow
                _ => {
                    insts.append(&mut expression_to_push(state, &expr.left));
                    insts.append(&mut expression_to_push(state, &expr.right));
                }
            }
            let type_error = Err(LowerError::NoOperation(expr.op.clone(), left_type, expr.span));
            match expr.op {
                Equal => match left_type {
                    Type::Int | Type::Bool => insts.push(state, Ok(llr::Instruction::Equal)),
                    Type::Float => (),
                    _ => insts.push(state, type_error),
                },
                // Arguments reversed previously
                Greater => insts.push(state, Ok(llr::Instruction::Less)),
                Less => insts.push(state, Ok(llr::Instruction::Less)),
                GreaterEqual | LessEqual => {
                    match expr.op {
                        LessEqual => {
                            insts.append(&mut expression_to_push(state, &expr.left));
                            insts.append(&mut expression_to_push(
                                state,
                                &expr.right,
                            ));
                        }
                        GreaterEqual => {
                            insts.append(&mut expression_to_push(state, &expr.right));
                            insts.append(&mut expression_to_push(
                                state,
                                &expr.left,
                            ));
                        }
                        _ => unreachable!(),
                    };
                    // Stack: l r
                    // (if > then it's r l but assume < for now)
                    // Duplicate left
                    insts.push(state, Ok(llr::Instruction::Dup(1)));
                    // Stack: l r l
                    // Duplicate right (further forward now)
                    insts.push(state, Ok(llr::Instruction::Dup(1)));
                    // Stack: l r l r
                    insts.push(state, Ok(llr::Instruction::Less));
                    // Stack: l r <
                    // Swap g to back
                    insts.push(state, Ok(llr::Instruction::Swap(2)));
                    // Stack: > l r
                    insts.push(state, Ok(llr::Instruction::Equal));
                    // Stack: > =
                    // Or == Add
                    insts.push(state, Ok(llr::Instruction::Add));
                }
                Plus => match left_type {
                    Type::Int => insts.push(state, Ok(llr::Instruction::Add)),
                    Type::Float => insts.push(state, Ok(llr::Instruction::FAdd)),
                    _ => insts.push(state, type_error),
                },
                Minus => match left_type {
                    Type::Int => insts.push(state, Ok(llr::Instruction::Sub)),
                    Type::Float => insts.push(state, Ok(llr::Instruction::FSub)),
                    _ => insts.push(state, type_error),
                },
                Times => match left_type {
                    Type::Int => insts.push(state, Ok(llr::Instruction::Mul)),
                    Type::Float => insts.push(state, Ok(llr::Instruction::FMul)),
                    _ => insts.push(state, type_error),
                },
                Divide => match left_type {
                    Type::Int => insts.push(state, Ok(llr::Instruction::Div)),
                    Type::Float => insts.push(state, Ok(llr::Instruction::FDiv)),
                    _ => insts.push(state, type_error),
                },
                Or => insts.push(state, Ok(llr::Instruction::Add)),
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
            };
            insts
        }
    };
    if !state.error_state {
        #[cfg(debug_assertions)]
        debug_assert_eq!(state.stack_length - 1, old_plus);
    }
    insts
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

fn lower_panic(state: &mut LowerState, call: &FnCall) -> InstResults {
    let mut insts = InstResults::default();
    let line = read_literal_int(call.arguments[0].clone()).unwrap_or_else(|e| {
        // push error give garbage
        insts.push(state, Err(e));
        0
    });
    let col = read_literal_int(call.arguments[1].clone()).unwrap_or_else(|e| {
        // push error give garbage
        insts.push(state, Err(e));
        0
    });
    insts.push(state, Ok(llr::Instruction::Panic(line, col)));
    insts
}

fn lower_assert(state: &mut LowerState, call: &FnCall) -> InstResults {
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

fn lower_fn_call(state: &mut LowerState, call: &FnCall, is_statement: bool) -> InstResults {
    let (index, node) = match state.fn_map.get_full(&call.name.name) {
        Some((i, _, func)) => (i, func),
        None => match &call.name.name[..] {
            "panic" => return lower_panic(state, call),
            "assert" => return lower_assert(state, call),
            _ => return InstResults::from(LowerError::UnknownFn(call.name.clone())),
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
        return InstResults::from(LowerError::ArgumentCount(
            signature.id.clone(),
            params.len(),
            call.arguments.len(),
            call.span,
        ));
    }
    let mut insts = InstResults::default();
    // Typecheck all arguments calls with their found IDs
    for (i, arg) in call.arguments.iter().enumerate() {
        let param = &params[i];
        let type_r = expression_type(state, arg);
        match type_r {
            Ok(given_type) => {
                if types_match(Some(given_type), param.id_type) == Some(false) {
                    insts.push(state, Err(LowerError::MismatchedType(
                        param.id_type.expect("type definitely given for mismatch"),
                        param.span,
                        given_type,
                        arg.full_span(),
                    )));
                }
            }
            Err(e) => insts.push(state, Err(e)),
        }
        // Otherwise our types are just fine
        // Now we just have to evaluate it
        // The number of arguments we've pushed already is i which is also stack_length
        let mut push = expression_to_push(state, arg);
        insts.append(&mut push);
    }
    if !state.error_state {
        // to simulate the function's return so we just the stack pluses
        state.stack_length -= call.arguments.len() as u8;
    }
    // Generate lowered call
    let fn_call = llr::FnCall { index, arg_count: call.arguments.len() as u8 };
    let call = if is_extern {
        llr::Instruction::ExternFnCall(fn_call)
    } else {
        llr::Instruction::FnCall(fn_call)
    };
    insts.push(state, Ok(call));
    if signature.id.id_type.is_some() {
        // Return value is unused if so it needs to be popped for balance
        if is_statement {
            // this has to be unsafe because we never noted the return push
            insts.push_unsafe(Ok(llr::Instruction::Pop));
        } else {
            // We have created a push in return but that's in fncall so not recorded yet
            state.stack_length += 1;
        }
    }
    insts
}

/// These must match up exactly!!! Except return, maybe that's different not sure
fn lower_scope_begin(state: &mut LowerState) {
    state.locals.push(IndexMap::new());
}
fn lower_scope_end(state: &mut LowerState) -> InstResults {
    // This pops every local
    // a proper stack machine will consume locals when last used
    // in an expression, which would make this obsolete
    // Actually i'm not sure that's true, what if final use is in if statement?
    // Something about single-assignment form
    let mut insts = InstResults::default();
    for _local in state.locals.pop().unwrap() {
        debug!("{:?}", _local);
        insts.push(state, Ok(llr::Instruction::Pop));
    }
    insts
}

fn lower_return(
    state: &mut LowerState,
    expr: &Option<Expression>,
    signature: &Signature,
) -> InstResults {
    let num_locals = state.locals.iter().fold(0, |c, l| c + l.len());
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
        insts.append(&mut expression_to_push(state, &expr));
        // the stack_length would be incremented by this push, but because
        // it's already been processed by fn_call in context, to best continue
        // forward we undo the stack plus
        state.stack_length -= 1;
        // We want to preserve value from coming pops by moving it to the bottom
        insts.push(Ok(llr::Instruction::Swap(num_locals as u8)))
    }
    // Return kills all scopes down thru function
    // CHECK: when we implement globals, this'll have to have -1 trickery
    // We could use lower_scope_end but it would delete the IndexMap entry(?)
    for scope in &state.locals {
        for _ in scope {
            insts.push(Ok(llr::Instruction::Pop));
        }
    }
    // We DON'T pop the *internal state scopes* because return may be mid-function
    // (non-lexical). Instead the popping occurs at the end of the function
    // lowering
    // Return only deals with the instruction pointer
    insts.push(Ok(llr::Instruction::Return));
    // returns have some fancy shenanigans going on, we can't use InstResults's safety
    debug!("POST RETURN STACK LEN {}", state.stack_length);
    InstResults::from_vec_unsafe(insts)
}

fn stack_search(state: &mut LowerState, name: &Id) -> OneResult<(u8, Type)> {
    let mut lower_scopes = 0;
    // First we find the forward index before finding the reverse one
    // TODO: maybe this means we should be using the forward index in Dup anyway? probly not
    // Assume not found until proven otherwise
    let mut forward = Err(LowerError::UnknownIdent(name.clone()));
    for scope in state.locals.iter() {
        if let Some((i, _, id_type)) = scope.get_full(&name.name) {
            forward = Ok(((lower_scopes + i) as u8, *id_type));
            // We use shadowing so keep searching for a closer one
        } // otherwise keep searching
        // track the lower scopes so when we get the final scope result we can get a total
        lower_scopes += scope.len();
    }
    if state.error_state {
        // avoid underflow issue
        return forward;
    }
    forward.and_then(|(i, t)| Ok((
        // now we subtract the known stack length minus the forward index for the reverse index
        // minus one because stack is length, index is index
        state.stack_length - i - 1, t)))
}
fn stack_index(state: &mut LowerState, name: &Id) -> OneResult<u8> {
    let (i, _) = stack_search(state, name)?;
    Ok(i)
}

fn lower_statement(
    state: &mut LowerState,
    statement: &Statement,
    signature: &Signature,
) -> InstResults {
    match statement {
        Statement::FnCall(call) => lower_fn_call(state, call, true),
        Statement::Return(expr) => lower_return(state, expr, signature),
        Statement::If(if_stmt) => {
            let cond_type = expression_type(state, &if_stmt.condition);
            let mut insts = InstResults::default();
            match cond_type {
                Ok(cond_type) => {
                    if cond_type != Type::Bool {
                        insts.push(state, Err(LowerError::MismatchedType(
                            Type::Bool,
                            signature.span,
                            cond_type,
                            if_stmt.condition.full_span(),
                        )));
                    }
                }
                Err(e) => insts.push(state, Err(e)),
            }
            // N.B. storing instructions considered harmful, believe it or not (scope issues possible)
            // CONDITION
            insts.append(&mut expression_to_push(state, &if_stmt.condition));
            let else_start = state.get_label();
            insts.push(state, Ok(llr::Instruction::JumpZero(else_start)));
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
                insts.push(state, Ok(llr::Instruction::Push(0)));
                insts.push(state, Ok(llr::Instruction::JumpZero(else_end)));
            }
            insts.push(state, Ok(llr::Instruction::LabelMark(else_start)));
            if !if_stmt.else_statements.is_empty() {
                // ELSE BLOCK
                lower_scope_begin(state);
                {
                    insts.append(&mut lower_statements(state, &if_stmt.else_statements, signature));
                }
                insts.append(&mut lower_scope_end(state));
                insts.push(state, Ok(llr::Instruction::LabelMark(else_end)));
            }
            insts
        }
        Statement::WhileLoop(loop_data) => lower_loop(state, loop_data, signature),
        Statement::Assignment(assign) => {
            let mut insts = InstResults::default();
            // Compile rvalue first in case it depends on lvalue
            insts.append(&mut expression_to_push(state, &assign.rvalue));
            let swap = stack_index(state, &assign.lvalue)
                // Swap the old value to the top, new value is in old spot
                .and_then(|i| Ok(llr::Instruction::Swap(i)));
            insts.push(state, swap);
            // Pop old value off, never to be seen again
            insts.push(state, Ok(llr::Instruction::Pop));
            insts
        }
        Statement::Declaration(decl) => {
            // Declaration is just a push where we change locals
            let mut insts = InstResults::default();
            // have to change locals AFTER push ofc
            match expression_type(state, &decl.rvalue) {
                Ok(o) => {
                    let mut rv = expression_to_push(state, &decl.rvalue);
                    insts.append(&mut rv);
                    // shadows within same scope are illegal
                    // TODO: make shadows within any scope illegal(?)
                    // best recovery for illegal shadow is to still insert new type
                    // because most likely cause is "i thought they were legal"
                    // so do what they expect
                    if let Some(_) = state.locals.last_mut().unwrap().insert(decl.lvalue.name.clone(), o) {
                        // TODO wouldn't it be nice if IndexMap held spans
                        // so we could indicate who shadows it
                        insts.push(state, Err(LowerError::Shadow(decl.lvalue.clone())));
                    }
                }
                Err(e) => insts.push(state, Err(e)),
            }
            insts
        }
    }
}

fn lower_statements(
    state: &mut LowerState,
    statements: &[Statement],
    parent_signature: &Signature,
) -> InstResults {
    let mut insts = InstResults::default();
    // Lower every statement
    for statement in statements.iter() {
        insts.append(&mut lower_statement(state, statement, &parent_signature));
    }
    insts
}

fn lower_fn_statements(state: &mut LowerState, func: &Fn) -> InstResults {
    let mut insts = InstResults::default();
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
        // However the error will be handled properly by lower_return by passing the signature
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
            // simulate the pushing that would have occurred in FnCall
            state.stack_length += 1;
        }
        let rv = llr::Fn {
            instructions: vec_to_res(lower_fn_statements(state, func).0)?,
            signature: lower_signature(&func.signature),
        };
        // fn return doesn't handle stack_length correctly because it can
        // happen multiple times
        // neither does local popping / scope end for same reason
        let locals = state.locals.pop().unwrap();
        if !state.error_state {
            debug_assert_eq!(state.stack_length, locals.len() as u8);
        }
        state.stack_length = 0;
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
    out.fns = match vec_errs_to_res(fns) {
        Ok(o) => o,
        Err(mut e) => {
            e.dedup();
            return Err(e);
        }
    };
    out.extern_fns = externs;
    assert!(!state.error_state, "if in error state error should've been reported");
    // here for a borrowck fanciness
    out.strings = strings;
    debug!("{}", out);
    Ok(out)
}

#[cfg(test)]
mod test {
    use crate::fmt_vec;
    #[test]
    fn no_returns_stack_balance() {
        use super::{lower, inst_stack};
        use crate::{lexer::lex, parser::parse};
        let script_string = std::fs::read_to_string("tests/scripts/no-returns.sfg")
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
        let mut balance: i8 = 0;
        for func in fns {
            for inst in func.instructions {
                balance += inst_stack(inst);
            }
        }
        assert_eq!(balance, 0);
    }
}
