// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{ast::*, llr, Type, Span};
use indexmap::IndexMap;

#[derive(Debug)]
pub enum LowerError {
	// TODO: figure out how to mark positions / spans in AST
    MismatchedType(Type, Span, Type, Span),
    //CannotInfer(Id), // TODO??? idk
    MismatchedReturn(Id, Option<Type>, Span),
    ArgumentCount(Id, usize, usize, Span),
    NonLiteral(&'static str, Span),
    UnknownFn(Id),
    UnknownIdent(Id),
}
impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use LowerError::*;
        match self {
            MismatchedType(a, a_s, b, b_s) => write!(f, "mismatched type, expected {} at {}, got {} at {}", a, a_s, b, b_s),
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
            ArgumentCount(id, expected, given, span) =>
            	write!(f, "function {} ({}) expected {} arguments, got {} at {}",
	            	id.name, id.span, expected, given, span),
            NonLiteral(name, span) => write!(f, "literal value required for {} at {}", name, span),
            UnknownFn(id) => write!(f, "called unknown function {} at {}", id.name, id.span),
            UnknownIdent(id) => write!(f, "referenced unknown identifier {} at {}", id.name, id.span),
        }
    }
}
// All relevant details in Display and Debug
impl std::error::Error for LowerError {}

type Result<T> = std::result::Result<T, LowerError>;

// A helper that doesn't require state useful for panic lower
fn literal_type(lit: &Literal) -> Type {
    match lit.data {
        LiteralData::String(_) => Type::Str,
        LiteralData::Int(_) => Type::Int,
        LiteralData::Bool(_) => Type::Bool,
    }
}
// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(state: &mut LowerState, expr: &Expression) -> Result<Type> {
    Ok(match expr {
		Expression::Literal(lit) => literal_type(lit),
        Expression::Identifier(id) => match stack_search(state, id)? {
            (_, t) => t,
        }
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
            match expr.op {
                Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual | Or | And => Type::Bool,
                // Retains type of arguments
                Plus | Minus | Times | Divide => {
                    let left = expression_type(state, &expr.left)?;
                    let right = expression_type(state, &expr.right)?;
                    if left != right {
	                    return Err(LowerError::MismatchedType(left, expr.left.full_span(), right, expr.right.full_span()));
                    }
                    left
                }
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
) -> Result<Vec<llr::Instruction>> {
    let mut insts = vec![];
    let begin = state.get_label();
    let end = state.get_label();
    insts.push(llr::Instruction::LabelMark(begin));
    insts.append(&mut expression_to_push(state, &loop_data.condition, 0)?);
    insts.push(llr::Instruction::JumpZero(end));
    lower_scope_begin(state);
        insts.append(&mut lower_statements(state, &loop_data.statements, parent_signature)?);
    // Immediately all go out of scope
    insts.append(&mut lower_scope_end(state)?);
    // Jump back to conditional, regardless
    // Lacking a Jump command, we push zero and then JumpZero
    insts.push(llr::Instruction::Push(0));
    insts.push(llr::Instruction::JumpZero(begin));
    insts.push(llr::Instruction::LabelMark(end));
    Ok(insts)
}

/// stack_plus: Parsing dup requires knowing how much extra we've added to the stack
fn expression_to_push(
    state: &mut LowerState,
    expr: &Expression,
    stack_plus: u8,
) -> Result<Vec<llr::Instruction>> {
    let LowerState { strings, .. } = state;
    Ok(match expr {
        Expression::Literal(lit) => match lit.data {
	        LiteralData::String(ref string) => {
	            strings.push(string.to_string());
	            vec![llr::Instruction::Push((strings.len() - 1) as u32)]
	        }
	        LiteralData::Int(int) => vec![llr::Instruction::Push(i_as_u(int))],
	        LiteralData::Bool(val) => vec![llr::Instruction::Push(i_as_u(val as i32))],
        }
        Expression::Not(expr) => {
            let mut insts = vec![];
            // expr == false
            insts.append(&mut expression_to_push(state, expr, stack_plus)?);
            insts.push(llr::Instruction::Push(0));
            insts.push(llr::Instruction::Equal);
            insts
        }
        // fn call leaves result on the stack which is exactly what we need
        Expression::FnCall(call) => lower_fn_call(state, call, false)?,
        Expression::Identifier(var) => {
            let mut insts = vec![];
            let rindex = stack_index(state, var)? + stack_plus;
            insts.push(llr::Instruction::Dup(rindex));
            insts
        }
        Expression::Binary(expr) => {
            use BinaryOp::*;
            let mut insts = vec![];
            // Special cases (most binary ops follow similar rules)
            match expr.op {
                Times | GreaterEqual | LessEqual | And => (),
                NotEqual => {
                    let mut as_equals = expr.clone();
                    as_equals.op = Equal;
                    let desugared = Expression::Not(Box::new(Expression::Binary(as_equals)));
                    insts.append(&mut expression_to_push(state, &desugared, stack_plus)?);
                }
                // Right, left, op-to-follow
                // r l < == l r >
                Greater => {
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus)?);
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus+1)?);
                }
                // Left, right, op-to-follow
                _ => {
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus)?);
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus+1)?);
                }
            }
            match expr.op {
                Equal => insts.push(llr::Instruction::Equal),
                // Arguments reversed previously
                Greater => insts.push(llr::Instruction::Less),
                Less => insts.push(llr::Instruction::Less),
                GreaterEqual | LessEqual => {
                    match expr.op {
                        LessEqual => {
                            insts.append(&mut expression_to_push(state, &expr.left, stack_plus)?);
                            insts.append(&mut expression_to_push(state, &expr.right, stack_plus+1)?);
                        }
                        GreaterEqual => {
                            insts.append(&mut expression_to_push(state, &expr.right, stack_plus)?);
                            insts.append(&mut expression_to_push(state, &expr.left, stack_plus+1)?);
                        }
                        _ => unreachable!(),
                    };
                    // Stack: l r
                    // (if > then it's r l but assume < for now)
                    // Duplicate left
                    insts.push(llr::Instruction::Dup(stack_plus+1));
                    // Stack: l r l
                    // Duplicate right (further forward now)
                    insts.push(llr::Instruction::Dup(stack_plus+1));
                    // Stack: l r l r
                    insts.push(llr::Instruction::Less);
                    // Stack: l r <
                    // Swap g to back
                    insts.push(llr::Instruction::Swap(stack_plus+2));
                    // Stack: > l r
                    insts.push(llr::Instruction::Equal);
                    // Stack: > =
                    // Or == Add
                    insts.push(llr::Instruction::Add);
                }
                Plus => insts.push(llr::Instruction::Add),
                Minus => insts.push(llr::Instruction::Sub),
                Times => {
                    // Translate 4*5 to internal_times(4,5)
                    // This might be cleaner in a "sugar" / parser-side change
                    // TODO: obviously lacking Times instruction is slow af
                    // Also we could ditch the lower_fn_call and just add a FnCall
                    // instruction and then we could skip the push conditional up above
                    let call = FnCall {
                        name: Id::fake("internal_times"),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: expr.span,
                    };
                    insts.append(&mut lower_fn_call(state, &call, false)?)
                }
                Or => insts.push(llr::Instruction::Add),
                And => {
                    // TODO: use multiply-generic? Or instruction?
                    let call = FnCall {
                        name: Id::fake("internal_and"),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                        span: Span::new(),
                    };
                    insts.append(&mut lower_fn_call(state, &call, false)?)
                }
                NotEqual => (),
                Divide => unimplemented!(),
            };
            insts
        }
    })
}

fn read_literal_int(expr: Expression) -> Result<u32> {
	match expr {
		Expression::Literal(ref lit) => match lit.data {
			LiteralData::Int(r) => Ok(r as u32),
			_ => Err(LowerError::MismatchedType(Type::Int, Span::new(), literal_type(&lit), expr.full_span())),
		}
		_ => Err(LowerError::NonLiteral("panic", expr.full_span()))
	}
}

fn lower_panic(call: &FnCall) -> Result<Vec<llr::Instruction>> {
    let mut insts = vec![];
    let line = read_literal_int(call.arguments[0].clone())?;
    let col  = read_literal_int(call.arguments[1].clone())?;
    insts.push(llr::Instruction::Panic(line as u32, col as u32));
    Ok(insts)
}

fn lower_assert(state: &mut LowerState, call: &FnCall) -> Result<Vec<llr::Instruction>> {
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
    let dummy_sig = Signature {
        id: Id::fake(""),
        parameters: vec![],
        span: call.span,
    };
    lower_statement(state, &desugared, &dummy_sig)
}

fn lower_fn_call(
	state: &mut LowerState,
	call: &FnCall,
	is_statement: bool)
-> Result<Vec<llr::Instruction>> {
    let (index, node) = match state.fn_map.get_full(&call.name.name) {
        Some((i, _, func)) => (i, func),
        None => match &call.name.name[..] {
            "panic" => return lower_panic(call),
            "assert" => return lower_assert(state, call),
            _ => return Err(LowerError::UnknownFn(call.name.clone())),
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
	    return Err(LowerError::ArgumentCount(signature.id.clone(), params.len(), call.arguments.len(), call.span));
    }
    let mut instructions = vec![];
    // Typecheck all arguments calls with their found IDs
    for (i, arg) in call.arguments.iter().enumerate() {
        let param = &params[i];
        let given_type = expression_type(state, arg)?;
        if types_match(Some(given_type), param.id_type) == Some(false) {
            return Err(LowerError::MismatchedType(
	            given_type,
	            signature.span,
	            param.id_type.expect("type definitely given for mismatch"),
	            arg.full_span()));
        }
        // Otherwise our types are just fine
        // Now we just have to evaluate it
        // The number of arguments we've pushed already is i which is also stack_plus
        let mut push = expression_to_push(state, arg, i as u8)?;
        instructions.append(&mut push);
    }
    // Generate lowered call
    let fn_call = llr::FnCall { index, arg_count: call.arguments.len() as u8 };
    let call = if is_extern {
        llr::Instruction::ExternFnCall(fn_call)
    } else {
        llr::Instruction::FnCall(fn_call)
    };
    instructions.push(call);
    if is_statement {
        // Return value is unused if so it needs to be popped for balance
        if signature.id.id_type.is_some() {
            instructions.push(llr::Instruction::Pop);
        }
    }
    Ok(instructions)
}

/// These must match up exactly!!! Except return, maybe that's different not sure
fn lower_scope_begin(state: &mut LowerState) {
    state.locals.push(IndexMap::new());
}
fn lower_scope_end(state: &mut LowerState) -> Result<Vec<llr::Instruction>> {
    // This pops every local
    // a proper stack machine will consume locals when last used
    // in an expression, which would make this obsolete
    // Actually i'm not sure that's true, what if final use is in if statement?
    // Something about single-assignment form
    let mut insts = vec![];
    for _local in state.locals.pop().unwrap() {
        debug!("{:?}", _local);
        insts.push(llr::Instruction::Pop);
    }
    Ok(insts)
}

fn lower_return(
    state: &mut LowerState,
    expr: &Option<Expression>,
    signature: &Signature,
) -> Result<Vec<llr::Instruction>> {
    let num_locals = state.locals.last().unwrap().len();
    // Typecheck return value
    // None == None -> return == void
    match (signature.id.id_type, expr) {
	    (Some(_), None) => return Err(LowerError::MismatchedReturn(signature.id.clone(), None, Span::new())),
	    (a, Some(b)) => {
		    let b_type = expression_type(state, b)?;
		    if a != Some(b_type) {
			    return Err(LowerError::MismatchedReturn(signature.id.clone(), Some(b_type), b.full_span()));
		    }
		    // otherwise we're good
	    }
	    (None, None) => ()
    }
    let mut insts = vec![];
    if let Some(expr) = expr {
        insts.append(&mut expression_to_push(state, &expr, 0)?);
        // We want to preserve value from coming pops by moving it to the bottom
        insts.push(llr::Instruction::Swap(num_locals as u8))
    }
    // Return kills all scopes down to function
    // CHECK: when we implement globals, this'll have to have -1 trickery
    for scope in &state.locals {
        for _local in scope {
            insts.push(llr::Instruction::Pop);
        }
    }
    // We DON'T pop the *internal state scopes* because return may be mid-function
    // (non-lexical). Instead the popping occurs at the end of the function
    // lowering
    // Return only deals with the instruction pointer
    insts.push(llr::Instruction::Return);
    Ok(insts)
}

fn stack_search(state: &mut LowerState, name: &Id) -> Result<(u8, Type)> {
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
            },
        }
    }
    // None found
    Err(LowerError::UnknownIdent(name.clone()))
}
fn stack_index(state: &mut LowerState, name: &Id) -> Result<u8> {
    let (i, _) = stack_search(state, name)?;
    Ok(i)
}

fn lower_statement(
    state: &mut LowerState,
    statement: &Statement,
    signature: &Signature,
) -> Result<Vec<llr::Instruction>> {
    Ok(match statement {
        Statement::FnCall(call) => lower_fn_call(state, call, true)?,
        Statement::Return(expr) => lower_return(state, expr, signature)?,
        Statement::If(if_stmt) => {
	        let cond_type = expression_type(state, &if_stmt.condition)?;
	        if cond_type != Type::Bool {
		        return Err(LowerError::MismatchedType(Type::Bool, signature.span, cond_type, if_stmt.condition.full_span()));
	        }
            lower_scope_begin(state);
                let mut push_condition = expression_to_push(state, &if_stmt.condition, 0)?;
                let mut if_block = lower_statements(state, &if_stmt.statements, signature)?;
                let mut else_block = lower_statements(state, &if_stmt.else_statements, signature)?;
                let mut lowered = vec![];
                lowered.append(&mut push_condition);
                let else_start = state.get_label();
                lowered.push(llr::Instruction::JumpZero(else_start));
                lowered.append(&mut if_block);
                // CHECK: does creating a label you might not use, fuck things up? So far, no
                let else_end = state.get_label();
                // Don't bother with jump if no statements in else
                if !if_stmt.else_statements.is_empty() {
                    // if we executed if, don't execute else (jump to end of else)
                    // TODO: unconditional jump, rather than push zero jmp0
                    lowered.push(llr::Instruction::Push(0));
                    lowered.push(llr::Instruction::JumpZero(else_end));
                }
                lowered.push(llr::Instruction::LabelMark(else_start));
                if !if_stmt.else_statements.is_empty() {
                    lowered.append(&mut else_block);
                    lowered.push(llr::Instruction::LabelMark(else_end));
                }
            lowered.append(&mut lower_scope_end(state)?);
            lowered
        }
        Statement::WhileLoop(loop_data) => lower_loop(state, loop_data, signature)?,
        Statement::Assignment(assign) => {
            let mut insts = vec![];
            // Compile rvalue first in case it depends on lvalue
            insts.append(&mut expression_to_push(state, &assign.rvalue, 0)?);
            // + 1 is to account for rvalue sitting on top
            let rindex = stack_index(state, &assign.lvalue)? + 1;
            // Swap the old value to the top, new value is in old spot
            insts.push(llr::Instruction::Swap(rindex));
            // Pop old value off, never to be seen again
            insts.push(llr::Instruction::Pop);
            insts
        }
        Statement::Declaration(decl) => {
            // Declaration is just a push where we change locals
            let rvalue_type = expression_type(state, &decl.rvalue)?;
            let rv = expression_to_push(state, &decl.rvalue, 0)?;
            state.locals.last_mut().unwrap().insert(decl.lvalue.name.clone(), rvalue_type);
            rv
        }
    })
}

fn lower_statements(
    state: &mut LowerState,
    statements: &[Statement],
    parent_signature: &Signature,
) -> Result<Vec<llr::Instruction>> {
    let mut insts = vec![];
    // Lower every statement
    for statement in statements.iter() {
        insts.append(&mut lower_statement(state, statement, &parent_signature)?);
    }
    Ok(insts)
}

fn lower_fn_statements(state: &mut LowerState, func: &Fn) -> Result<Vec<llr::Instruction>> {
    let mut instructions = Vec::<llr::Instruction>::new();
    instructions.append(&mut lower_statements(state, &func.statements, &func.signature)?);
    let last_statement_return = instructions.last() == Some(&llr::Instruction::Return);
    // Add implied returns
    // If the final command was a proper return, no need to clean it up
    if !last_statement_return {
        // If the function is empty or didn't end in return we need to add one
        // We can add the implicit void return but not implicit typed return
        // However the error will be handlede properly by lower_return by passing the signature
        instructions.append(&mut lower_return(state, &None, &func.signature)?);
    }
    Ok(instructions)
}

fn lower_signature(signature: &Signature) -> Result<llr::Signature> {
    // Lower parameters
    let mut parameters = vec![];
    for param in &signature.parameters {
        parameters.push(param.id_type.expect("untyped parameter let through parser"));
    }
    Ok(llr::Signature {
        name: signature.id.name.clone(),
        parameters,
        return_type: signature.id.id_type,
    })
}

fn lower_fn(state: &mut LowerState, func: &Fn) -> Result<llr::Fn> {
    lower_scope_begin(state);
        for param in &func.signature.parameters {
            state.locals.last_mut().unwrap().insert(param.name.clone(), param.id_type.expect("untyped parameter let through parser"));
        }
        let rv = llr::Fn {
            instructions: lower_fn_statements(state, func)?,
            signature: lower_signature(&func.signature)?,
        };
    // fn return doesn't pop locals
    state.locals.pop();
    Ok(rv)
}

pub fn lower(ast: AST) -> Result<llr::LLR> {
    let mut out = llr::LLR::new();
    let mut state = LowerState::new(&ast, &mut out.strings);
    // Find all function calls and set their ID to the map's id
    // (And also their instructions lol)
    for node in ast.iter() {
        match node {
            ASTNode::Fn(func) => {
                // We generate state for each function to keep locals scoped
                // This might be cleaner if we used a real scoping system
                let out_f = lower_fn(&mut state, &func)?;
                out.fns.push(out_f);
            }
            ASTNode::ExternFn(func) => {
                let out_s = lower_signature(&func.signature)?;
                out.extern_fns.push(out_s);
            }
        }
    }
    Ok(out)
}

mod test {
    #[test]
    fn non_branching_stack_balance() {
        use super::lower;
        use crate::llr::Instruction;
        use crate::{lexer::lex, parser::parse};
        let script_string = std::fs::read_to_string("tests/scripts/non-branching.sfg")
            .expect("could not load given file");
        let lexed = lex(&script_string);
        let parsed = parse(lexed).map_err(|e| { println!("{}", e); panic!() }).unwrap();
        let lowered = lower(parsed).map_err(|e| { println!("{}", e); panic!() }).unwrap();
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
