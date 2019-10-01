// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{ast::*, llr, Type};
use indexmap::IndexMap;

// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(state: &mut LowerState, expr: &Expression) -> Type {
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::String(_) => Type::Str,
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
        },
        Expression::Identifier(id) => match stack_search(state, &id.name) {
            (_, t) => t,
        }
        Expression::Not(_) => Type::Bool,
        Expression::FnCall(func) => {
            let node = match state.fn_map.get(&*func.name) {
                Some(func) => func,
                None => panic!("could not find function {}", func.name),
            };
            let return_type = match node {
                ASTNode::Fn(f) => &f.signature.return_type,
                ASTNode::ExternFn(f) => &f.signature.return_type,
            };
            match return_type {
                Some(what) => *what,
                None => panic!("ERROR: function used as expression is void"),
            }
        }
        Expression::Binary(expr) => {
            use BinaryOp::*;
            match expr.op {
                Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual | Or | And => Type::Bool,
                // Retains type of arguments
                Plus | Minus | Times | Divide => {
                    let left = expression_type(state, &expr.left);
                    let right = expression_type(state, &expr.right);
                    assert_eq!(left, right, "Commutative operation between different types");
                    left
                }
            }
        }
    }
}

fn type_size(id_type: Type) -> u8 {
    use Type::*;
    match id_type {
        // TODO: 8-bit types? like bool?
        Int | Str | Bool => 32,
        Infer => panic!("type not yet inferred by size check"),
    }
}

// Some(true) is like (Int, Int) OR (Int, Infer)
// Some(false) is like (Int, String)
// None is (Infer, Infer)
fn types_match(a: Type, b: Type) -> Option<bool> {
    if a == Type::Infer && b == Type::Infer {
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
                ASTNode::Fn(func) => func.signature.name.clone(),
                ASTNode::ExternFn(_) => continue,
            };
            fn_map.insert(name, node);
        }
        // In order to keep numbers consistent, we keep externs after interns at all times
        for node in ast.iter() {
            let name = match node {
                ASTNode::Fn(_) => continue,
                ASTNode::ExternFn(func) => func.signature.name.clone(),
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
) -> Vec<llr::Instruction> {
    let mut insts = vec![];
    let begin = state.get_label();
    let end = state.get_label();
    insts.push(llr::Instruction::LabelMark(begin));
    insts.append(&mut expression_to_push(state, &loop_data.condition, 0));
    insts.push(llr::Instruction::JumpZero(end));
    lower_scope_begin(state);
        insts.append(&mut lower_statements(state, &loop_data.statements, parent_signature));
    // Immediately all go out of scope
    insts.append(&mut lower_scope_end(state));
    // Jump back to conditional, regardless
    // Lacking a Jump command, we push zero and then JumpZero
    insts.push(llr::Instruction::Push(0));
    insts.push(llr::Instruction::JumpZero(begin));
    insts.push(llr::Instruction::LabelMark(end));
    insts
}

/// stack_plus: Parsing dup requires knowing how much extra we've added to the stack
fn expression_to_push(
    state: &mut LowerState,
    expr: &Expression,
    stack_plus: u8,
) -> Vec<llr::Instruction> {
    let LowerState { strings, .. } = state;
    match expr {
        Expression::Literal(Literal::String(string)) => {
            strings.push(string.to_string());
            vec![llr::Instruction::Push((strings.len() - 1) as u32)]
        }
        Expression::Literal(Literal::Int(int)) => vec![llr::Instruction::Push(i_as_u(*int))],
        Expression::Literal(Literal::Bool(val)) => vec![llr::Instruction::Push(i_as_u(*val as i32))],
        Expression::Not(expr) => {
            let mut insts = vec![];
            // expr == false
            insts.append(&mut expression_to_push(state, expr, stack_plus));
            insts.push(llr::Instruction::Push(0));
            insts.push(llr::Instruction::Equal);
            insts
        }
        // fn call leaves result on the stack which is exactly what we need
        Expression::FnCall(call) => lower_fn_call(state, call, false),
        Expression::Identifier(var) => {
            let mut insts = vec![];
            let rindex = stack_index(state, &var.name) + stack_plus;
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
                    insts.append(&mut expression_to_push(state, &desugared, stack_plus));
                }
                // Right, left, op-to-follow
                // r l < == l r >
                Greater => {
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus));
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus+1));
                }
                // Left, right, op-to-follow
                _ => {
                    insts.append(&mut expression_to_push(state, &expr.left, stack_plus));
                    insts.append(&mut expression_to_push(state, &expr.right, stack_plus+1));
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
                            insts.append(&mut expression_to_push(state, &expr.left, stack_plus));
                            insts.append(&mut expression_to_push(state, &expr.right, stack_plus+1));
                        }
                        GreaterEqual => {
                            insts.append(&mut expression_to_push(state, &expr.right, stack_plus));
                            insts.append(&mut expression_to_push(state, &expr.left, stack_plus+1));
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
                        name: "internal_times".to_string(),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                    };
                    insts.append(&mut lower_fn_call(state, &call, false))
                }
                Or => insts.push(llr::Instruction::Add),
                And => {
                    // TODO: use multiply-generic? Or instruction?
                    let call = FnCall {
                        name: "internal_and".to_string(),
                        arguments: vec![expr.left.clone(), expr.right.clone()],
                    };
                    insts.append(&mut lower_fn_call(state, &call, false))
                }
                NotEqual => (),
                Divide => unimplemented!(),
            };
            insts
        }
    }
}

fn lower_panic(call: &FnCall) -> Vec<llr::Instruction> {
    let mut insts = vec![];
    let (line, col) = match (call.arguments[0].clone(), call.arguments[1].clone()) {
        (Expression::Literal(Literal::Int(l)), Expression::Literal(Literal::Int(c))) => (l,c),
        got => panic!("Panic expected line + col as Int, got {:?}", got),
    };
    insts.push(llr::Instruction::Panic(line as u32, col as u32));
    insts
}

fn lower_assert(state: &mut LowerState, call: &FnCall) -> Vec<llr::Instruction> {
    let condition = call.arguments[0].clone();
    let line = call.arguments[1].clone();
    let col = call.arguments[2].clone();
    // panic(line, col)
    let panic_statement = Statement::FnCall(FnCall {
        name: "panic".to_string(),
        arguments: vec![line, col],
    });
    // if !condition
    // 	panic(line, col)
    let desugared = Statement::If(If {
        condition: Expression::Not(Box::new(condition)),
        statements: vec![panic_statement],
        else_statements: vec![],
    });
    // Completely arbitrary, but lower_statement expects it in case of return
    let dummy_sig = Signature {
        name: "".to_string(),
        return_type: None,
        parameters: vec![],
    };
    lower_statement(state, &desugared, &dummy_sig)
}

fn lower_fn_call(
    state: &mut LowerState,
    call: &FnCall,
    is_statement: bool,
) -> Vec<llr::Instruction> {
    let (index, node) = match state.fn_map.get_full(&*call.name) {
        Some((i, _, func)) => (i, func),
        None => match &call.name[..] {
            "panic" => return lower_panic(call),
            "assert" => return lower_assert(state, call),
            _ => panic!("could not find function {}", call.name),
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
    assert_eq!(
        call.arguments.len(),
        params.len(),
        "{} expected {} arguments, got {}",
        call.name,
        params.len(),
        call.arguments.len()
    );
    let mut instructions = vec![];
    // Typecheck all arguments calls with their found IDs
    for (i, arg) in call.arguments.iter().enumerate() {
        let param = &params[i];
        let given_type = expression_type(state, arg);
        if types_match(given_type, param.id_type) == Some(false) {
            panic!("expected type {:?} but got {:?}", param.id_type, given_type);
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
    instructions.push(call);
    if is_statement {
        // Return value is unused if so it needs to be popped for balance
        // TODO: Support >32-bit returns
        if let Some(rt) = signature.return_type {
            match type_size(rt) {
                32 => instructions.push(llr::Instruction::Pop),
                _ => panic!("non-u32 return types unsupported"),
            }
        }
    }
    instructions
}

/// These must match up exactly!!! Except return, maybe that's different not sure
fn lower_scope_begin(state: &mut LowerState) {
    state.locals.push(IndexMap::new());
}
fn lower_scope_end(state: &mut LowerState) -> Vec<llr::Instruction> {
    // This pops every local
    // TODO: a proper stack machine will consume locals when last used
    // in an expression, which would make this obsolete
    // Actually i'm not sure that's true, what if final use is in if statement?
    // Something about single-assignment form
    let mut insts = vec![];
    for _local in state.locals.pop().unwrap() {
        debug!("{:?}", _local);
        insts.push(llr::Instruction::Pop);
    }
    insts
}

fn lower_return(
    state: &mut LowerState,
    expr: &Option<Expression>,
    signature: &Signature,
) -> Vec<llr::Instruction> {
    let num_locals = state.locals.last().unwrap().len();
    // Typecheck return value
    // None == None -> return == void
    assert_eq!(expr.as_ref().map(|x| expression_type(state, x)), signature.return_type);
    let mut insts = vec![];
    if let Some(expr) = expr {
        insts.append(&mut expression_to_push(state, &expr, 0));
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
    insts
}

fn stack_search(state: &mut LowerState, name: &str) -> (u8, Type) {
    // We use shadowing so search in opposite order
    let mut more_local_total = 0;
    for scope in state.locals.iter().rev() {
        match scope.get_full(name) {
            Some((i, _, id_type)) => {
                let i = more_local_total + scope.len() - i - 1;
                return (i as u8, *id_type);
            }
            None => {
                more_local_total += scope.len();
            },
        }
    }
    // None found
    panic!("unknown local variable {}", name);
}
fn stack_index(state: &mut LowerState, name: &str) -> u8 {
    let (i, _) = stack_search(state, name);
    i
}

fn lower_statement(
    state: &mut LowerState,
    statement: &Statement,
    signature: &Signature,
) -> Vec<llr::Instruction> {
    match statement {
        Statement::FnCall(call) => lower_fn_call(state, call, true),
        Statement::Return(expr) => lower_return(state, expr, signature),
        Statement::If(if_stmt) => {
            assert_eq!(expression_type(state, &if_stmt.condition), Type::Bool, "if statement requires bool");
            lower_scope_begin(state);
                let mut push_condition = expression_to_push(state, &if_stmt.condition, 0);
                let mut if_block = lower_statements(state, &if_stmt.statements, signature);
                let mut else_block = lower_statements(state, &if_stmt.else_statements, signature);
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
            lowered.append(&mut lower_scope_end(state));
            lowered
        }
        Statement::WhileLoop(loop_data) => lower_loop(state, loop_data, signature),
        Statement::Assignment(assign) => {
            let mut insts = vec![];
            // Compile rvalue first in case it depends on lvalue
            insts.append(&mut expression_to_push(state, &assign.rvalue, 0));
            // + 1 is to account for rvalue sitting on top
            let rindex = stack_index(state, &assign.lvalue) + 1;
            // Swap the old value to the top, new value is in old spot
            insts.push(llr::Instruction::Swap(rindex));
            // Pop old value off, never to be seen again
            insts.push(llr::Instruction::Pop);
            insts
        }
        Statement::Declaration(decl) => {
            // Declaration is just a push where we change locals
            let rvalue_type = expression_type(state, &decl.rvalue);
            let rv = expression_to_push(state, &decl.rvalue, 0);
            state.locals.last_mut().unwrap().insert(decl.lvalue.clone(), rvalue_type);
            rv
        }
    }
}

fn lower_statements(
    state: &mut LowerState,
    statements: &[Statement],
    parent_signature: &Signature,
) -> Vec<llr::Instruction> {
    let mut insts = vec![];
    // Lower every statement
    for statement in statements.iter() {
        insts.append(&mut lower_statement(state, statement, &parent_signature));
    }
    insts
}

fn lower_fn_statements(state: &mut LowerState, func: &Fn) -> Vec<llr::Instruction> {
    let mut instructions = Vec::<llr::Instruction>::new();
    instructions.append(&mut lower_statements(state, &func.statements, &func.signature));
    let last_statement_return = instructions.last() == Some(&llr::Instruction::Return);
    // Add implied returns
    // If the final command was a proper return, no need to clean it up
    if !last_statement_return {
        // If the function is empty or didn't end in return we need to add one
        // We can add the implicit void return but not implicit typed return
        // However the error will be handlede properly by lower_return by passing the signature
        instructions.append(&mut lower_return(state, &None, &func.signature));
    }
    instructions
}

fn lower_signature(signature: &Signature) -> llr::Signature {
    // Lower parameters
    let mut parameters = vec![];
    for param in &signature.parameters {
        parameters.push(param.id_type);
    }
    llr::Signature {
        name: signature.name.clone(),
        parameters,
        return_type: signature.return_type,
    }
}

fn lower_fn(state: &mut LowerState, func: &Fn) -> llr::Fn {
    lower_scope_begin(state);
        for param in &func.signature.parameters {
            state.locals.last_mut().unwrap().insert(param.name.clone(), param.id_type);
        }
        let rv = llr::Fn {
            instructions: lower_fn_statements(state, func),
            signature: lower_signature(&func.signature),
        };
    // fn return doesn't pop locals
    state.locals.pop();
    rv
}

pub fn lower(ast: AST) -> llr::LLR {
    let mut out = llr::LLR::new();
    let mut state = LowerState::new(&ast, &mut out.strings);
    // Find all function calls and set their ID to the map's id
    // (And also their instructions lol)
    for node in ast.iter() {
        match node {
            ASTNode::Fn(func) => {
                // We generate state for each function to keep locals scoped
                // This might be cleaner if we used a real scoping system
                let out_f = lower_fn(&mut state, &func);
                out.fns.push(out_f);
            }
            ASTNode::ExternFn(func) => {
                let out_s = lower_signature(&func.signature);
                out.extern_fns.push(out_s);
            }
        }
    }
    out
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
        let parsed = parse(lexed);
        let lowered = lower(parsed);
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
