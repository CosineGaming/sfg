// Most static analysis occurs here. Lower the AST which matches syntax into
// LLR which matches bytecode

use crate::{ast::*, llr, Type};
use indexmap::IndexMap;

static DEBUG: bool = true;

// As much as it pains me to require fn_map, we need it to determine type of FnCall
fn expression_type(state: &mut LowerState, expr: &Expression) -> Type {
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::String(_) => Type::Str,
            Literal::Int(_) => Type::Int,
        },
        Expression::Identifier(id) => match state.locals.get(&id.name) {
            Some(id_type) => *id_type,
            None => panic!("unknown local variable {}", id.name),
        },
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
            match expr.op {
                // TODO: Use Bool type when present
                BinaryOp::Equals => Type::Int,
                // Retains type of arguments
                BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide => {
                    let left = expression_type(state, &expr.left);
                    let right = expression_type(state, &expr.right);
                    assert_eq!(left, right, "Binary operation between different types");
                    left
                }
            }
        }
    }
}

fn type_size(id_type: Type) -> u8 {
    use Type::*;
    match id_type {
        Int | Str => 32,
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

/// requires mutable reference to llr's strings so strings that come up can be added
struct LowerState<'a> {
    fn_map: IndexMap<String, &'a ASTNode>,
    locals: IndexMap<String, Type>, // String / Index / Type
    strings: &'a mut Vec<String>,
    next_label: usize,
}
impl<'a> LowerState<'a> {
    fn new(ast: &'a AST, strings: &'a mut Vec<String>, next_label_global: usize) -> Self {
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
        Self { fn_map, locals: IndexMap::new(), strings: strings, next_label: next_label_global }
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
    insts.append(&mut lower_statements(state, &loop_data.statements, parent_signature));
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
        // fn call leaves result on the stack which is exactly what we need
        Expression::FnCall(call) => lower_fn_call(state, call, false),
        Expression::Identifier(var) => {
            let mut insts = vec![];
            let rindex = stack_index(state, &var.name) + stack_plus;
            insts.push(llr::Instruction::Dup(rindex));
            insts
        }
        Expression::Binary(expr) => {
            let mut insts = vec![];
            if expr.op != BinaryOp::Times {
                insts.append(&mut expression_to_push(state, &expr.left, 0));
                insts.append(&mut expression_to_push(state, &expr.right, 1));
            }
            match expr.op {
                BinaryOp::Equals => insts.push(llr::Instruction::Equals),
                BinaryOp::Plus => insts.push(llr::Instruction::Add),
                BinaryOp::Minus => insts.push(llr::Instruction::Sub),
                BinaryOp::Times => {
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
                BinaryOp::Divide => unimplemented!(),
            };
            insts
        }
    }
}

fn lower_panic(state: &mut LowerState, call: &FnCall) -> Vec<llr::Instruction> {
    let mut insts = vec![];
    for (i, arg) in call.arguments.iter().enumerate() {
        let given_type = expression_type(state, arg);
        if types_match(given_type, Type::Int) == Some(false) {
            panic!("expected type Int but got {:?}", given_type);
        }
        // TODO: storing the line/col in code rather than stack would be:
        // 1. Simpler to compile
        // 2. Safer (eg panic on stack overflow possible)
        // But requires rewriting assert in Rust as well
        // Also: this code is corrently fairly duplicated with lower_fn_call
        let mut push = expression_to_push(state, arg, i as u8);
        insts.append(&mut push);
    }
    insts.push(llr::Instruction::Panic);
    insts
}

fn lower_fn_call(
    state: &mut LowerState,
    call: &FnCall,
    is_statement: bool,
) -> Vec<llr::Instruction> {
    let (index, node) = match state.fn_map.get_full(&*call.name) {
        Some((i, _, func)) => (i, func),
        None => match &call.name[..] {
            "panic" => return lower_panic(state, call),
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
    let call = match is_extern {
        false => llr::Instruction::FnCall(fn_call),
        true => llr::Instruction::ExternFnCall(fn_call),
    };
    instructions.push(call);
    if is_statement {
        // Return value is unused if so it needs to be popped for balance
        // TODO: Support >32-bit returns
        match signature.return_type {
            Some(rt) => match type_size(rt) {
                32 => instructions.push(llr::Instruction::Pop),
                _ => panic!("non-u32 return types unsupported"),
            },
            None => (),
        }
    }
    instructions
}

fn lower_return(
    state: &mut LowerState,
    expr: &Option<Expression>,
    signature: &Signature,
) -> Vec<llr::Instruction> {
    let num_locals = state.locals.len();
    // Typecheck return value
    // None == None -> return == void
    assert_eq!(expr.as_ref().map(|x| expression_type(state, x)), signature.return_type);
    let mut insts = vec![];
    if let Some(expr) = expr {
        insts.append(&mut expression_to_push(state, &expr, 0));
        // We want to preserve value from coming pops by moving it to the bottom
        insts.push(llr::Instruction::Swap(num_locals as u8))
    }
    // This pops every local
    // TODO: a proper stack machine will consume locals when last used
    // in an expression, which would make this obsolete
    for _local in &state.locals {
        insts.push(llr::Instruction::Pop);
    }
    // Return only deals with the instruction pointer
    insts.push(llr::Instruction::Return);
    insts
}

fn stack_index(state: &mut LowerState, name: &str) -> u8 {
    match state.locals.get_full(name) {
        Some((i, _, _)) => (state.locals.len() - i - 1) as u8,
        None => panic!("unknown local variable {}", name),
    }
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
            let mut push_condition = expression_to_push(state, &if_stmt.condition, 0);
            let mut block = lower_statements(state, &if_stmt.statements, signature);
            let mut lowered = vec![];
            lowered.append(&mut push_condition);
            let end = state.get_label();
            lowered.push(llr::Instruction::JumpZero(end));
            lowered.append(&mut block);
            lowered.push(llr::Instruction::LabelMark(end));
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
            state.locals.insert(decl.lvalue.clone(), rvalue_type);
            expression_to_push(state, &decl.rvalue, 0)
        }
    }
}

fn lower_statements(
    state: &mut LowerState,
    statements: &Vec<Statement>,
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
    // We can't keep track of the stack perfectly, but we can assume
    // the stack is clean and then look at what we expect to be there
    //let mut locals = IndexMap::new();
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
        parameters: parameters,
        return_type: signature.return_type,
    }
}

fn lower_fn(state: &mut LowerState, func: &Fn) -> llr::Fn {
    for param in &func.signature.parameters {
        state.locals.insert(param.name.clone(), param.id_type);
    }
    llr::Fn {
        instructions: lower_fn_statements(state, func),
        signature: lower_signature(&func.signature),
    }
}

pub fn lower(ast: AST) -> llr::LLR {
    let mut out = llr::LLR::new();
    // We need to maintain globally unique state of next label
    let mut next_label_global = 0;
    // Find all function calls and set their ID to the map's id
    for node in ast.iter() {
        match node {
            ASTNode::Fn(func) => {
                // We generate state for each function to keep locals scoped
                // This might be cleaner if we used a real scoping system
                let mut state = LowerState::new(&ast, &mut out.strings, next_label_global);
                let out_f = lower_fn(&mut state, &func);
                out.fns.push(out_f);
                next_label_global = state.next_label;
            }
            ASTNode::ExternFn(func) => {
                let out_s = lower_signature(&func.signature);
                out.extern_fns.push(out_s);
            }
        }
    }
    if DEBUG {
        println!("{:#?}", out);
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
                    Instruction::Push(_) => balance += 8,
                    Instruction::Pop => balance -= 8,
                    _ => (),
                }
            }
        }
        assert_eq!(balance, 0);
    }
}
