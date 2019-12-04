use crate::read::*;
use crate::sfg_std;

use indexmap::IndexMap;

/// Should be small enough to make small scripts low-RAM, but high enough
/// that startup doesn't take forever with 1000s of incremental allocs
const INIT_STACK_SIZE: usize = 32;
/// Similarly chosen for the expected call stack size
const INIT_CALL_STACK_SIZE: usize = 6;
/// This is mostly arbitrary but keeps us from unrecoverable OOM error on
/// incorrect recursion
/// 256MB is excessively large but within the realms of normal operation
/// /4 means gives twice on 64-bit
const CALL_STACK_MAX_SIZE: usize = 256*1024*1024/4;

#[derive(PartialEq, Debug)]
pub struct Thread {
    pub stack: Vec<i32>,
    /// The call stack is managed by the VM, containing calls only
    /// It's kept separate as opposed to machine architectures because
    /// the use of the data stack is made harder by combining them.
    /// Each usize is a ip
    pub call_stack: Vec<usize>,
    code: Vec<u8>,
    // Code pointer
    ip: usize,
    pub strings: Vec<String>,
    fns: IndexMap<String, Fn>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Fn {
    // ip will be 0 for externs (TODO: make this safer)
    ip: usize,
    return_type: Option<Type>,
    parameters: Vec<Type>,
}
impl Fn {
    pub fn new(ip: usize, return_type: Option<Type>, parameters: Vec<Type>) -> Self {
        Self {
            ip,
            return_type,
            parameters,
        }
    }
}

fn i_as_f(what: i32) -> f32 {
    f32::from_bits(what as u32)
}
fn f_as_i(what: f32) -> i32 {
    f32::to_bits(what) as i32
}
fn pop_f(stack: &mut Vec<i32>) -> f32 {
	i_as_f(stack.pop().unwrap())
}
fn push_f(stack: &mut Vec<i32>, f: f32) {
	stack.push(f_as_i(f))
}

macro_rules! thread_assert {
    ( $thread:ident, $cond:expr, $lit:literal $(,)? $($expr:expr),* ) => {
        if !$cond {
            $thread.thread_panic(&format!($lit, $($expr),*))
        }
    }
}

impl std::fmt::Display for Thread {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for stack in vec![&vec![self.ip], &self.call_stack] {
            for ip in stack {
                for (name, func) in &self.fns {
                    let begin = &func.ip;
                    if ip > begin {
                        writeln!(f, "{} ({})", name, begin)?;
                        break;
                    }
                }
            }
        }
        Ok(())
    }
}

impl Thread {
    fn thread_panic(&self, msg: &str) -> ! {
        println!("vm panic: {}\nBACKTRACE:\n{}", msg, self);
        panic!("vm panic");
    }
    pub fn new(code: Vec<u8>) -> Self {
        let mut ip = 0;
        if next(&code, &mut ip) != b'b'
            || next(&code, &mut ip) != b'c'
            || next(&code, &mut ip) != b'f'
            || next(&code, &mut ip) != b'g'
        {
            panic!("expected bcfg");
        }
        let mut fns = IndexMap::new();
        let mut strings = Vec::new();
        while let Some(Deser::FnHeader) = deser(code[ip]) {
            ip += 1;
            let (name, func) = read_fn_header(&code, &mut ip, false);
            fns.insert(name, func);
        }
        while let Some(Deser::ExternFnHeader) = deser(code[ip]) {
            ip += 1;
            let (name, func) = read_fn_header(&code, &mut ip, true);
            fns.insert(name, func);
        }
        while let Some(Deser::StringLit) = deser(code[ip]) {
            ip += 1;
            let string = read_string(&code, &mut ip);
            strings.push(string);
        }
        Self {
            stack: Vec::with_capacity(INIT_STACK_SIZE),
            call_stack: Vec::with_capacity(INIT_CALL_STACK_SIZE),
            code,
            ip,
            strings,
            fns,
        }
    }
    /// Returns whether a return has been called requiring exit
    fn exec_next(&mut self) -> bool {
        // The stack is a rust Vec. rust vecs don't deallocate
        // until asked to, so we don't have to worry about pop then
        // push being slow. If we're worried, we can shrink_to at the
        // end of each instruction
        match deser_strong(next(&self.code, &mut self.ip)) {
            Deser::Push => {
                self.stack.push(read_i32(&self.code, &mut self.ip));
            }
            Deser::Pop => {
                self.stack.pop();
            }
            Deser::ExternFnCall => {
                let index = read_u32(&self.code, &mut self.ip);
                let (name, func) = match self.fns.get_index(index as usize) {
                    Some(tuple) => tuple,
                    _ => self.thread_panic(&format!("could not find extern function at {}", index)),
                };
                thread_assert!(self, func.ip == 0, "extern fn call calling non-extern function");
                match &name[..] {
                    "_log" => sfg_std::log(self),
                    _ => {
                        self.thread_panic("special reflection business not yet supported and stdlib not found")
                    }
                };
            }
            Deser::FnCall => {
                let index = read_u32(&self.code, &mut self.ip);
                let func = match self.fns.get_index(index as usize) {
                    Some((_name, func)) => func,
                    _ => self.thread_panic(&format!("could not find function at {}", index)),
                };
                // We do we push ip here and not in
                // set_fn? because set_fn by user should not
                // push to stack, it should allow exit
                self.call_stack.push(self.ip);
                if self.call_stack.len() > CALL_STACK_MAX_SIZE {
                    self.sane_state();
                    self.thread_panic("call stack overflow (too much recursion?)");
                }
                Self::set_fn(&mut self.ip, func);
            }
            Deser::BAnd => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push((a & b) as i32);
            }
            Deser::BNot => {
                let a = self.stack.pop().unwrap();
                self.stack.push((!a) as i32);
            }
            Deser::JumpZero => {
                let to = read_u32(&self.code, &mut self.ip);
                let test = self.stack.pop().unwrap();
                if test == 0 {
                    self.ip = to as usize;
                }
            }
            Deser::Dup => {
                let count = next(&self.code, &mut self.ip) as usize;
                thread_assert!(self, count < self.stack.len(),
                    "attempted to Dup with stack underflow {}", count);
                // -1 because 0 means last but len() means last+1
                let stack_elem = self.stack[self.stack.len() - count - 1];
                self.stack.push(stack_elem);
            }
            Deser::Swap => {
                let count = next(&self.code, &mut self.ip) as usize;
                thread_assert!(self, count < self.stack.len(),
                    "attempted to Swap with stack underflow {}", count);
                // -1 because 0 means last but len() means last+1
                let down_i = self.stack.len() - 1 - count;
                let up_i = self.stack.len() - 1;
                self.stack.swap(down_i, up_i);
            }
            Deser::Panic => {
                let line = read_u32(&self.code, &mut self.ip);
                let col = read_u32(&self.code, &mut self.ip);
                self.sane_state();
                self.thread_panic(&format!("sfg code panicked at line {}:{}", line, col));
            }
            Deser::Add => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(a + b);
            }
            Deser::Sub => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(b - a);
            }
            Deser::Mul => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(b * a);
            }
            Deser::Div => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(b / a);
            }
            Deser::FAdd => {
                let a = pop_f(&mut self.stack);
                let b = pop_f(&mut self.stack);
                push_f(&mut self.stack, a + b);
            }
            Deser::FSub => {
                let a = pop_f(&mut self.stack);
                let b = pop_f(&mut self.stack);
                push_f(&mut self.stack, b - a);
            }
            Deser::FLess => {
                let a = pop_f(&mut self.stack);
                let b = pop_f(&mut self.stack);
                self.stack.push((b < a) as i32);
            }
            Deser::FMul => {
                let a = pop_f(&mut self.stack);
                let b = pop_f(&mut self.stack);
                push_f(&mut self.stack, b * a);
            }
            Deser::FDiv => {
                let a = pop_f(&mut self.stack);
                let b = pop_f(&mut self.stack);
                push_f(&mut self.stack, b / a);
            }
            Deser::Return => {
                self.ip = match self.call_stack.pop() {
                    Some(ip) => ip,
                    None => return true,
                };
            }
            // TODO: Split deser into categories so this unreachable code won't be necessary
            got @ Deser::Type(_)
            | got @ Deser::FnHeader
            | got @ Deser::ExternFnHeader
            | got @ Deser::StringLit
            | got @ Deser::Void => self.thread_panic(&format!("expected instruction, got {:?}", got)),
        }
        false
    }
    fn sane_state(&mut self) {
        // Deallocate everything
        self.stack.resize(0, 0);
        self.stack.shrink_to_fit();
        self.call_stack.resize(0, 0);
        self.call_stack.shrink_to_fit();
    }
    // Changed to an associated function because borrow checker still can't
    // understand that Fn is contained within self
    fn set_fn(ip: &mut usize, func: &Fn) {
        assert_ne!(func.ip, 0, "tried to call extern function");
        *ip = func.ip;
    }
    fn run(&mut self) {
        loop {
            debug!(
                "stack {:?}| next {:?}| call stack {:?}",
                self.stack,
                deser_strong(self.code[self.ip]),
                self.call_stack,
            );
            if self.exec_next() {
                break;
            }
        }
    }
    // I can't believe this works
    // I have no idea how this works
    // Do not ask me how this works
    // Owned string necessary for evil pointer arithmetic
    #[allow(clippy::ptr_arg)]
    pub fn push_string(&mut self, string: &String) {
        let as_number = string as *const String as i32;
        self.stack.push(as_number);
    }
    /// This ONLY calls the function, does NOT push to stack
    /// use the c! macro to perform a call. It's only public because
    /// it has to be
    #[doc(hidden)]
    pub fn call_name(&mut self, name: &str) {
        let func = match self.fns.get(name) {
            Some(func) => func,
            None => self.thread_panic(&format!("could not find function {}", name)),
        };
        Self::set_fn(&mut self.ip, func);
        self.run();
    }
}
