use crate::read::*;
use crate::sfg_std;

use indexmap::IndexMap;

/// Should be small enough to make small scripts low-RAM, but high enough
/// that startup doesn't take forever with 1000s of incremental allocs
const INIT_STACK_SIZE: usize = 16;
/// Similarly chosen for the expected call stack size
const INIT_CALL_STACK_SIZE: usize = 8;
/// Similarly chosen for the expected locals size
const INIT_LOCALS_SIZE: usize = 16;
/// This is mostly arbitrary but keeps us from unrecoverable OOM error on
/// incorrect recursion
/// 256MB is excessively large but within the realms of normal operation
/// /4 means gives twice on 64-bit
const CALL_STACK_MAX_SIZE: usize = 256 * 1024 * 1024 / 4;

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
    /// shouldn't really be public but used for testing in rsfg (TODO)
    pub locals: Vec<i32>,
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
    fn thread_panic(&mut self, msg: &str) -> ! {
        self.sane_state();
        eprintln!("vm panic: {}\nBACKTRACE:\n{}", msg, self);
        panic!("vm panic");
    }
    pub fn new(code: Vec<u8>) -> Self {
        let mut ip = 0;
        if read_u8(&code, &mut ip) != b'b'
            || read_u8(&code, &mut ip) != b'c'
            || read_u8(&code, &mut ip) != b'f'
            || read_u8(&code, &mut ip) != b'g'
        {
            panic!("expected bcfg");
        }
        let mut fns = IndexMap::new();
        let mut strings = Vec::new();
        while let Some(DeserHeader::FnHeader) = deser_header(code[ip]) {
            ip += 1;
            let (name, func) = read_fn_header(&code, &mut ip, false);
            fns.insert(name, func);
        }
        while let Some(DeserHeader::ExternFnHeader) = deser_header(code[ip]) {
            ip += 1;
            let (name, func) = read_fn_header(&code, &mut ip, true);
            fns.insert(name, func);
        }
        while let Some(DeserHeader::StringLit) = deser_header(code[ip]) {
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
            locals: Vec::with_capacity(INIT_LOCALS_SIZE),
        }
    }
    /// Returns whether a return has been called requiring exit
    fn exec_next(&mut self) -> bool {
        // The stack is a rust Vec. rust vecs don't deallocate
        // until asked to, so we don't have to worry about pop then
        // push being slow. If we're worried, we can shrink_to at the
        // end of each instruction
        match deser(read_u8(&self.code, &mut self.ip)) {
            Deser::Push => {
                let lit = read_i32(&self.code, &mut self.ip);
                self.push(lit);
            }
            Deser::Pop => {
                self.pop();
            }
            Deser::ExternFnCall => {
                let index = read_u16(&self.code, &mut self.ip);
                let (name, func) = match self.fns.get_index(index as usize) {
                    Some(tuple) => tuple,
                    _ => self.thread_panic(&format!("could not find extern function at {}", index)),
                };
                thread_assert!(
                    self,
                    func.ip == 0,
                    "extern fn call calling non-extern function"
                );
                match &name[..] {
                    "_log" => sfg_std::log(self),
                    _ => self.thread_panic(
                        "special reflection business not yet supported and stdlib not found",
                    ),
                };
            }
            Deser::FnCall => {
                let index = read_u16(&self.code, &mut self.ip);
                let func = match self.fns.get_index(index as usize) {
                    Some((_name, func)) => func,
                    _ => self.thread_panic(&format!("could not find function at {}", index)),
                };
                // We do we push ip here and not in
                // set_fn? because set_fn by user should not
                // push to stack, it should allow exit
                self.call_stack.push(self.ip);
                if self.call_stack.len() > CALL_STACK_MAX_SIZE {
                    self.thread_panic("call stack overflow (too much recursion?)");
                }
                Self::set_fn(&mut self.ip, func);
            }
            Deser::Xor => {
                let b = self.pop();
                let a = self.pop();
                self.push(a ^ b);
            }
            Deser::Less => {
                let b = self.pop();
                let a = self.pop();
                self.push((a < b) as i32);
            }
            Deser::JumpZero => {
                let to = read_u16(&self.code, &mut self.ip);
                let test = self.pop();
                if test == 0 {
                    self.ip = to as usize;
                }
            }
            Deser::Jump => {
                let to = read_u16(&self.code, &mut self.ip);
                self.ip = to as usize;
            }
            Deser::Panic => {
                let line = read_u32(&self.code, &mut self.ip);
                let col = read_u32(&self.code, &mut self.ip);
                self.thread_panic(&format!("sfg code panicked at line {}:{}", line, col));
            }
            Deser::Decl => {
                let a = self.pop();
                self.locals.push(a);
            }
            Deser::DeclLit => {
                let lit = read_i32(&self.code, &mut self.ip);
                self.locals.push(lit);
            }
            Deser::Store => {
                let ri = read_u8(&self.code, &mut self.ip) as usize;
                let i = self.locals.len() - ri - 1;
                let a = self.pop();
                self.locals[i] = a;
            }
            Deser::StoreLit => {
                let ri = read_u8(&self.code, &mut self.ip) as usize;
                let lit = read_i32(&self.code, &mut self.ip);
                let i = self.locals.len() - ri - 1;
                self.locals[i] = lit;
            }
            Deser::Load => {
                let ri = read_u8(&self.code, &mut self.ip) as usize;
                debug!("ri {}", ri);
                let i = self.locals.len() - ri - 1;
                self.push(self.locals[i]);
            }
            Deser::Locals => {
                let count = read_u8(&self.code, &mut self.ip) as usize;
                self.locals.reserve(count);
            }
            Deser::DeVars => {
                let count = read_u8(&self.code, &mut self.ip) as usize;
                let new_size = self.locals.len() - count;
                self.locals.resize(new_size, 0);
            }
            Deser::Dup => {
                let a = self.last();
                self.push(a);
            }
            Deser::Add => {
                let b = self.pop();
                let a = self.pop();
                self.push(a + b);
            }
            Deser::Sub => {
                let b = self.pop();
                let a = self.pop();
                self.push(a - b);
            }
            Deser::Mul => {
                let b = self.pop();
                let a = self.pop();
                self.push(a * b);
            }
            Deser::Div => {
                let b = self.pop();
                let a = self.pop();
                self.push(a / b);
            }
            Deser::Mod => {
                let b = self.pop();
                let a = self.pop();
                self.push(a % b);
            }
            Deser::FAdd => {
                let b = self.pop_f();
                let a = self.pop_f();
                self.push_f(a + b);
            }
            Deser::FSub => {
                let b = self.pop_f();
                let a = self.pop_f();
                self.push_f(a - b);
            }
            Deser::FLess => {
                let b = self.pop_f();
                let a = self.pop_f();
                self.push((a < b) as i32);
            }
            Deser::FMul => {
                let b = self.pop_f();
                let a = self.pop_f();
                self.push_f(a * b);
            }
            Deser::FDiv => {
                let b = self.pop_f();
                let a = self.pop_f();
                self.push_f(a / b);
            }
            Deser::Return => {
                self.ip = match self.call_stack.pop() {
                    Some(ip) => ip,
                    None => return true,
                };
            }
            Deser::Not => {
                let a = self.pop();
                self.push(!(a != 0) as i32);
            }
        }
        false
    }
    fn sane_state(&mut self) {
        // Deallocate everything
        self.stack.resize(0, 0);
        self.stack.shrink_to_fit();
        self.call_stack.resize(0, 0);
        self.call_stack.shrink_to_fit();
        self.locals.resize(0, 0);
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
                "stack {:?}| next {:?}| call stack {:?}| locals {:?}",
                self.stack,
                deser(self.code[self.ip]),
                self.call_stack,
                self.locals,
            );
            if self.exec_next() {
                break;
            }
        }
    }
    fn pop(&mut self) -> i32 {
        match self.stack.pop() {
            Some(p) => p,
            None => self.thread_panic("stack underflow"),
        }
    }
    fn last(&mut self) -> i32 {
        match self.stack.last() {
            Some(p) => *p,
            None => self.thread_panic("stack underflow"),
        }
    }
    fn push(&mut self, p: i32) {
        self.stack.push(p)
    }
    fn pop_f(&mut self) -> f32 {
        i_as_f(self.pop())
    }
    fn push_f(&mut self, f: f32) {
        self.push(f_as_i(f))
    }
    // I can't believe this works
    // I have no idea how this works
    // Do not ask me how this works
    // Owned string necessary for evil pointer arithmetic
    #[allow(clippy::ptr_arg)]
    pub fn push_string(&mut self, string: &String) {
        let as_number = string as *const String as i32;
        self.push(as_number);
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
