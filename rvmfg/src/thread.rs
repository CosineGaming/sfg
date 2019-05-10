use crate::read::*;
use crate::sfg_std;

use indexmap::IndexMap;

/// Should be small enough to make small scripts low-RAM, but high enough
/// that startup doesn't take forever with 1000s of incremental allocs
const INIT_STACK_SIZE: usize = 50;
/// Similarly chosen for the expected call stack size
const INIT_CALL_STACK_SIZE: usize = 6;
/// Prints the stack and each instruction
const DEBUG: bool = false;

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
    ip: u32,
    return_type: Option<Type>,
    parameters: Vec<Type>,
}
impl Fn {
    pub fn new(ip: u32, return_type: Option<Type>, parameters: Vec<Type>) -> Self {
        Self {
            ip,
            return_type,
            parameters,
        }
    }
}

impl Thread {
    pub fn new(code: Vec<u8>) -> Self {
        let mut ip = 0;
        if next(&code, &mut ip) != 'b' as u8
            || next(&code, &mut ip) != 'c' as u8
            || next(&code, &mut ip) != 'f' as u8
            || next(&code, &mut ip) != 'g' as u8
        {
            panic!("expected bcfg");
        }
        let mut fns = IndexMap::new();
        let mut strings = Vec::new();
        loop {
            match deser(code[ip]) {
                Some(Deser::FnHeader) => {
                    ip += 1;
                    let (name, func) = read_fn_header(&code, &mut ip, false);
                    fns.insert(name, func);
                }
                _ => break,
            }
        }
        loop {
            match deser(code[ip]) {
                Some(Deser::ExternFnHeader) => {
                    ip += 1;
                    let (name, func) = read_fn_header(&code, &mut ip, true);
                    fns.insert(name, func);
                }
                _ => break,
            }
        }
        loop {
            match deser(code[ip]) {
                Some(Deser::StringLit) => {
                    ip += 1;
                    let string = read_string(&code, &mut ip);
                    strings.push(string);
                }
                _ => break,
            }
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
                    _ => panic!("could not find extern function at {}", index),
                };
                assert_eq!(func.ip, 0, "extern fn call calling non-extern function");
                match &name[..] {
                    "log" => sfg_std::log(self),
                    _ => {
                        panic!("special reflection business not yet supported and stdlib not found")
                    }
                };
            }
            Deser::FnCall => {
                let index = read_u32(&self.code, &mut self.ip);
                let func = match self.fns.get_index(index as usize) {
                    Some((_name, func)) => func,
                    _ => panic!("could not find function at {}", index),
                };
                // We do we push ip here and not in
                // call_fn? because call_fn by user should not
                // push to stack, it should allow exit
                self.call_stack.push(self.ip);
                Self::set_fn(&mut self.ip, func);
            }
            Deser::Equals => {
                let a = self.stack.pop();
                let b = self.stack.pop();
                self.stack.push((a == b) as i32);
            }
            Deser::JumpZero => {
                let amount = read_i8(&self.code, &mut self.ip);
                let test = self.stack.pop().unwrap();
                if test == 0 {
                    self.ip = (self.ip as isize + amount as isize) as usize;
                }
            }
            Deser::Dup => {
                let count = next(&self.code, &mut self.ip) as usize;
                // -1 because 0 means last but len() means last+1
                let stack_elem = *self.stack.get(self.stack.len() - count - 1).unwrap();
                self.stack.push(stack_elem);
            }
            Deser::Swap => {
                let count = next(&self.code, &mut self.ip) as usize;
                // -1 because 0 means last but len() means last+1
                let down_i = self.stack.len() - 1 - count;
                let up_i = self.stack.len() - 1;
                let down = self.stack[down_i];
                self.stack[down_i] = self.stack[up_i];
                self.stack[up_i] = down;
            }
            Deser::Panic => {
                let col = self.stack.pop().unwrap();
                let line = self.stack.pop().unwrap();
                // Deallocate everything
                self.stack.resize(0, 0);
                self.stack.shrink_to_fit();
                self.call_stack.resize(0, 0);
                self.call_stack.shrink_to_fit();
                panic!("sfg code panicked at line {}:{}", line, col);
            }
            Deser::Add => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(a + b);
            }
            Deser::Sub => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                // The stack is placed in makes-sense order, we reverse
                self.stack.push(b - a);
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
            | got @ Deser::Void => panic!("expected instruction, got {:?}", got),
        }
        false
    }
    // Changed to an associated function because borrow checker still can't
    // understand that Fn is contained within self
    fn set_fn(ip: &mut usize, func: &Fn) {
        assert_ne!(func.ip, 0, "tried to call extern function");
        *ip = func.ip as usize;
    }
    fn run(&mut self) {
        loop {
            if DEBUG {
                println!(
                    "stack {:?}| next {:?}| call stack {:?}",
                    self.stack,
                    deser_strong(self.code[self.ip]),
                    self.call_stack,
                );
            }
            if self.exec_next() {
                break;
            }
        }
    }
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
            None => panic!("could not find function {}", name),
        };
        Self::set_fn(&mut self.ip, func);
        self.run();
    }
}
