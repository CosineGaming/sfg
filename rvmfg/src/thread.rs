use crate::f_as_i;
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

/// Start here, i.e. to run code, you use a Thread.
///
/// A Thread is created using bcfg bytecode only, however sfg
/// code can be interpreted simply by running rsfg compile as a library and then
/// running rvmfg with the in-memory result. In fact, rsfg depends on rvmfg
/// specifically for this purpose so you can run `rsfg --run interpret_this.sfg`
///
/// Once created, a thread's state is maintained until destroyed, however
/// note there are currently no faculties for global state in sfg. There are
/// two ways to run a thread:
///
/// 1. Using [Thread::arg] and
/// [Thread::call_name]. This method is the true API, and is the recommended
/// way to build C or other language APIs out of rvmfg. Because all data is
/// stored internally as i32, we know that any VM types can be stored as
/// i32 except for string literals, hence the [Thread::arg_str_lit]
/// function. Rust users can use the given [crate::Argable] implementations to help you
/// push the builtin VM types, or even your own, but it is merely sugar for
/// an eventual `arg` call.
///
/// 2. Using the [call] macro. This is recommended for rust programs when
/// dynamic args len is not necessary, eg when calling known code. It
/// allows you to simply write a rust-style function call with values.
#[derive(PartialEq, Debug)]
pub struct Thread {
    stack: Vec<i32>,
    /// The call stack is managed by the VM, containing calls only
    /// It's kept separate as opposed to machine architectures because
    /// the use of the data stack is made harder by combining them.
    /// Each usize is a ip
    call_stack: Vec<usize>,
    code: Vec<u8>,
    // Code pointer
    ip: usize,
    strings: Vec<String>,
    fns: IndexMap<String, Fn>,
    locals: Vec<i32>,
}

#[derive(PartialEq, Debug)]
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

macro_rules! thread_assert {
    ( $thread:ident, $cond:expr, $lit:literal $(,)? $($expr:expr),* ) => {
        if !$cond {
            $thread.thread_panic(&format!($lit, $($expr),*))
        }
    }
}

impl std::fmt::Display for Thread {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for &stack in &[&[self.ip], &self.call_stack[..]] {
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
    /// Construct a new thread with the given bcfg bytecode. This can have
    /// been compiled with rsfg in memory just now or loaded from disk after
    /// being compiled. You need a Vec because I don't wanna dabble in const
    /// generics rn, and I need an owned, dynamic length array.
    /// ```
    /// let program = include_bytes!("../tests/rvmfg-call-doc.bcfg").to_vec();
    /// let mut thread = rvmfg::Thread::new(program);
    /// ```
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
    /// Push a value to locals as in an argument to a function.
    /// It should be used in the calling of a function that requires arguments.
    /// Because DeVars is in the end of functions, there is no need to pop
    /// the arguments.
    /// ```
    /// # let program = include_bytes!("../tests/rvmfg-call-doc.bcfg").to_vec();
    /// # let mut thread = rvmfg::Thread::new(program);
    /// thread.arg(4);
    /// thread.arg(8);
    /// thread.call_name("fn_to_call");
    /// ```
    pub fn arg(&mut self, what: i32) {
        self.locals.push(what);
    }
    /// Push a string as an argument, becoming a string literal that will
    /// not be modifyable
    pub fn arg_str_lit(&mut self, string: String) {
        self.locals.push(self.strings.len() as i32);
        self.strings.push(string);
    }
    /// Calls a function. To pass arguments, you have to have called [Thread::arg]
    /// in proper order before calling this.
    /// In rust, you can use the [call] macro for convenience.
    /// ```
    /// # let program = include_bytes!("../tests/rvmfg-call-doc.bcfg").to_vec();
    /// # let mut thread = rvmfg::Thread::new(program);
    /// thread.call_name("main");
    /// ```
    /// panics on nonexistant function
    pub fn call_name(&mut self, name: &str) {
        let func = match self.fns.get(name) {
            Some(func) => func,
            None => self.thread_panic(&format!("could not find function {}", name)),
        };
        Self::set_fn(&mut self.ip, func);
        self.run();
    }
    /// Functions in sfg can only return one 32-bit value. Although the
    /// type could be ascertained by the function signature, there's no way to
    /// guarantee it compile-time so instead we merely ask that you convert
    /// from i32 to whatever type it was meant to be
    pub fn pop_return(&mut self) -> i32 {
        return self.pop();
    }
    /// Typechecking is not doen by the thread, that's your responsibility
    /// and your responsibility to choose how to deal with that. None => Void
    ///
    /// panics on nonexistant function
    pub fn get_return_type(&self, name: &str) -> Option<Type> {
        let func = match self.fns.get(name) {
            Some(func) => func,
            None => panic!("could not find function {}", name)
        };
        func.return_type
    }
    /// At the *end* of a top-level function call (ie our conception of main)
    /// There are some state guarantees we could make. Properly compiled
    /// sfg code executed by a properly implemented VM should *never* reach an
    /// unsane state (ie it's not a programming error). Additionally, rsfg has
    /// some runtime checks to ensure push/pop balance, so this *most likely*
    /// indicates an internal VM error
    ///
    /// Note that stack length assumes there hasn't been a return, so you
    /// need to have called pop_return to prevent that false negative
    pub fn is_sane_state(&self) -> Result<(), StateError> {
        let stack = self.stack.len();
        let call_stack = self.call_stack.len();
        let locals = self.locals.len();
        if stack != 0 || call_stack != 0 || locals != 0 {
            Err(StateError(stack, call_stack, locals))
        } else {
            Ok(())
        }
    }
    fn thread_panic(&mut self, msg: &str) -> ! {
        self.sane_state();
        eprintln!("vm panic: {}\nBACKTRACE:\n{}", msg, self);
        panic!("vm panic");
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
                    "_log" => sfg_std::log(&self.strings, &mut self.locals),
                    other => {
                        let msg = format!("vm std lib fn {} not found", other);
                        self.thread_panic(&msg);
                    }
                }
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
                self.push((a == 0) as i32);
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
}

/// Used for [Thread::is_sane_state], if it's not sane this error is returned.
/// In order: stack length, call stack length, locals length.
#[derive(Debug)]
pub struct StateError(usize, usize, usize);
impl std::fmt::Display for StateError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "improper state, stack length was {}, call stack length was {}, locals len was {}",
            self.0, self.1, self.2
        )
    }
}
impl std::error::Error for StateError {}

#[cfg(test)]
mod test {
    use super::Thread;
    use crate::{call, Argable};
    #[test]
    fn arg_stuff() {
        let program = include_bytes!("../tests/rvmfg-add.bcfg").to_vec();
        let mut thread = Thread::new(program);
        let rv = call![thread.add(5, 8)];
        thread.is_sane_state().expect("unsane state");
        assert_eq!(rv, Some(13));
    }
}
