extern crate rsfg;
use rsfg::{compile, CompileError};
use std::path::Path;
use std::error::Error;
// We use rvmfg for convenient integration testing
// Not a build dependency, just for this test.
extern crate rvmfg;
use rvmfg::Thread;

// Log needs to be initialized once in the async mess that is cargo test
use std::sync::Once;
static LOGGER_INIT: Once = Once::new();

// TODO: need a test of call![main("a string")] because i think it's broken
// but capturing stdout and such is hard(?) so yeah

fn compile_no_std_safe(filename: &str) -> Vec<u8> {
    let script_string = std::fs::read_to_string(filename).expect("could not load given file");
    match compile(&script_string, "") {
        Ok(c) => c,
        Err(e) => {
            println!("{}", e);
            panic!("file test failed to compile");
        }
    }
}

fn assert_hex(a: Vec<u8>, b: Vec<u8>) {
    assert_eq!(a, b, "result:\n{:x?}\nexpected:\n{:x?}", a, b);
}

#[test]
fn decompile() {
    env_logger::builder().filter_level(log::LevelFilter::Debug);
    let result = compile_no_std_safe("tests/scripts/decompile.sfg");
    assert_hex(
        result,
        vec![
            // trailing comments on all lines to prevent rustfmt
            b'b', b'c', b'f', b'g', // bcfg magic number
            // header for main:
            0x02, // fn header
            0x10, // return type: void
            0,    // parameter count
            0x6d, 0x61, 0x69, 0x6e, // "main"
            0,    // "\0"
            0x1c, 0x00, 0x00, 0x00, // codeloc
            // header for log:
            0x03, // extern fn header
            0x10, // return type: void
            1,    // parameter count
            // parameters:
            0x12, // type: string
            0x6c, 0x6f, 0x67, 0, // "log\0"
            0x01, // string lit
            0x68, 0x69, 0, // "hi\0"
            0x20, // push
            0x08, 0x00, 0x00, 0x00, // 8
            0x20, 0x08, 0x00, 0x00, 0x00, // push 8 again
            0x50, // add
            0x2a, // decl x
            0x20, 0x00, 0x00, 0x00, 0x00, // push 0 (false)
            0x25, // jump zero
            0x3b, 0x00, // to absolute point (label AFTER):
            // call log:
            0x20, 0x00, 0x00, 0x00, // push string lit
            0,    // string index
            0x2a, // decl for function call
            0x23, // call extern log
            // function index (4b)
            0x01, 0x00, 0x00, 0x00, // 1 (0:main, 1:log)
            // AFTER:
            0x28, 0x01, // Delet 1 local x
            0x24, // Return
        ]
    );
}

fn ensure_log_init() {
    LOGGER_INIT.call_once(|| {
        env_logger::builder().filter_level(log::LevelFilter::Debug).is_test(true).init()
    });
}

fn get_stdlib() -> String {
    let std_filename = "src/sfg/std.sfg";
    std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(path: &Path) -> Result<Vec<u8>, CompileError> {
    let script_string = std::fs::read_to_string(path).expect("could not load given file");
    compile(&script_string, &get_stdlib())
}

fn compile_safe(path: &Path) -> Vec<u8> {
    match compile_file(path) {
        Ok(c) => c,
        Err(e) => {
            println!("{}", e);
            panic!("file test failed to compile");
        }
    }
}

#[derive(Debug)]
struct StateError(usize, usize, usize);
impl std::fmt::Display for StateError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "improper state, stack length was {}, call stack length was {}, locals len was {}", self.0, self.1, self.2)
    }
}
impl Error for StateError {}

type ThreadResult = (std::thread::Result<()>, Result<(), StateError>);
type SetResult = Vec<(String, ThreadResult)>;

// There are some additional tests we can make on all programs
// Like assert various things about the final state of the program
// By convention IN THIS TEST SUITE ONLY main() is void main(void)
// This allows us to assume no returns
// Note that there is a test in lower that checks for total push/pop balance
// So a problem here should indicate a VM problem
fn state_tests(thread: &Thread) -> Result<(), StateError> {
    let tsl = thread.stack.len();
    let tcsl = thread.call_stack.len();
    let tll = thread.locals.len();
    if tsl != 0 || tcsl != 0 || tll != 0 {
        Err(StateError(tsl, tcsl, tll))
    } else {
        Ok(())
    }
}

fn show_failures(fails: SetResult) {
    let mut should_panic = false;
    for (path, fail) in fails {
        if let Err(ref e) = fail.0 {
            eprintln!("{}: error {:?}", path, e);
            should_panic = true;
        }
        if let Err(ref e) = fail.1 {
            eprintln!("{}: error {}", path, e);
            should_panic = true;
        }
    }
    if should_panic {
        panic!("some tests failed")
    }
}

fn show_successes(fails: SetResult) {
    let mut should_panic = false;
    for (path, fail) in fails {
        if let (Ok(()), Ok(())) = fail {
            eprintln!("{}, which should fail, succeeded", path);
            should_panic = true;
        }
    }
    if should_panic {
        panic!()
    }
}

fn test_dir(path: &'static str) -> SetResult {
    let mut failures = vec![];
    for entry in std::fs::read_dir(path).unwrap() {
        // unwrap too much error fuckery
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            let pathstr = path.to_string_lossy().to_string();
            println!("TESTING: {}", pathstr);
            failures.push((pathstr, run_test(&path)));
        }
    }
    failures
}

#[test]
fn test_succeed() {
    ensure_log_init();
    let failures = test_dir("tests/scripts");
    show_failures(failures);
}

// Ignore because these performance tests literally take a long time
#[ignore]
#[test]
fn test_perf_tests() {
    // No log to accurately test perf - TODO: actually may still log if ran after logging one
    let failures = test_dir("tests/scripts/perf");
    show_failures(failures);
}

fn run_test(path: &Path) -> ThreadResult {
    use std::panic::{catch_unwind, AssertUnwindSafe};
    // Compilation should succeed
    let bytecode = compile_safe(&path);
    // As well as LOADING into the vm
    let mut thread = Thread::new(bytecode);
    // &mut is not UnwindSafe so we wrap it because we test
    // that panics are expected and properly handled by the VM
    // https://doc.rust-lang.org/beta/std/panic/trait.UnwindSafe.html
    let mut wrapped = AssertUnwindSafe(&mut thread);
    // These should panic. The following construction ensures that
    // We can't use call! because it adds additional &mut already wrapped
    let result = catch_unwind(move || wrapped.call_name("main"));
    // print the result in time so we can trace easier
    println!("TEST RESULT for {}: {:?}", path.to_string_lossy(), result);
    // These should PASS. Thus not wrappend in catch_unwind
    let state_res = state_tests(&thread); // TODO: return somehow idk how to type this
    (result, state_res)
}

#[test]
fn test_fails() {
    ensure_log_init();
    let failures = test_dir("tests/scripts/fail");
    show_successes(failures)
}

#[ignore]
#[test]
fn test_ignored_fails() {
    // No log to speed up - TODO: actually may still log if ran after logging one
    let failures = test_dir("tests/scripts/fail/ignore");
    show_successes(failures)
}

// errors / ui
#[test]
fn test_errors() {
    for entry in std::fs::read_dir("tests/scripts/error").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension() == Some(&std::ffi::OsString::from("sfg")) {
            let pathstr = path.to_string_lossy();
            println!("TESTING: {}", pathstr);
            let out_path = path.with_extension("stderr");
            let err = compile_file(&path).expect_err("error example compiled without error");
            if !out_path.is_file() {
                panic!(
                    "no expected output for test at {}. to interactively populate:
cargo run -- --update-tests",
                    out_path.to_string_lossy()
                );
            }
            let err_str = format!("{}", err);
            let expected = std::fs::read_to_string(out_path.clone()).unwrap();
            assert_eq!(err_str, expected);
        }
    }
}
