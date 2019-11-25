extern crate rsfg;
use rsfg::{compile, CompileError};
use std::path::Path;
// We use rvmfg for convenient integration testing
// Not a build dependency, just for this test.
extern crate rvmfg;
use rvmfg::{call, Thread};

// Log needs to be initialized once in the async mess that is cargo test
use std::sync::Once;
static LOGGER_INIT: Once = Once::new();

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
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug);
    let result = compile_no_std_safe("tests/scripts/decompile.sfg");
    assert_hex(
        result,
        vec![
            // bcfg
            0x62, 0x63, 0x66, 0x67, // header for main:
            // fn header
            0x33, // return type: void
            0x21, // parameter count
            0, // "main"
            0x6d, 0x61, 0x69, 0x6e, // "\0"
            0, // codeloc (4b)
            0x1c, 0x00, 0x00, 0x00, // header for log:
            // extern fn header
            0x34, // return type: void
            0x21, // parameter count
            1, // parameters:
            // type: string
            0x11, // "log\0"
            0x6c, 0x6f, 0x67, 0, // string lit
            0x32, // "hi\0"
            0x68, 0x69, 0, // push
            0x30, // 8
            0x08, 0x00, 0x00, 0x00, // push 8 again
            0x30, 0x08, 0x00, 0x00, 0x00, // equals
            0x38, // jump zero
            0x39, // by one instruction (just the return) to AFTER:
            0x01, // return
            0x35, // AFTER:
            // call log:
            // push string lit
            0x30, 0x00, 0x00, 0x00, // string index
            0, // call extern log
            0x31, // function index (4b)
            // 1 (0:main, 1:log)
            0x01, 0x00, 0x00, 0x00, // Return
            0x35,
        ],
    );
}

fn ensure_log_init() {
    LOGGER_INIT.call_once(|| env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .is_test(true)
        .init()
    );
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

fn call_main(path: &Path) {
    let bytecode = compile_safe(path);
    let mut thread = Thread::new(bytecode);
    call![thread.main()];
    state_tests(&thread);
}

// There are some additional tests we can make on all programs
// Like assert various things about the final state of the program
// By convention IN THIS TEST SUITE ONLY main() is void main(void)
// This allows us to assume no returns
// Note that there is a test in lower that checks for total push/pop balance
// So a problem here should indicate a VM problem
fn state_tests(thread: &Thread) {
    assert_eq!(thread.stack.len(), 0);
    assert_eq!(thread.call_stack.len(), 0);
}

fn run_in_dir(dir: &str) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            let pathstr = path.to_string_lossy();
            println!("TESTING: {}", pathstr);
            call_main(&path);
        }
    }
    Ok(())
}

#[test]
fn test_scripts() -> std::io::Result<()> {
    ensure_log_init();
    run_in_dir("tests/scripts")
}

// Ignore because these performance tests literally take a long time
#[ignore]
#[test]
fn test_perf_tests() -> std::io::Result<()> {
    // No log to accurately test perf - TODO: actually may still log if ran after logging one
	run_in_dir("tests/scripts/perf")
}

fn test_should_fail(entry: std::fs::DirEntry) {
    use std::panic::{catch_unwind, AssertUnwindSafe};
    let path = entry.path();
    if path.is_file() {
        let pathstr = path.to_string_lossy();
        println!("TESTING (SHOULD PANIC): {}", pathstr);
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
        assert!(result.is_err());
        // These should PASS. Thus not wrappend in catch_unwind
        state_tests(&thread);
    }
}

#[test]
fn test_fails() -> std::io::Result<()> {
    ensure_log_init();
    for entry in std::fs::read_dir("tests/scripts/fail")? {
        test_should_fail(entry?);
    }
    Ok(())
}

#[ignore]
#[test]
fn test_ignored_fails() -> std::io::Result<()> {
    // No log to speed up - TODO: actually may still log if ran after logging one
    for entry in std::fs::read_dir("tests/scripts/fail/ignore")? {
        test_should_fail(entry?);
    }
    Ok(())
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
            if !out_path.is_file() {
	            panic!("no expected output for test at {}. to interactively populate:
cargo run -- --update-tests GARBAGE", out_path.to_string_lossy());
            }
            let err = compile_file(&path).expect_err("error example compiled without error");
            let err_str = format!("{}", err);
            let expected = std::fs::read_to_string(out_path.clone()).unwrap();
            assert_eq!(err_str, expected);
        }
    }
}

