extern crate rsfg;
// We use rvmfg for convenient integration testing
// Not a build dependency, just for this test.
// Simply rm this test file to remove this dependency
extern crate rvmfg;
use rsfg::compile;
use rvmfg::{Thread, call};

fn get_stdlib() -> String {
	let std_filename = "src/sfg/std.sfg";
	std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(filename: &str) -> Vec<u8> {
	let script_string = std::fs::read_to_string(filename)
		.expect("could not load given file");
	compile(&script_string, &get_stdlib())
}

fn call_main(filename: &str) {
	let bytecode = compile_file(filename);
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

#[test]
fn test_scripts() -> std::io::Result<()> {
	for entry in std::fs::read_dir("tests/scripts")? {
		let entry = entry?;
		let path = entry.path();
		if path.is_file() {
			let pathstr = path.to_string_lossy();
			println!("TESTING: {}", pathstr);
			call_main(&pathstr);
		}
	}
	Ok(())
}

// Ignore because these performance tests literally take a long time
#[ignore]
#[test]
fn test_perf_tests() -> std::io::Result<()> {
	for entry in std::fs::read_dir("tests/scripts/perf")? {
		let entry = entry?;
		let path = entry.path();
		if path.is_file() {
			let pathstr = path.to_string_lossy();
			println!("TESTING: {}", pathstr);
			call_main(&pathstr);
		}
	}
	Ok(())
}

#[test]
fn test_fails() -> std::io::Result<()> {
	use std::panic::{catch_unwind, AssertUnwindSafe};
	for entry in std::fs::read_dir("tests/scripts/fail")? {
		let entry = entry?;
		let path = entry.path();
		if path.is_file() {
			let pathstr = path.to_string_lossy();
			println!("TESTING (SHOULD PANIC): {}", pathstr);
			let bytecode = compile_file(&pathstr);
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
	Ok(())
}

