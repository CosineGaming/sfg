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
	// There are some additional tests we can make on all programs
	// By convention IN THIS TEST SUITE ONLY main() is void main(void)
	// This allows us to assume no returns
	// Note that there is a test in lower that checks for total push/pop balance
	// So a problem here should indicate a VM problem
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
			println!("{}", pathstr);
			call_main(&pathstr);
		}
	}
	Ok(())
}

