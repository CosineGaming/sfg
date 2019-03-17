extern crate rvmfg;
use rvmfg::Thread;

fn load_file(filename: &str) -> Thread {
	let code = std::fs::read(filename)
		.expect("could not load given file");
	Thread::new(code)
}

#[test]
fn hello_world_no_panic() {
	let thread = load_file("tests/binaries/hello-world.bcfg");
}

//#[test]
//fn without_errors() {
	//// Basically, i just put everything i implement in this file, so it
	//// can check if it compiles without errors without having to design a
	//// rigorous test
	//compile_file("tests/binaries/without-errors.bcfg");
//}

