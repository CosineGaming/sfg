extern crate rsfg;
use rsfg::compile;

// TODO: eventually when we have an import system, lib is going to depend
// on file operations so might as well bite the bullet
fn get_stdlib() -> String {
	let std_filename = "src/sfg/std.sfg";
	std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(filename: &str) -> Vec<u8> {
	let script_string = std::fs::read_to_string(filename)
		.expect("could not load given file");
	compile(&script_string, &get_stdlib())
}

#[test]
fn hello_world() {
	let result = compile_file("tests/scripts/hello-world.sfg");
	assert_eq!(result, vec![
		// bcfg
		0x62, 0x63, 0x66, 0x67,
		// header for main:
		// stack size
		0,
		// return type: void
		0x15,
		// parameter count
		0,
		// "main"
		0x6d, 0x61, 0x69, 0x6e,
		// "\0"
		0,
		// codeloc 0x1A (4b)
		0x1A, 0x00, 0x00, 0x00,
		// externs sep
		0,
		// header for log:
		// stack size
		8,
		// return type: void
		0x15,
		// parameter count
		1,
		// parameters:
		// type: string
		0xb,
		// "log\0"
		0x6c, 0x6f, 0x67, 0,
		// strings sep
		0,
		// "hi\0"
		0x68, 0x69, 0,
		// push string lit
		0x1e,
		// string index
		0,
		// call extern fn
		// TODO: should be index 0
		0x1f,
		// function index (4b)
		0x01, 0x00, 0x00, 0x00,
	]);
}

#[test]
fn without_errors() {
	// Basically, i just put everything i implement in this file, so it
	// can check if it compiles without errors without having to design a
	// rigorous test
	compile_file("tests/scripts/without-errors.sfg");
}

