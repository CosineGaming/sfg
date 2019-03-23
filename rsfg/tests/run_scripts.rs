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

fn assert_hex(a: Vec<u8>, b: Vec<u8>) {
	assert_eq!(a, b, "result:\n{:X?}\nexpected:\n{:X?}", a, b);
}

#[test]
fn hello_world() {
	let result = compile_file("tests/scripts/hello-world.sfg");
	assert_hex(result, vec![
		// bcfg
		0x62, 0x63, 0x66, 0x67,
		// header for main:
		// fn header
		0x33,
		// return type: void
		0x21,
		// parameter count
		0,
		// "main"
		0x6d, 0x61, 0x69, 0x6e,
		// "\0"
		0,
		// codeloc (4b)
		0x1C, 0x00, 0x00, 0x00,
		// header for log:
		// extern fn header
		0x34,
		// return type: void
		0x21,
		// parameter count
		1,
		// parameters:
		// type: string
		0x11,
		// "log\0"
		0x6c, 0x6f, 0x67, 0,
		// string lit
		0x32,
		// "hi\0"
		0x68, 0x69, 0,
		// push string lit
		0x30,
		// string index
		0,
		// call extern log
		0x31,
		// function index (4b)
		// 1 (0:main, 1:log)
		0x01, 0x00, 0x00, 0x00,
		// Return
		0x35,
		// Params size
		0x00,
	]);
}

#[test]
fn without_errors() {
	// Basically, i just put everything i implement in this file, so it
	// can check if it compiles without errors without having to design a
	// rigorous test
	compile_file("tests/scripts/without-errors.sfg");
}

