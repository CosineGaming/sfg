extern crate rsfg;
use rsfg::compile;

// TODO: eventually when we have an import system, lib is going to depend
// on file operations so might as well bite the bullet
fn get_stdlib() -> String {
	let std_filename = "src/sfg/std.sfg";
	std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(filename: &str) {
	let script_string = std::fs::read_to_string(filename)
		.expect("could not load given file");
	compile(&script_string, &get_stdlib());
}

#[test]
fn hello_world_parse() {
	compile_file("tests/scripts/hello-world.sfg");
}

#[test]
fn without_errors() {
	// Basically, i just put everything i implement in this file, so it
	// can check if it compiles without errors without having to design a
	// rigorous test
	compile_file("tests/scripts/without-errors.sfg");
}

