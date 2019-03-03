extern crate sfg;
use sfg::compile;

fn compile_file(filename: &str) {
	let script_string = std::fs::read_to_string(filename)
		.expect("could not load given file");
	compile(script_string);
}

#[test]
fn hello_world() {
	compile_file("tests/scripts/hello-world.sfg");
}

