mod lib;

fn compile_file(filename: String) {
	let script_string = std::fs::read_to_string(filename)
		.expect("could not load given file");
	lib::compile(script_string);
}

