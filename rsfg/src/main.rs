extern crate rsfg;
use rsfg::compile;
use std::path::Path;

fn main() {
	let script_filename = &std::env::args().nth(1)
		.expect("no filename given");
	let script_path = Path::new(&script_filename);
	let script_string = std::fs::read_to_string(script_path)
		.expect("could not load given file");
	println!("{}", script_string);
	let compiled = compile(&script_string);
	let out = script_path.with_extension("bcfg");
	std::fs::write(out, compiled.clone()).expect("couldn't output compiled file");
	println!("{:X?}", compiled);
}

