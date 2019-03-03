extern crate rsfg;
use rsfg::compile;

fn main() {
	let script_filename = std::env::args().nth(1)
		.expect("no filename given");
	let script_string = std::fs::read_to_string(script_filename)
		.expect("could not load given file");
    println!("{}", script_string);
    println!("{}", &compile(&script_string));
}

