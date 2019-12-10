#![feature(test)]

extern crate test;
use test::Bencher;

use rsfg::{compile, CompileError};
use rvmfg::{Thread, call};

// copied from test_scripts.rs, cause idk how to import it
fn get_stdlib() -> String {
    let std_filename = "src/sfg/std.sfg";
    std::fs::read_to_string(std_filename).expect("couldn't find std library")
}

fn compile_file(path: &str) -> Result<Vec<u8>, CompileError> {
    let script_string = std::fs::read_to_string(path).expect("could not load given file");
    compile(&script_string, &get_stdlib())
}

#[bench]
fn compile_hodge(b: &mut Bencher) {
    b.iter(||
        compile_file("benches/scripts/hodgepodge.sfg").expect("bench failed"));
}

#[bench]
fn run_hodge(b: &mut Bencher) {
    let compiled = compile_file("benches/scripts/hodgepodge.sfg").expect("bench failed");
    let mut thread = Thread::new(compiled);
    b.iter(|| {
        call![thread.main()]
    });
}

