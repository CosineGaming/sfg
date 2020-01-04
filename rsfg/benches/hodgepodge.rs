#![feature(test)]

extern crate test;
use test::Bencher;

use rsfg::{compile, CompileError, STDLIB};
use rvmfg::{call, Thread};

fn compile_file(path: &str) -> Result<Vec<u8>, CompileError> {
    let script_string = std::fs::read_to_string(path).expect("could not load given file");
    compile(&script_string, STDLIB)
}

#[bench]
fn compile_hodge(b: &mut Bencher) {
    b.iter(|| compile_file("benches/scripts/hodgepodge.sfg").expect("bench failed"));
}

#[bench]
fn run_hodge(b: &mut Bencher) {
    let compiled = compile_file("benches/scripts/hodgepodge.sfg").expect("bench failed");
    let mut thread = Thread::new(compiled);
    b.iter(|| call![thread.main()]);
}
