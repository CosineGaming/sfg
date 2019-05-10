extern crate rvmfg;
use rvmfg::{call, Thread};

fn main() {
    let code_filename = &std::env::args().nth(1).expect("no filename given");
    let bytecode = std::fs::read(code_filename).expect("could not read given file");
    let mut thread = Thread::new(bytecode);
    call![thread.main()];
}
