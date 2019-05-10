// As much as possible of the standard library is compiled to bcfg to keep
// the VM minimal
// That which cannot be is supposed to be implemented by the order of
// one or two parent-app functions, and is here

use crate::thread::Thread;

pub fn log(thread: &mut Thread) {
    let string_index = thread.stack.pop().expect("param not provided");
    let string = &thread.strings[string_index as usize];
    println!("{}", string);
}
