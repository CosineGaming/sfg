#[macro_use]
extern crate log;

mod read;
mod sfg_std;
mod thread;

pub use thread::Thread;

/// let mut thread = Thread::new(program);
/// call![thread.main()]
#[macro_export]
macro_rules! call {
    ( $thread:ident.$function:ident($( $push:expr )*) ) => {
        {
            $(
                $push.push_to(&mut $thread);
            )*
            $thread.call_name(stringify!($function));
        }
    };
}

/// This trait serves only to make the macro work easily
/// relevant push_[type]()s and call_name() are just as effective
pub trait Pushable {
    fn push_to(&self, thread: &mut Thread);
}
impl Pushable for String {
    fn push_to(&self, thread: &mut Thread) {
        thread.push_string(self);
    }
}
impl Pushable for str {
    fn push_to(&self, thread: &mut Thread) {
        thread.push_string(&self.to_string());
    }
}
