to get memory support:

1. swap i32 to isize, i32 is merely the format in the bytecode
2. add two instructions:
	Deref: push (read memory at (pop as pointer))
	Memstore: pop address, pop value, store value at address
3. decide how to demarcate pass-by-reference (&mut) vs pass-by-value OR just always pass-by-reference
4. implement that, if it helps you can only implement VM-facing, not internal calls (probly won't help)
5. now you can make basic systems that actually affect state!
6. you can also finally have arrays and proper strings and more, but don't
bother going there until a real system calls for it. if i have to guess you
might wanna implement vec2 first

to get dynamic systems:

1. add a foreign type syntax and put it in headers
2. use https://docs.rs/specs/0.15.1/specs/trait.Accessor.html
	1. match on the foreign type strings to compile-time types like
	```rust
	match foreign_type {
		"FrameNumber" => FrameNumber::reads(),
		"Pos" => Pos::reads(),
	}
	```
	could even probly use a macro like
	```rust
	foreign_types!(reads():
		FrameNumber,
		Pos,
	)
	```
	or maybe you can use `dyn` types with `SystemData` or something, idk
3. next you wanna probly make a generator that creates a SystemData based
on the code, or maybe you have to make it fully dynamic, it's fine either way
4. just read each bcfg file, read the types, generate the SystemData, create
a system, and add it!

to typecheck rvmfg:

1. change the API to call to `fn call(func: &str, args: Vec<&dyn Any>) -> Option<i32>`
or something like that
2. check the lengths / types with `arg.type_id`, downcast and convert types
3. if the function returns, Some(pop_return()), otherwise None
4. only downside is unwrap() at the end is not checked, but it's part of our expected signature
5. change call! macro to simply rearrange things a bit and add a vec

