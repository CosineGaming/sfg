Scripting for Games
===================

> A scripting language with strong static analysis and a minimal VM, made
for game development

This language is intended to script systems in an ECS scalably, safely,
and cleanly. These are my goals:

- Script-like
	- New systems should have minimal boilerplate and be accessible to
	tweak for low-experience programmers. Syntax matters
- Strong static analysis, including static typing
	- It shouldn't require triggering a logic path to discover a typo
- Minimal VM
	- No static dependency or binary bloat, and re-implementing across
	languages should be easy
- No garbage collection
	- Garbage collection leads to framerate stutter
- Idiomatic and easy ABI bindings
	- Scripting and your game should fit like a glove, and it shouldn't
	require a binding layer

```
// clean syntax
fn main()
	// implicit typing
	var x = 5 + 2 * 6
	assert(x == 17)
	if entity_id_exists(x)
		log("exists")
	else if false
		// tracks line number
		panic()
	while x != 0
		x -= 1

// Easily declare externs
@fn entity_id_exists(id: int) bool
```

rsfg
----

[`rsfg`](rsfg/) is the compiler for the `sfg` language, written in Rust, with more
documentation at [`rsfg/README.md`](rsfg/README.md). the best documentation
for `sfg` is found in the extensive test suite for `rsfg`.

bcfg
----

The bytecode `rsfg` compiles to is `bcfg` (ByteCode for Games). documentation
for the `bcfg` format can be found at [`rvmfg/bcfg.md`](rvmfg/bcfg.md).

rvmfg
-----

i have provided an implementation of a Virtual Machine executing `bcfg`
in Rust, called [`rvmfg`](rvmfg/). This Virtual Machine is intended to be used by
Rust applications (games) to import and run `bcfg` bytecode. usage is
extremely simple, but a `cargo doc --open` should get you started

i do not recommend writing bindings from `rvmfg` to other languages. While
such a binding may be useful, it provides minimal effort saved for the cost
of significant additional tooling and total bloat. `bcfg` is designed so
that VM implementations may be as simple as possible: re-implementation is
an assumed cost.

