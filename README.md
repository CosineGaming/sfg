Scripting for Games
===================

> A performant scripting language with strong static analysis, made for
game development

This language is intended to script systems in an ECS scalably, safely,
and cleanly. These are my goals:

- Performant
	- games are one of the most performance-critical softwares we
	make these days
- Script-like *
	- new systems should have minimal boilerplate and be accessible to
	tweak for low-experience programmers. Syntax matters
- Strong static analysis, including static typing *
	- it shouldn't require triggering a logic path to discover a typo,
	and dynamic typing was never any easier than type inference
- Easy to integrate VM for C, C++, and Rust
	- goal is to maintain within LUAJit binary size *
	- the compiler is considered a dev dependency, but games are big
	these days so the VM is allowed some space
- No stop-the-world garbage collection
	- stop-the-world garbage collection is a non-starter for game
	performance.
	- counterpoint: ARC can actually be reasonably consistently performant
	if you use reasonable data structures (no DAGs, no LLs), which is
	reasonable in game scripting. but if i can figure out how to do it
	statically, that's a win for performance and elegance
- Idiomatic and compact ABI bindings
	- scripting and your game should fit like a glove, and it shouldn't
	require a binding layer. set name rewrite rules and use a simple
	extern function syntax or write one-line typed function bindings

\* already implemented

sfg is a work-in-progress, here's what we have so far:

```
// clean syntax
fn main()
	// implicit typing
	var x = 5 + 2 * 6
	assert(x == 17)
	if extern_from_binding(x)
		log("exists")
	else if @inline_externs("untyped")
		// tracks line number
		panic()
	while x != 0
		x -= 1

// Easily declare externs
@fn extern_from_binding(id: int) bool
```

rsfg
----

[`rsfg`](rsfg/) is the compiler for the `sfg` language, written in Rust, with more
documentation at [`rsfg/README.md`](rsfg/README.md). the best documentation
for `sfg` is found in the extensive test suite for `rsfg`.

rvmfg
-----

the Virtual Machine is also written in Rust, but with a clean C interface,
in [`rvmfg`](rvmfg/). `cargo doc --open` should get you started

the bytecode is not necessarily planned to be stable in step with
the VM even after 1.0, but a best-effort reference is located at
[`rvmfg/bcfg.md`](rvmfg/bcfg.md). once the VM is much more stable, perhaps
the bytecode will be stabilized and other VMs could be implemented

