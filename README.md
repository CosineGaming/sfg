Scripting for Games
===================

> A performant scripting language with strong static analysis, made for
game development

This language is intended to script systems in an ECS scalably, safely,
and cleanly. These are my goals:

- Performant * (>50% speed of lua)
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

If your `cargo test` is failing, you probably haven't generated the
bytecode. Since the VM doesn't depend on the compiler, and the bytecode is
not yet incredibly stable, you'll want to be in the `rsfg` dir and run
`tests/update_rvmfg_tests.sh`.

comparison to other projects
----------------------------

pro is more like "exceeds expectations of sfg" and con is "doesn't match
expectations of sfg," it's not a value judgement on language design

## [dyon](https://github.com/PistonDevelopers/dyon)

pros

- feature rich / far along in development
    - implements all goals of sfg except those in cons list and more
- neat new syntax like
    - global closures ([current objects](https://github.com/PistonDevelopers/dyon/issues/224))
    - [inferred for loops](https://github.com/PistonDevelopers/dyon/issues/116)

cons

- targetting rust only (may be true of others)
- no intermediate representation / non-compiler-driven
- { braces }
- immutable-by-default\*

\* games are all about mutable state. sfg plans a mutation detector that
allows for ECS to efficiently dispatch without requiring mut tags on nearly
every variable

## [lily](https://fascinatedbox.gitlab.io/lily-docs/)

pros

- only always-statically-typed lang i found

cons

- OOP
- tracing + RC GC
- braces

## [haxe](https://haxe.org/)

pros

- performant, battle-tested VM with intermediate representation and static analysis

cons

- OOP
- tracing GC
- really unscripty syntax (){};

## [rhai](https://github.com/jonathandturner/rhai)

pros

- very strong host-side API
- some hot reloading

cons

- {};

garbage collection model unknown

## [mun](https://github.com/mun-lang/mun)

pros

- fully statically typed
- first-class hot reloading

cons

- while AOT is nice, it's to platform-specific code
- {};

GC model unknown

**go ahead and let me know if i have mischaracterized your project or if
you're aware of another project in this niche, i'd love to research it**

