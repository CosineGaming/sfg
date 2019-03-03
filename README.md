Scripting for Games
===================

> A scripting language with strong static analysis and a minimal VM, made
for game development

This language is intended to *script systems in an ECS with safety and
portability*. These are my goals:

- Script-like
	- New systems should have minimal boilerplate and be accessible to
	tweak for low-experience programmers
- Strong static analysis, including static typing
	- It shouldn't require triggering a logic path to discover a typo
- Extremely minimal VM
	- No static dependency or binary bloat, and re-implementing across
	languages should be easy
- No garbage collection
	- Garbage collection leads to framerate stutter
- Idiomatic and easy ABI bindings
	- Scripting and your game should fit like a glove, and it shouldn't
	require a binding layer

sfg language reference
----------------------

`sfg` is the name of this scripting language. The documentation for this
language can be found in the `lang/` directory of this repository.

Note that this language is **theoretical**, it has not yet been designed.

bcfg reference
--------------

The bytecode designed along with `sfg` is `bcfg` (ByteCode for Games). This
bytecode is intended to be a target of `sfg` compilation. Anyone could write a
VM for this bytecode, or a compiler that targets it. It is designed such that
the VM reading it may be simple. Simplicity within `bcfg` itself is not a goal.

Documentation for the `bcfg` format can be found in the `bcfg/` directory of
this repository.

Note that this format is **theoretical**, it has not yet been designed.

rsfg reference
--------------

`rsfg` is my initial compiler for the `sfg` language. Anyone could make
another compiler, or an interpreter if they wanted. This implementation is
written in *Rust* (hence the name).

`rsfg` is included in this repository for now because it is the only known
implementation of `sfg`. The crate root is the `rsfg/` directory.

Note that this application is **theoretical**, work has not yet begun.

rvmfg reference
---------------

i have provided an implementation of a *Virtual Machine executing `bcfg`*
in *Rust*, called `rvmfg`. This Virtual Machine is intended to be used by
Rust applications (games) to import and run `bcfg` bytecode.

i do not recommend writing bindings from `rvmfg` to other languages. While
such a binding may be useful, it provides minimal effort saved for the cost
of significant additional tooling and total bloat. `bcfg` is designed so
that VM implementations may be as simple as possible: re-implementation is
an assumed cost.

`rvmfg` is included in this repository for now because it is the only known
implementation of a VM for `bcfg`. It can be found in the `rvmfg/` directory.

Note that this library is **theoretical**, it has not yet been implemented.

Non-goals
---------

- Speed
	- **Speed is only a secondary goal**. Most of the frametime of a
	game is spent in physics and rendering. **Static typing was never
	about speed, but about feedback**.
- Package management
	- i think package management is best left up to the OS *in general*,
	but `sfg` is designed to *use the game's codebase as an API*. Any
	package management should be handled by whatever package management
	the game uses.
- Runtime
	- Similarly, i believe the runtime should be provided by the VM, and
	(at least in the opinion of `rvmfg`) the VM should use *the game*
	and its respective runtime, as its runtime. This may result in some
	bindings being written by a VM from the few runtime-requiring elements
	of `bcfg` to its runtime

