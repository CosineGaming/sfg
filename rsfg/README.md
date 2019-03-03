rsfg
====

This is a rust implementation of a compiler for `sfg`, and the original
canonical one. For docs on the `sfg` language see ../lang/lang.md .

Because it's in early stages of development, rsfg implements a rough subset
of the sfg language. The best way to find out what's *currently implemented*
is the file tests/scripts/without-errors.sfg . As i add language features,
i add them to this file as part of the testing process. Using this along
with the language docs should be adequate information.

