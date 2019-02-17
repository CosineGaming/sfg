SFG Language Reference
======================

Within this guide
-----------------

`sfg` programs simply do not stand on their own. They always reside within
a parent program (usually a game). i will refer to "parent program" and
"parent language" to refer to the specific game running the VM, or the
language the VM is implemented in respectively.

Comments and formatting
-----------------------

Line comments are defined with `//`. Multiline comments are not supported at
this time.

Tabs are preferred. Consistent spaces are allowed. Mixing indentation will
causes errors due to the whitespace blocking model. Aligning indents with
open-parens and other whitespace fuckery (as in Haskell and ;-y languages) is
highly discouraged and usually unsupported.

Function definition
-------------------

A fully-qualified function definition looks like this:

```sfg
fn add_person(age: num, name: string) result
	// [BODY]
	return result@ok
```

**All functions in `sfg` are public**. i recommend against using `_` prefixes
and such and instead operating under an understanding of publicity. These are
scripts, and not meant to be systems programming.

