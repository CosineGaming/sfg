SFG Language Reference
======================

Within this guide
-----------------

`sfg` programs simply do not stand on their own. They always reside within
a parent program (usually a game). i will refer to "parent program" and
"parent language" to refer to the specific game running the VM, or the
language the VM is implemented in respectively.

`Choicy` is a term i use to make fun of language features that are redundant for
no reason. Choicy things get techbrows riled up but they just stress me out.

Comments and formatting
-----------------------

Line comments are defined with `//`. Multiline comments are not supported, they
are choicy.

Tabs are preferred. Consistent spaces are allowed, despite being choicy. Mixing
indentation will causes errors due to the whitespace blocking model. Aligning
indents with open-parens and other whitespace fuckery (as in Haskell and
;-y languages) is highly discouraged and usually unsupported.

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

Externs
-------

Externs are a first-class citizen in `sfg`. By convention, externs are not
wrapped into syntax-fitting name conventions but rather dual-conventions are
allowed to exist. Externs can be called with `@`. After the `@` sign extern
rules are defined by the VM, including scoping, importing, etc.

Externs are type-inferenced and checked only for consistency. To specify an
extern's signature, declare an `sfg` function signature with `fn@` instead of
`fn` and omit any body

If an extern does not follow `sfg` identifier rules, use `@"'ilovelisp"`

*Design note*:  
The Extern design provide a difficult compromise between two of our fundamental
design principles: "Extremely minimal VM" and "Powerful static analysis". On
one hand, providing automatic bindings to every function removes the need for
plates of boilerplate bindings in a VM, on the other hand, this limits our
ability for static checking of these extern functions.

Our approach takes a hybrid approach, but favors simplicity. Bindings to the
most important and common functions are expected to be provided by the VM.
(TODO: List somewhere). The rest are checked for *consistency within the
program* and can be wrapped by a type-checked function with `fn@`signatures.

### Extern types

Extern types are equally first-class. An extern type is also referenced with `@`
notation, and similarly handled by the VM. An extern type can use `.`
"member-function-like" syntax. No additional `@` is needed.

Strings
-------

Strings are vectors of characters. Non-ASCII characters and Unicode is
not supported. If unicode is needed, `extern types` can be used

Literals use `"double quotes"`. Single quotes are forbidden because they
are choicy. Standard escape characters exist i guess.

String concatenation uses `%`. Each side of `%` *is automatically cast to a
string*.

Operations
----------

The obvious: `+-*%`  
`/` is divide. `5 / 2 == 2` AND `5 / 2 == 2.5` BUT `5 / 2. != 2`. How? Type
checking of int / int occurs in the next stage of inference. I guess.

Floats
------

Integers don't have to be explicitly cast to floats. If there wasn't an error,
your type has changed to float. On the other hand, floats *do* have to be
explicitly cast to integers.

