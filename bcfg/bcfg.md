bcfg
====

This documents the whole of the bcfg format. Notice that i only update this
document when i think of it, the most up-to-date documentation is in
`rsfg/src/codegen.rs:serialize` and `rvmfg/src/thread.rs` in general. (The
VM is only a few hundred lines, it can be run and understood fairly quickly.)

Integer literals are little endian **signed** integers

bcfg header
-----------

Begins with magic numbers `bcfg`

Then includes Function Headers **and then** Extern Function Headers **and
then** Static String Declarations strictly in that order.

Then it's unspecified, but at some point code begins (as long as codelocs point
to it, can be anywhere).

Function headers
----------------

Headers include some extra information about functions to ease in ABI. A
function header looks like:

- FnHeader OR ExternFnHeader
- return type
- number of parameters
- [each parameter's type]
- utf-8 function name ending in \0
- codeloc ONLY if intern header, no codeloc for extern header

codeloc is the location within the binary of the first instruction of the
function.

String declarations
-------------------

- StringLit
- utf-8 string ending in \0

Instructions table
------------------

| name           | hex | description                                                   |
| ----           | ---:| -----------                                                   |
| Sep            |  00 | separates many things                                         |
| Int            |  10 | int type                                                      |
| Str            |  11 | str type                                                      |
| Void           |  21 | void type (function returns only!)                            |
| Push32(32)     |  30 | push following 32 bits to stack                               |
| ExternFnCall(i)|  31 | lookup extern function by i and call                          |
| StringLit      |  32 | declare string literal (in headers)                           |
| FnHeader       |  33 | declare function header                                       |
| ExternFnHeader |  34 | declare extern function header                                |
| Return         |  35 | return to ip in call stack                                    |
| FnCall(i)      |  36 | lookup function by iand call                                  |
| Pop32          |  37 | pop 32 bits off the stack and discard                         |
| Equals         |  38 | evaluate two pops for equality, leaving boolean               |
| JumpZero(lines)|  39 | jump by lines if pop is 0                                     |
| Dup(n)         |  3a | duplicate the nth-down element to top of stack                |
| Panic          |  3b | display panic message with line/col off stack, clean up state |
| Add            |  3c | add top two pops, leaving result                              |
| Sub            |  3d | subtract pop2 - pop1, leaving result                          |

**Note**: Types are 1x, Other 2x, Instructions 3x

Standard library
----------------

Though not specific to bcfg, sfg programs expect a certain standard library to
be provided with the VM. This is kept minimal and implementations undemanding.
The best reference of the standard library is simply reading
`rvmfg/src/sfg_std.rs`.

