bcfg
====

See table of values for the actual bytes used, i use assembly-like names in
this document.

Header
------

Begins with magic numbers `bcfg`

```
stack_size [return]type num_params [parameters] type type [etc] name\0 code_loc
[a second function, until all functions represented]
```

Word literals are little endian

Function definition
-------------------

```
call code_loc argument argument [etc]
```

argument is defined by `(literal | local)`

Locals
------

the parameters defined in the header indicate the number of each local.
followed by the stack defined in the function `local 2` would refer to the
third parameter

Values table
------------

| name | dec    | ascii | description |
| ---- | ------:| ----- | ----------- |
| sep  | 0      |       | separates many things |
| type |        |       | a type can be any of the bcfg types |
| name |        |       | the string name of the function in the source |
| stack_size |  |       | the number of bytes to reserve for the stack when calling this function (word literal) |
| code_loc |    |       | the index within the compiled program of the function's entry point |
| call | 99     | c     | call        |
| local | 108   | l     | the following is a local variable number |
| literal | 76  | L     | the following is a literal number |
| int  | 105    | i     | int type    |

