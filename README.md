# kitty (KIdsTryTYpes)

work in progress - not yet usable

## Vision
The kitty programming language aims to provide a simple, approachable way to introduce children to statically typed programming.

## REPL
The REPL can be started by typing `stack run` (provided stack is installed). To exit the REPL, type `quit`.

Some simple operations are currently supported:

### Arithmetic operation

type in the operation, e.g.:

`3 * (8 + 2) - 4`

### Booleans

currently supported: `and`, `or`, `xor`, `not`.

`true and (not false)`

### Comparison

you can equality compare all currently supported types.

<,>, >= <= can be used on numbers only.

`2 > 1`

### Variables

variables are declared and assigned at the same time:

`myvar = 5`

Type annotations are not required (and currently not supported) as kitty has type inference.
(Optional) type annotations might be supported in the future.

### If statements

the syntax for if/else is:

`if ... then ... endif` 

or

`if ... then ... else ... endif`

It is possible to assign varibles within if statements which persist beyond the if/else block.

Eg.

`if (5 > 2) then myvar = 5 else myvar = "hello world" endif`

The variable myvar would then have type oneOf wholeNumber or text and has to be destructured before use. This would result in a type error:

`myvar + 2`

typeError: 

To unwrap a oneOf:

`unwrap myvar as wholeNumber named myUnwrappedVar andDo myUnwrappedVar + 2 endunwrap`

If the different branches assign the same type to the variable, kitty unifies them automatically and unwraps them from the oneOf. This works:

`if true then mynewvar = 1 else mynewvar = 2 endif`

`mynewvar + 5`

### while loops

while loops can be used to execute code repeatedly, as long as
a specific condition evaluates to true.
`while [condition] do [code block] endwhile`

#### Examples

`x = 1`

`while (x < 3) do x = x + 1 endwhile` 

`x`

x should now be 3.
### print statements

Only values of type text or letter can be printed.

`print("hello world")`

This will result in a type error:

`print(2)`

### using source files

Instead of entering code in the REPL, it can be saved to a source file.
The file can be loaded and evaluated with `evalFile [filepath]`.

### checking the type of an expression

you can ask for the type of an expression by prefixing it with the keyword `type`

`type 1 + 2`

`type "hello world"`

`type true and false`

`type myvar`


### Types
The names of kitty's types are a bit different from what you might be used to. This is because the names of types usually used in programming might be unfamiliar to kids.

Currently, kitty has the following types:
* wholeNumber (corresponding to int)
* decimalNumber (corresponding to float)
* text (corresponding to string)
* letter (corresponding to char)
* truth (corresponding to bool)
* empty (corresponding to void)
* oneOf type1 or type2 (a union type of type1 type2; corresponding to, e.g., Either in Haskell or Result in Rust)
