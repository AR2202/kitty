# kitty (KIdsTryTYpes)

work in progress - not yet usable

## Vision
The kitty programming language aims to provide a simple, approachable way to introduce children to statically typed programming.

## REPL
The REPL can be started by typing `stack run` (provided stack is installed). To exit the REPL, type `quit`.

Some simple operations are currently supported:

### Arithmetic operation

type in the operation, e.g.:

`3 * (8 + 2) -4`

### Booleans

currently supported: `and`, `or`, `xor`, `not`.

`true and (not false)`

### Comparison

you can equality compare all currently supported types.

<,>, >= <= can be used on numbers only.

### checking the type of an expression

you can ask for the type of an expression by prefixing it with the keyword `type`

`type 1 + 2`

`type "hello world"`

`type true and false`

