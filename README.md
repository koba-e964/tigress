# Tigress
## Overview
This project contains a tiny interpreter/compiler for Tigress, a subset of Tiger language ([reference](http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf)).

## Grammar
The grammar of Tigress is similar to Tiger, but there are some modifications. The major difference between them is the array creation. While we create an array by `type-id [ expr ] of expr` in Tiger, we create one by `new type-id [ expr ] OF expr` (for the simplicity of parser) in Tigress. Besides, there are some features that are not supported in Tigress.

## functionality
|item|status|
|---|---|
| 1 Lexical Aspects | Strings are not supported |
| 2.1 Lvalues | ok |
| 2.2 Return values | ok |
| 2.3 Record and Array Literals | not supported |
| 2.4 Function Calls | ok |
| 2.5 Operators | ok |
| 2.6 Assignment | ok |
| 2.7 nil | not supported |
| 2.8 Flow control | ok? |
| 2.9 Let | partially supported |
| 3 Declarations| ok |
| 4 Standard Library | not complete |

