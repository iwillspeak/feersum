# Values and Types

Values in Feersum are all CIL objects. Value types are generally boxed types.
The empty list is represented by `null`.

[TOC]

## Boolean

Booleans can be created with `#t`, `#true`, `#f`, and `#false`. 

### Implementation status

Booleans are represented as boxed `bool` instances of `System.Boolean`.

## Vector

Vector literals are contiguous array types. Vectors are lists prefixed by `#(`.
e.g.: `#(123 456 789)`.

### Implementation Status

Vectors are not yet supported.

## Bytevector

Byte vector literals are similar to vectors but contain arrays of byte values.
Byte vectors are lists prefixed by `#u8(`. e.g.: `#u8(0 10 5)`.

### Implementation status

Byte vectors are not yet supported.

## Char

Characters are single textual characters. Character literals consist of a `#\`
followed by a literal character or character name. Arbitrary unicode values can
also be encoded as `#\x` followed by a hexedecimal character value.

### Implementation status

Characters are not yet supported.

## Null

Null, or the empty list, is represented as `()`. 

### Implementation status

Null is represented by the CIL `null` value.

## Number

Numbers in scheme form a "tower of sub-types". There are several different
numeric types any of which may be supported, however the relationship between
them must be consistent.

The tower is:

 * number
 * complex number
 * real number
 * rational number
 * integer

Number literals can be either integer or floating point literals, or constant
values defined in other bases such as `#x` for hex and `#o` for octal.

### Implementation Status

Inexact numbers are the only supported number type right now. All numbers are
represented as boxed `double` instances of `System.Double`. Only floating point
or integer literals are supported.

## Pair

Pairs, or "cons pairs" are the building block of lists. Pairs are represented
as `(<car> . <cdr>)` where `<car>` is the 'left hand' or 'head' of the pair and
`<cdr>` is the 'right hand' or 'tail' portion.

### Implementation Status

Cons pairs are not yet supported. Pair literals are not yet handled or bound.

## Port

Ports are used for reading and writing data. The obvious choice for them would
be some form of wrapper around streams.

### Implementation status

Ports are not yet supported.

## Procedure

Procedures are callable values. New procedures can be introduced with `lambda`
`define` speical forms.

### Implementation Status

In Feersum procedures follow a 'unified calling convention' of passing all
arguments as a single CIL array of objects. Procedures are repsonisble for
unpacking their own arguments and raising errors as appropriate.

When lambda defintiions are used to create procedures a 'thunk' is generated
which unpacks the arguments into the target's parameters. Lambda values are
passed around as `System.Func<object[], objec>` instances.

## String

Strings are arrays or sequences of characters. String literals are enclosed in
`"`. Characters can also use hexedecimal or character escapes `\` foloowed by a
character or character name, or `\x` followed by a hex literal followed by `;`.

### Implementation Status

Strings are represented as .NET `String` instances. String literals and
character escapes are supported. Not _all_ Unicode characters can be specified
in character escapes due to the UTF-16 reprensetaion of characters used in .NET.

## Symbol

Symbols are quoted identifiers. Symbols behave like interned strings.

### Implementation status

Symbols are not yet implemented.
