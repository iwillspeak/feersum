# Syntax Trees

The syntax trees used in the parsing stage in Feersum are based on the idea of
Red / Green trees. Low level green nodes contain no position information. Higher
level red nodes add ofsets and parent pointers to provide the full concrete
syntax tree. We then provide a _third_ layer built on top of this red tree in
the `Feersum.Syntax.Tree` module. Each node in this tree provides a typed, lazy,
and fault-tolerant view over the underlying tree.

For move information about the underlying syntax trees, as well as a discussion
about the implementation in Firethorn, the library that Feersum uses, [check out
this blog post][blog].

 [blog]: https://willspeak.me/2021/11/24/red-green-syntax-trees-an-overview.html
