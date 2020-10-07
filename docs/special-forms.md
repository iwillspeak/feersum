# Special Forms

In Scheme some forms are not handled as plain function
application. This document lists the planned special forms for the
language, along with their implementation status.

The set of forms are derived from those in [R7RS][r7rs-spec].

[TOC]

## Lambda Expressions

`(lambda <formals> <expression>+)`

Returns an anonymous procedure which binds its arguments to
`<formals>` and then evaluates a number of `<expression>`s in the new
environment.

Formals can be either a single identifier to bind the parameter list
to, or a list which the parameters should be matched against. If the
list contains `.` then following parameters are bound to a list to
following argument.

e.g.:

The expression `(lambda a (+ 1 2))` defines a procedure that takes any
arguments and binds the list to `a` in the lambda's environment.

The expression `(lambda (b) (+ 1 2))` defines a procedure that takes a
single argument and binds that to `b` in the lambda's environment.

The expression `(labmda (c . d) (+ 1 2))` defines a procedure that takes
a single argument and an optional number of following arguments. The
first argument is bound to `c` in the lambda's environment and all
remaining arguments are bound to a list `d` in the lambda's environment.

### Implementation status

All forms are lambdas are bound and lowered. Lambdas are implemnted as instances
of `Func<object[],object>`. Implementation was live-streamed. Checkout [YouTube]
for the stream archive.

## Lexical Bindings

```scheme
(let (<init>+) <expression>+)
(let* (<init>+) <expression>+)
(letrec (<init>+) <expression>+)
```

Let bindings create a new environment and introduce bindings to
them. In the first 'basic' `let` each `init` is evaluated in the
parent environment. For a `let*` each initialiser is evaluated in the
child environment sequentially allowing the new bindings to reference
previous ones. In the final `letrec` form each new binding is
initialised with some placeholder value before the intialiser
expressions are evaluated in the child environment allowing full
mutual recursion.

### Implementation status

Lexical bindings are partially implemneted. Bindings in lambda context work.
Bindings do not work properly for global variables that shadow.

## Definitions

```scheme
(define <variable> <expression>?)
(define <formals> <expression>+)
```

Define statements bind variables to new storage location and assign
them a value. Simple `define` forms without an <expression> assign
some nominal unknown value. Definitions can be mutually recursive
within a `lambda`, but only up to the first non-definition expression.

Procedure definitions are just lambdas bound to the name that occurs
as the first element of the `<formals>` definition.

### Implementation status

Variable definitions are bound and lowered. Function definitions are bound
as definitions of lambdas and rely on `lambda` support for lowering.

## Assignments

```scheme
(set! <variable> <expression>)
```

Set expressions assign the result of `<expression>` to the location
that `<variable>` is currently bound to. If `<variable>` is unbound
then the expression raises an error.

### Implementation status

Assignments are bound and lowered.

## Quoting

```scheme
(quote <expression>)
'<expression>
```

Quote expressions evaluate to the external representation of the
expression rather than evaluating the expression directly. For
literals there is no difference. For lists this skips application and
allows creation of lists directly.

### Implementation status

Quoring is only impelmented in the interpreter so far.

## Conditionals

```scheme
(if <cond> <if-true> <if-false>?)
```

An if expression evaluates the `<cond>`ition followed by one of either
`<if-true>` or `<if-false>`. This defers the evaluation of the
consequent expression until after the variable check.

```scheme
(and <expression>?)
(or <expression>?)
```

Boolean conditional expressions. Both lazily evaluate the given
expression returning the result of the last expression that needed to
be evaluated to derive the overall truthiness of the expression. An
empty `and` is implicitly truthy, an empty `or` implicitly falsey.

```scheme
(cond <clause>+)
```

Where each `<clause>` is `(<predicate> <expression>+)`. Evaluates the
first `<clause>`'s body where the `<predicate>` is truthy. The final
predicate _may_ be `else`, which is always considered truthy.

```scheme
(case <key> <case>+)
```

Where `<case>` is `((<value>+) <expression>+)`. Evaluates `<key>` once
and then evaluates the body of whichever `<case>` contains the
resulting value in `<value>`s. The final case may use `else` in place
of the value list to provide a default.

### Implementation status

The `if` conditional special form is implemnted. The remaining forms
could be implemneted either directly or wait for macros.

## Sequence

```scheme
(begin <expression>+)
```

The `begin` expression evaluates each inner `expression` in sequence
and returns the result of the final expression.

### Implementation status

The begin special form is implemented.

 [r7rs-spec]: https://small.r7rs.org/
 [YouTube]: https://www.youtube.com/playlist?list=PLCum1jXOlhoRCBewbQD8ELE7B_7EWnWaO