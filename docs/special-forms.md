# Special Forms

In Scheme some forms are not handled as plain function
application. This document lists the planned special forms for the
language, along with their implementation status.

The set of forms are derived from those in [MIT-Scheme][mit-specials].

## Lambda Expressions

`(lambda <formals> <expression>+)`

Returns an anonymous procedure which binds its arguments to
`<formals>` and then evaluates a number of `<expression>`s in the new
environment.

Formals can be either a single identifier to bind the parameter list
to, or a list which the parameters should be matched against. If the list contains `.` then following paramete

e.g.:

The expression `(lambda a (+ 1 2))` defines a procedure that takes any
arguments and binds the list to `a` in the lambda's environment.

The expression `(lambda (b) (+ 1 2))` defines a procedure that takes a
single argument and binds that to `b` in the lambda's environment.

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

## Assignments

```scheme
(set! <variable> <expression>?)
```

Set expressions assign the result of `<expression>` to the location
that `<variable>` is currently bound to. If `<variable>` is unbound
then the expression raises an error.

## Quoting

```scheme
(quote <expression>)
'<expression>
```

Quote expressions evaluate to the external representation of the
expression rather than evaluating the expression directly. For
literals there is no difference. For lists this skips application and
allows creation of lists directly.

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

# Sequence

```scheme
(begin <expression>+)
```

The `begin` expression evaluates each inner `expression` in sequence
and returns the result of the final expression.

 [mit-specials]: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Special-Forms.html
