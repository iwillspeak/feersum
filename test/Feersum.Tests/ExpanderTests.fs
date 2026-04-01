module ExpanderTests

open Xunit
open Feersum.CompilerServices.NewBindingTest
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

// ── Helpers ──────────────────────────────────────────────────────────────────

let private registry = SourceRegistry.empty ()

/// Parse a Scheme program string and run the new expander.
/// Returns (BoundExpr list, Diagnostics).
let private expand (source: string) =
    let prog = Parse.readProgramSimple "test" source

    if ParseResult.hasErrors prog then
        failwithf "Parse error in '%s': %A" source prog.Diagnostics

    let ctx = ExpandCtx.createGlobal registry "test" []
    let exprs = Expand.expandProgram prog.Root SyntaxEnv.builtin ctx
    exprs, ctx.Diagnostics.Diagnostics

/// Expand and assert there are no errors; return the bound expressions.
let private expandOk (source: string) =
    let exprs, diags = expand source

    let errors = diags |> List.filter (fun d -> d.Kind.Level = DiagnosticLevel.Error)

    if not (List.isEmpty errors) then
        let msgs = errors |> List.map (fun d -> d.Message) |> String.concat "; "
        failwithf "Unexpected errors in '%s': %s" source msgs

    exprs

/// Expand and assert that at least one error matches the given substring.
let private expandError (needle: string) (source: string) =
    let _, diags = expand source

    let errors = diags |> List.filter (fun d -> d.Kind.Level = DiagnosticLevel.Error)

    let found =
        errors |> List.exists (fun d -> d.Message.Contains(needle))

    if not found then
        let msgs = errors |> List.map (fun d -> $"[{d.Kind.Code}] {d.Message}") |> String.concat "; "
        failwithf "Expected error containing '%s' but got: %s" needle msgs

/// Parse a `(syntax-rules ...)` Stx node from source.
let private parseSyntaxRules (name: string) (source: string) : SyntaxTransformer =
    let prog = Parse.readProgramSimple "test" source

    if ParseResult.hasErrors prog then
        failwithf "Parse error: %A" prog.Diagnostics

    let tree =
        prog.Root.Body
        |> Seq.exactlyOne
        |> (fun expr -> Stx.ofExpr registry prog.Root.DocId expr)

    let diag = DiagnosticBag.Empty
    let env = SyntaxEnv.builtin

    match MacrosNew.parseSyntaxRulesStx name tree env diag TextLocation.Missing with
    | Some t -> t
    | None ->
        let msgs =
            diag.Diagnostics
            |> List.map (fun d -> d.Message)
            |> String.concat "; "

        failwithf "parseSyntaxRulesStx returned None: %s" msgs

// ── MacrosNew parser tests ─────────────────────────────────────────────────

[<Fact>]
let ``parseSyntaxRulesStx: simple identity rule`` () =
    let t = parseSyntaxRules "id" "(syntax-rules () ((_ x) x))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Underscore; StxPattern.Variable "x" ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: constant literal pattern`` () =
    let t = parseSyntaxRules "foo" "(syntax-rules () ((_ 123) 'ok))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Underscore; StxPattern.Constant(BoundLiteral.Number n) ] ->
        Assert.Equal(123.0, n)
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: keyword literal in literal list`` () =
    let t = parseSyntaxRules "my-macro" "(syntax-rules (else) ((_ else x) x))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Underscore; StxPattern.Literal "else"; StxPattern.Variable "x" ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: ellipsis pattern`` () =
    let t = parseSyntaxRules "my-list" "(syntax-rules () ((_ x ...) (list x ...)))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Underscore; StxPattern.Repeat(StxPattern.Variable "x") ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: dotted rest pattern`` () =
    let t = parseSyntaxRules "f" "(syntax-rules () ((_ a b . c) c))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.DottedForm([ StxPattern.Underscore; StxPattern.Variable "a"; StxPattern.Variable "b" ],
                             StxPattern.Variable "c") -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: multiple rules`` () =
    let t =
        parseSyntaxRules "foo" "(syntax-rules () ((_ 123) 'one-two-three) ((_) \"bar\"))"

    Assert.Equal(2, List.length t.Patterns)

// ── End-to-end expand tests ───────────────────────────────────────────────

[<Fact>]
let ``expand: identity macro`` () =
    let exprs =
        expandOk
            """
(define-syntax id
  (syntax-rules ()
    ((_ x) x)))
(id 42)
"""

    // Last expression should be Load or Literal 42
    let last = List.last exprs

    match last with
    | BoundExpr.Literal(BoundLiteral.Number 42.0) -> ()
    | other -> failwithf "Expected Literal 42, got %A" other

[<Fact>]
let ``expand: constant pattern match`` () =
    let exprs =
        expandOk
            """
(define-syntax foo
  (syntax-rules ()
    ((_ 123) '(1 2 3))
    ((_) "bar")))
(foo 123)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Quoted _ -> ()
    | other -> failwithf "Expected Quoted datum for (foo 123), got %A" other

[<Fact>]
let ``expand: no-arg constant pattern (foo)`` () =
    let exprs =
        expandOk
            """
(define-syntax foo
  (syntax-rules ()
    ((_ 123) '(1 2 3))
    ((_) "bar")))
(foo)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Literal(BoundLiteral.Str "bar") -> ()
    | other -> failwithf "Expected Literal \"bar\" for (foo), got %A" other

[<Fact>]
let ``expand: ellipsis macro`` () =
    // Use `begin` (a special form, always in env) rather than `list` (a builtin).
    let exprs =
        expandOk
            """
(define-syntax my-begin
  (syntax-rules ()
    ((_ x ...) (begin x ...))))
(my-begin 1 2 3)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Seq items -> Assert.Equal(3, List.length items)
    | BoundExpr.Literal _ -> () // degenerate: begin with 1 item simplified
    | other -> failwithf "Expected Seq of 3 from begin expansion, got %A" other

[<Fact>]
let ``expand: let-syntax scoping`` () =
    // The inner binding should shadow the outer define-syntax.
    let exprs =
        expandOk
            """
(define-syntax foo
  (syntax-rules ()
    ((_) "outer")))
(let-syntax
  ((foo (syntax-rules ()
          ((_) "inner"))))
  (foo))
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Seq _ -> () // let-syntax body
    | BoundExpr.Literal(BoundLiteral.Str "inner") -> ()
    | other -> failwithf "Expected inner string from let-syntax, got %A" other

[<Fact>]
let ``expand: hygienic rename`` () =
    // 'x' inside swap! body should not capture the caller's 'x'
    let exprs =
        expandOk
            """
(define-syntax swapit
  (syntax-rules ()
    ((_ a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))
(define x 1)
(define y 2)
(swapit x y)
"""

    // Should expand without errors is the key assertion; full hygiene
    // check needs runtime eval, but we can verify no expand errors here.
    Assert.NotEmpty(exprs)

[<Fact>]
let ``literal pattern: keyword must appear literally`` () =
    // 'else' must match as a literal, not as a variable.
    let exprs =
        expandOk
            """
(define-syntax my-cond
  (syntax-rules (else)
    ((_ else e) e)
    ((_ test e) (if test e))))
(my-cond else 99)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Literal(BoundLiteral.Number 99.0) -> ()
    | other -> failwithf "Expected Literal 99 from else branch, got %A" other

[<Fact>]
let ``parseSyntaxRulesStx: keyword-name head binds as variable`` () =
    // When the head element matches the keyword name, it becomes Variable kw
    // so the template can refer to it (e.g. recursive `or` expansion).
    let t = parseSyntaxRules "or" "(syntax-rules () ((or a b ...) (if a a (or b ...))))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Variable "or"; StxPattern.Variable "a"; StxPattern.Repeat(StxPattern.Variable "b") ] ->
        // Template should have or as Subst (it's in bound)
        match snd t.Patterns.[0] with
        | StxTemplate.Form(_, elems) ->
            // The (if ...) form; just verify it parsed without error
            Assert.NotEmpty(elems)
        | other -> failwithf "Expected Form template, got %A" other
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``expand: recursive macro via keyword-head binding`` () =
    // Basic `or` that uses recursive `(or ...)` in template head position.
    let exprs =
        expandOk
            """
(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((t e1))
       (if t t (my-or e2 ...))))))
(my-or #f #f 42)
"""

    Assert.NotEmpty(exprs)


    expandError "no macro rule" """
(define-syntax only-numbers
  (syntax-rules ()
    ((_ 42) 'ok)))
(only-numbers "wrong")
"""

// ── Custom ellipsis (R7RS extended syntax-rules form) ────────────────────

[<Fact>]
let ``parseSyntaxRulesStx: custom ellipsis identifier`` () =
    // R7RS extended form: (syntax-rules <ellipsis> (literals...) rules...)
    // The custom ellipsis `dots` should be usable in place of `...`.
    let t = parseSyntaxRules "my-seq" "(syntax-rules dots () ((my-seq expr dots) (begin expr dots)))"
    Assert.Single(t.Patterns) |> ignore

    match fst t.Patterns.[0] with
    | StxPattern.Form [ StxPattern.Variable "my-seq"; StxPattern.Repeat(StxPattern.Variable "expr") ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``expand: custom ellipsis macro`` () =
    // A macro defined with a non-`...` ellipsis identifier must work end-to-end.
    let exprs =
        expandOk
            """
(define-syntax my-seq
  (syntax-rules dots ()
    ((my-seq expr dots)
     (begin expr dots))))
(my-seq 1 2 3)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Seq items -> Assert.Equal(3, List.length items)
    | BoundExpr.Literal _ -> () // single-item begin simplified
    | other -> failwithf "Expected Seq of 3 from (my-seq 1 2 3), got %A" other

// ── Dotted-tail pattern matching against proper lists ────────────────────

[<Fact>]
let ``expand: dotted-tail pattern (underscore) matches proper list tail`` () =
    // The pattern `(_ . _)` should match any call with at least zero args.
    // This tests that DottedForm is tried against Stx.List nodes.
    let exprs =
        expandOk
            """
(define-syntax count-to-2
  (syntax-rules ()
    ((_) 0)
    ((_ _) 1)
    ((_ _ _) 2)
    ((_ . _) 'many)))
(count-to-2 a b c d)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Quoted _ -> () // 'many
    | other -> failwithf "Expected Quoted 'many for (count-to-2 a b c d), got %A" other

[<Fact>]
let ``expand: dotted-tail pattern selects over fixed-arity rules`` () =
    // Verify fixed-arity rules still take priority over the dotted catchall.
    let exprs =
        expandOk
            """
(define-syntax count-to-2
  (syntax-rules ()
    ((_) 0)
    ((_ _) 1)
    ((_ _ _) 2)
    ((_ . _) 'many)))
(count-to-2)
"""

    let last = List.last exprs

    match last with
    | BoundExpr.Literal(BoundLiteral.Number 0.0) -> ()
    | other -> failwithf "Expected Literal 0 for (count-to-2), got %A" other

// ── Macro-generated define-syntax ────────────────────────────────────────

[<Fact>]
let ``expand: macro generates top-level define-syntax via begin`` () =
    // A macro that expands to (begin (define march-hare 42) (define-syntax hatter ...))
    // must splice the generated define-syntax into the top-level environment so
    // subsequent code sees `hatter` as a macro.
    let exprs =
        expandOk
            """
(define-syntax jabberwocky
  (syntax-rules ()
    ((_ hatter)
     (begin
       (define march-hare 42)
       (define-syntax hatter
         (syntax-rules ()
           ((_) march-hare)))))))
(jabberwocky mad-hatter)
(mad-hatter)
"""

    Assert.NotEmpty(exprs)

[<Fact>]
let ``expand: macro directly expands to define-syntax`` () =
    // Simpler case: the macro expansion IS a define-syntax (no wrapping begin).
    let exprs =
        expandOk
            """
(define-syntax def-const
  (syntax-rules ()
    ((_ name val)
     (define-syntax name
       (syntax-rules ()
         ((_) val))))))
(def-const the-answer 42)
(the-answer)
"""

    Assert.NotEmpty(exprs)

[<Fact>]
let ``expand: macro generates define-syntax in let body`` () =
    // A macro call inside a let body that expands to a define-syntax must make
    // the newly defined macro visible to subsequent forms in the same body.
    let exprs =
        expandOk
            """
(let ()
  (define-syntax make-const
    (syntax-rules ()
      ((make-const name val)
       (define-syntax name
         (syntax-rules ()
           ((_) val))))))
  (make-const answer 42)
  (answer))
"""

    Assert.NotEmpty(exprs)

[<Fact>]
let ``expand: be-like-begin pattern with custom ellipsis`` () =
    // be-like-begin3 defines a macro with a custom ellipsis `dots` inside its
    // template. This stresses both macro-generated define-syntax and custom
    // ellipsis inside transcribed (Closure-wrapped) syntax-rules forms.
    let exprs =
        expandOk
            """
(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules dots ()
         ((name expr dots)
          (begin expr dots)))))))
(be-like-begin my-sequence)
(my-sequence 1 2 3)
"""

    Assert.NotEmpty(exprs)
