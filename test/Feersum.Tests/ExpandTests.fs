module ExpandTests

open Xunit
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

[<AutoOpen>]
module private Helpers =

    let private illuminationCtx doc : IlluminationCtx =
        { Doc = doc
          Diagnostics = DiagnosticBag.Empty }

    let private expandCtx doc : StxCtx =
        { Doc = doc
          Diagnostics = DiagnosticBag.Empty
          Env = Map.empty }

    /// Parse a string, illuminate every expression, expand each in sequence
    /// (threading the ctx so define bindings are visible to later forms), and
    /// return the list of resulting Stx values.
    let expandProgram (src: string) : Stx list =
        let doc = TextDocument.fromParts "test" src
        let parsed = Parse.readProgram doc.Path src

        if Parse.ParseResult.hasErrors parsed then
            failwithf "Parse errors in '%s': %A" src parsed.Diagnostics

        let ictx = illuminationCtx doc

        let illuminated =
            parsed.Root.Body |> Seq.map (Expand.illuminateExpr ictx) |> List.ofSeq

        let ctx0 = expandCtx doc

        let ctx, revResults =
            List.fold
                (fun (ctx, acc) stx ->
                    let ctx', result = Expand.expand ctx stx
                    let acc' = result :: acc
                    ctx', acc')
                (ctx0, [])
                illuminated

        // Fail loudly if the expander emitted any diagnostics
        let diags = ctx.Diagnostics.Take

        if diags |> List.isEmpty |> not then
            failwithf "Expand diagnostics: %A" diags

        revResults |> List.rev |> List.choose id

    /// Expand a single form (no env threading needed).
    let expandOne (src: string) : Stx =
        match expandProgram src with
        | [ stx ] -> stx
        | forms -> failwithf "Expected exactly one form, got %d in '%s'" (List.length forms) src

    // ------------------------------------------------------------------
    // Stx traversal helpers
    // ------------------------------------------------------------------

    /// Collect all Symbol nodes in a Stx as (name, stamp) pairs, in
    /// depth-first left-to-right order.
    let rec collectSymbols (stx: Stx) : (string * int) list =
        match stx with
        | Stx.Symbol(id, _) -> [ id.Name, id.Stamp ]
        | Stx.Literal _ -> []
        | Stx.Form(body, tail, _) ->
            List.collect collectSymbols body
            @ (tail |> Option.map collectSymbols |> Option.defaultValue [])
        | Stx.Closure(inner, _) -> collectSymbols inner

    /// Return all symbols that appear as the formal parameters of the
    /// outermost lambda in `stx`, as (name, stamp) pairs.
    let formalsOf (stx: Stx) : (string * int) list =
        match stx with
        | Stx.Form(Stx.Symbol({ Name = "lambda" }, _) :: formals :: _, _, _) -> collectSymbols formals
        | _ -> failwithf "Not a lambda form: %A" stx

    /// Return just the body expressions of the outermost lambda (everything
    /// after the formals).
    let bodyOf (stx: Stx) : Stx list =
        match stx with
        | Stx.Form(Stx.Symbol({ Name = "lambda" }, _) :: _ :: body, _, _) -> body
        | _ -> failwithf "Expected a two-element+ form: %A" stx

    /// Look up the stamp for a name among a list of (name, stamp) pairs.
    /// Fails if the name is not found.
    let stampOf (name: string) (pairs: (string * int) list) : int =
        match pairs |> List.tryFind (fst >> (=) name) with
        | Some(_, s) -> s
        | None -> failwithf "Name '%s' not found in %A" name pairs

    /// Assert two stamps are equal.
    let assertSameStamp (expected: int) (actual: int) = Assert.Equal<int>(expected, actual)

    /// Assert a stamp is fresh (not the global stamp 0).
    let assertFreshStamp stamp =
        Assert.NotEqual(Ident.GlobalStamp, stamp)

// ---------------------------------------------------------------------------
// lambda
// ---------------------------------------------------------------------------

[<Fact>]
let ``lambda: formal is alpha-renamed to a fresh stamp`` () =
    let stx = expandOne "(lambda (x) x)"
    let (_, xStamp) = formalsOf stx |> List.exactlyOne
    assertFreshStamp xStamp

[<Fact>]
let ``lambda: body reference gets the same stamp as the formal`` () =
    let stx = expandOne "(lambda (x) x)"
    let (_, formalStamp) = formalsOf stx |> List.exactlyOne
    let bodySymbols = bodyOf stx |> List.collect collectSymbols
    let refStamp = stampOf "x" bodySymbols
    assertSameStamp formalStamp refStamp

[<Fact>]
let ``lambda: multiple formals each get distinct fresh stamps`` () =
    let stx = expandOne "(lambda (x y) y)"
    let formals = formalsOf stx
    let xStamp = stampOf "x" formals
    let yStamp = stampOf "y" formals
    assertFreshStamp xStamp
    assertFreshStamp yStamp
    Assert.NotEqual(xStamp, yStamp)

[<Fact>]
let ``lambda: nested lambdas give same name different stamps`` () =
    // inner x should shadow outer x with a different stamp
    let stx = expandOne "(lambda (x) (lambda (x) x))"
    let (_, outerStamp) = formalsOf stx |> List.exactlyOne

    let innerLambda =
        match bodyOf stx with
        | [ inner ] -> inner
        | _ -> failwith "expected single inner form"

    let (_, innerStamp) = formalsOf innerLambda |> List.exactlyOne
    assertFreshStamp outerStamp
    assertFreshStamp innerStamp
    Assert.NotEqual(outerStamp, innerStamp)

[<Fact>]
let ``lambda: inner body references inner stamp not outer`` () =
    let stx = expandOne "(lambda (x) (lambda (x) x))"

    let innerLambda =
        match bodyOf stx with
        | [ inner ] -> inner
        | _ -> failwith "expected single inner form"

    let (_, innerFormalStamp) = formalsOf innerLambda |> List.exactlyOne
    let innerBodySymbols = bodyOf innerLambda |> List.collect collectSymbols
    assertSameStamp innerFormalStamp (stampOf "x" innerBodySymbols)

[<Fact>]
let ``lambda: bare rest-arg is renamed`` () =
    let stx = expandOne "(lambda args args)"
    let formals = formalsOf stx
    let stamp = stampOf "args" formals
    assertFreshStamp stamp
    let bodyStamp = bodyOf stx |> List.collect collectSymbols |> stampOf "args"
    assertSameStamp stamp bodyStamp

[<Fact>]
let ``lambda: dotted rest-arg is renamed`` () =
    let stx = expandOne "(lambda (x . rest) rest)"
    let formals = formalsOf stx
    let restStamp = stampOf "rest" formals
    assertFreshStamp restStamp
    let bodyStamp = bodyOf stx |> List.collect collectSymbols |> stampOf "rest"
    assertSameStamp restStamp bodyStamp

[<Fact>]
let ``lambda: stamps don't leak into later forms`` () =
    let stx = expandProgram "(lambda (x) x) x"
    let lambdaFormals = formalsOf stx.[0]
    let lambdaStamp = stampOf "x" lambdaFormals
    assertFreshStamp lambdaStamp
    let lambdaBody = stampOf "x" (bodyOf stx.[0] |> List.collect collectSymbols)
    assertSameStamp lambdaStamp lambdaBody
    // The later reference to x should be unbound and stay at GlobalStamp, not get the lambda's stamp
    let laterStamp = collectSymbols stx.[1] |> stampOf "x"
    assertSameStamp Ident.GlobalStamp laterStamp

// ---------------------------------------------------------------------------
// define
// ---------------------------------------------------------------------------

[<Fact>]
let ``define: introduced name gets a fresh stamp`` () =
    let forms = expandProgram "(define x 1)"
    // define produces one output form: (define x' 1)
    let syms = forms |> List.collect collectSymbols
    assertFreshStamp (stampOf "x" syms)

[<Fact>]
let ``define: later reference gets the same stamp`` () =
    let forms = expandProgram "(define x 1) x"
    let defSyms = collectSymbols forms[0]
    let defStamp = stampOf "x" defSyms
    let refStamp = collectSymbols forms[1] |> stampOf "x"
    assertSameStamp defStamp refStamp

[<Fact>]
let ``define shorthand: function name gets fresh stamp visible in later form`` () =
    let forms = expandProgram "(define (f x) x) f"
    let defSyms = collectSymbols forms[0]
    let fDefStamp = stampOf "f" defSyms
    assertFreshStamp fDefStamp
    let refStamp = collectSymbols forms[1] |> stampOf "f"
    assertSameStamp fDefStamp refStamp

[<Fact>]
let ``define shorthand: formal is renamed and visible in body`` () =
    let forms = expandProgram "(define (f x) x)"
    let syms = collectSymbols forms[0]
    let xStamp = stampOf "x" syms
    assertFreshStamp xStamp
    // x should appear exactly twice: once in formals, once in body — same stamp both times
    let allX = syms |> List.filter (fst >> (=) "x")
    Assert.All(allX, fun (_, s) -> assertSameStamp xStamp s)

[<Fact>]
let ``define shorthand: function name visible in its own body (recursion)`` () =
    // (define (f x) (f x)) — f inside the body should get the same stamp as in the define head
    let forms = expandProgram "(define (f x) (f x))"
    let syms = collectSymbols forms[0]
    let fStamps = syms |> List.filter (fst >> (=) "f") |> List.map snd
    // Should be at least two occurrences: head and body reference
    Assert.True(List.length fStamps >= 2)
    // All occurrences of f should share the same stamp
    let expected = List.head fStamps
    Assert.All(fStamps, assertSameStamp expected)

// ---------------------------------------------------------------------------
// let
// ---------------------------------------------------------------------------

[<Fact>]
let ``let: binding name gets a fresh stamp`` () =
    let stx = expandOne "(let ((x 1)) x)"
    let syms = collectSymbols stx
    assertFreshStamp (stampOf "x" syms)

[<Fact>]
let ``let: body reference gets same stamp as binding`` () =
    let stx = expandOne "(let ((x 1)) x)"
    let syms = collectSymbols stx
    // x appears in two positions: the binding and the body
    let xStamps = syms |> List.filter (fst >> (=) "x") |> List.map snd
    Assert.True(List.length xStamps >= 2)
    let expected = List.head xStamps
    Assert.All(xStamps, assertSameStamp expected)

[<Fact>]
let ``let: inits are evaluated in the outer env (global stamp)`` () =
    // In (let ((x y)) x), y in the init is unbound and stays at GlobalStamp
    let stx = expandOne "(let ((x y)) x)"
    let syms = collectSymbols stx
    let yStamp = stampOf "y" syms
    assertSameStamp Ident.GlobalStamp yStamp

[<Fact>]
let ``let: multiple bindings each get distinct fresh stamps`` () =
    let stx = expandOne "(let ((x 1) (y 2)) (+ x y))"
    let syms = collectSymbols stx
    let xStamp = stampOf "x" syms
    let yStamp = stampOf "y" syms
    assertFreshStamp xStamp
    assertFreshStamp yStamp
    Assert.NotEqual(xStamp, yStamp)

// ---------------------------------------------------------------------------
// let*
// ---------------------------------------------------------------------------

[<Fact>]
let ``let*: second init can reference first binding (sequential scoping)`` () =
    // (let* ((x 1) (y x)) y) — the x in the init of y should match the bound x
    let stx = expandOne "(let* ((x 1) (y x)) y)"
    let syms = collectSymbols stx
    let xStamp = stampOf "x" syms
    assertFreshStamp xStamp
    // All occurrences of x (binding + reference in y's init) should share stamps
    let xStamps = syms |> List.filter (fst >> (=) "x") |> List.map snd
    Assert.All(xStamps, assertSameStamp xStamp)

[<Fact>]
let ``let*: bindings get distinct stamps`` () =
    let stx = expandOne "(let* ((x 1) (y 2)) y)"
    let syms = collectSymbols stx
    let xStamp = stampOf "x" syms
    let yStamp = stampOf "y" syms
    Assert.NotEqual(xStamp, yStamp)

// ---------------------------------------------------------------------------
// letrec
// ---------------------------------------------------------------------------

[<Fact>]
let ``letrec: all names visible in all inits`` () =
    // (letrec ((f (lambda () (g))) (g (lambda () (f)))) f)
    // Both f and g inside the lambdas should carry the renamed stamps, not GlobalStamp
    let stx = expandOne "(letrec ((f (lambda () g)) (g (lambda () f))) f)"
    let syms = collectSymbols stx
    let fStamp = stampOf "f" syms
    let gStamp = stampOf "g" syms
    assertFreshStamp fStamp
    assertFreshStamp gStamp
    // Every occurrence of f should have fStamp, same for g
    let fStamps = syms |> List.filter (fst >> (=) "f") |> List.map snd
    let gStamps = syms |> List.filter (fst >> (=) "g") |> List.map snd
    Assert.All(fStamps, assertSameStamp fStamp)
    Assert.All(gStamps, assertSameStamp gStamp)

[<Fact>]
let ``letrec: binding names get distinct stamps`` () =
    let stx = expandOne "(letrec ((x 1) (y 2)) y)"
    let syms = collectSymbols stx
    Assert.NotEqual(stampOf "x" syms, stampOf "y" syms)

// ---------------------------------------------------------------------------
// Interaction: closures strip the closure wrapper
// ---------------------------------------------------------------------------

[<Fact>]
let ``closure: inner syntax expanded in captured env`` () =
    // Build a Closure by illuminating a symbol, then wrapping it in an env
    // where "x" points to a fresh ident — simulating a macro-introduced binding.
    let doc = TextDocument.fromParts "test" "x"

    let ictx: IlluminationCtx =
        { Doc = doc
          Diagnostics = DiagnosticBag.Empty }

    let parsed = Parse.readProgram doc.Path "x"
    let inner = parsed.Root.Body |> Seq.exactlyOne |> Expand.illuminateExpr ictx

    let freshId = Ident.fresh (Ident.mint "x")
    let env: StxEnv = Map.ofList [ Ident.mint "x", StxBinding.Var freshId ]
    let closure = Stx.Closure(inner, env)

    let ctx: StxCtx =
        { Doc = doc
          Diagnostics = DiagnosticBag.Empty
          Env = Map.empty }

    let _, result = Expand.expand ctx closure

    match result with
    | Some(Stx.Symbol(id, _)) -> assertSameStamp freshId.Stamp id.Stamp
    | other -> failwithf "Expected a renamed symbol, got: %A" other
