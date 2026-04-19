module LowerTests

open Xunit
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Diagnostics

// -- Helpers ------------------------------------------------------------------

/// Parse, bind, and lower a Scheme expression. Returns the lowered BoundBody.
let private lowerScheme source =
    let registry = SourceRegistry.empty ()
    let result = Parse.readProgram registry "program" source

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" source result.Diagnostics

    let env = Environments.empty
    let initialScope, preloaded = Environments.intoParts env

    let bound =
        Binder.bindProgram registry initialScope preloaded [] Map.empty [ result.Root ]
    // if hasErrors bound.Diagnostics then
    //     failwithf "Bind error in '%s': %A" source bound.Diagnostics

    (Lower.lower bound).Root

/// Strip SequencePoint wrappers to reach the underlying expression.
let rec private stripSeqPoints =
    function
    | BoundExpr.SequencePoint(inner, _) -> stripSeqPoints inner
    | other -> other

/// Extract the inner lambda body from an expression, failing if not found.
let private lambdaBody source expr =
    match stripSeqPoints expr with
    | BoundExpr.Lambda(_, body) -> body
    | other -> failwithf "Expected Lambda in '%s', got %A" source other

/// Assert two StorageRef lists are equal as sets (order-independent).
let private assertEqualEnv (expected: StorageRef list) (actual: StorageRef list) =
    let exp = expected |> Set.ofList
    let act = actual |> Set.ofList
    Assert.True((exp = act), sprintf "Expected env %A but got %A" expected actual)

/// Extract the original StorageRef from an Environment wrapper.
let private envSource =
    function
    | StorageRef.Environment(_, s) -> s
    | s -> s

// -- Tests: captures and environment mappings ---------------------------------

[<Fact>]
let ``simple lambda without captures has no env mappings`` () =
    let lowered = lowerScheme "(lambda (x) x)"
    let body = lambdaBody "(lambda (x) x)" lowered.Body
    // Identity lambda: no inner lambdas capture anything, so the binder
    // never marks this body as having a dynamic env — EnvMappings stays None.
    Assert.Empty(body.Captures)
    Assert.Equal(None, body.EnvMappings)

[<Fact>]
let ``captured variable rewrites arg into environment slot`` () =
    let lowered = lowerScheme "(lambda (x) (lambda () x))"
    let outerBody = lambdaBody "outer" lowered.Body
    // x (Arg 0) must be hoisted into an env slot
    assertEqualEnv [ StorageRef.Environment(0, StorageRef.Arg 0) ] (outerBody.EnvMappings |> Option.defaultValue [])
    let innerBody = lambdaBody "inner" outerBody.Body
    // Inner lambda's captures should reference the hoisted env slot
    assertEqualEnv [ StorageRef.Environment(0, StorageRef.Arg 0) ] innerBody.Captures
    // Inner lambda reads from the parent env (Some []) but hoists nothing itself
    Assert.Equal(Some [], innerBody.EnvMappings)

[<Fact>]
let ``two captures produce two environment slots`` () =
    let lowered = lowerScheme "(lambda (a b) (lambda () (a b)))"
    let outerBody = lambdaBody "outer" lowered.Body
    // Both a (Arg 0) and b (Arg 1) must be hoisted
    assertEqualEnv
        [ StorageRef.Environment(0, StorageRef.Arg 0)
          StorageRef.Environment(1, StorageRef.Arg 1) ]
        (outerBody.EnvMappings |> Option.defaultValue [])

    let innerBody = lambdaBody "inner" outerBody.Body

    assertEqualEnv
        [ StorageRef.Environment(0, StorageRef.Arg 0)
          StorageRef.Environment(1, StorageRef.Arg 1) ]
        innerBody.Captures

[<Fact>]
let ``lambda that captures only one of two args`` () =
    let lowered = lowerScheme "(lambda (a b) (lambda () a))"
    let outerBody = lambdaBody "outer" lowered.Body
    // Only a should be in the environment; b is not captured
    assertEqualEnv [ StorageRef.Environment(0, StorageRef.Arg 0) ] (outerBody.EnvMappings |> Option.defaultValue [])
    let innerBody = lambdaBody "inner" outerBody.Body
    assertEqualEnv [ StorageRef.Environment(0, StorageRef.Arg 0) ] innerBody.Captures

[<Fact>]
let ``three levels of nesting chain captures through environment`` () =
    // a and b are captured all the way through to the innermost lambda
    let lowered = lowerScheme "(lambda (a b) (lambda (c) (lambda () (a b c))))"
    let outerBody = lambdaBody "outer" lowered.Body
    // a and b hoisted by outer lambda — into some pair of env slots
    assertEqualEnv
        [ StorageRef.Environment(0, StorageRef.Arg 0)
          StorageRef.Environment(1, StorageRef.Arg 1) ]
        (outerBody.EnvMappings |> Option.defaultValue [])

    let midBody = lambdaBody "mid" outerBody.Body
    // Mid lambda's captures are the rewritten a, b env refs passed from outer
    assertEqualEnv
        [ StorageRef.Environment(0, StorageRef.Arg 0)
          StorageRef.Environment(1, StorageRef.Arg 1) ]
        midBody.Captures
    // Mid lambda also hoists c (Arg 0) for the innermost lambda
    let midEnvSources =
        midBody.EnvMappings |> Option.defaultValue [] |> List.map envSource

    Assert.Contains(StorageRef.Arg 0, midEnvSources)
    let innerBody = lambdaBody "inner" midBody.Body
    Assert.NotEmpty(innerBody.Captures)

[<Fact>]
let ``uncaptured lambda has no environment entries`` () =
    // b is never used inside the inner lambda
    let lowered = lowerScheme "(lambda (a b) (lambda () a))"
    let outerBody = lambdaBody "outer" lowered.Body
    let envMappings = outerBody.EnvMappings |> Option.defaultValue []
    // b should not appear in the environment at all
    Assert.DoesNotContain(
        StorageRef.Arg 1,
        envMappings
        |> List.map (function
            | StorageRef.Environment(_, s) -> s
            | s -> s)
    )

// -- Tests: sequence flattening -----------------------------------------------

[<Fact>]
let ``empty begin flattens to Nop`` () =
    let lowered = lowerScheme "(begin)"
    Assert.Equal(BoundExpr.Nop, stripSeqPoints lowered.Body)

[<Fact>]
let ``begin with single expression unwraps it`` () =
    let lowered = lowerScheme "(begin 42)"
    Assert.Equal(BoundExpr.Literal(BoundLiteral.Number 42.0), stripSeqPoints lowered.Body)

[<Fact>]
let ``nested begin flattens to a single sequence`` () =
    let lowered = lowerScheme "(begin (begin 1 2) 3)"

    match stripSeqPoints lowered.Body with
    | BoundExpr.Seq exprs -> Assert.Equal(3, List.length exprs)
    | BoundExpr.Nop -> Assert.True(false, "Expected non-empty sequence")
    | _ -> Assert.True(false, "Expected Seq from nested begin")

// -- Tests: local variable counts ---------------------------------------------

[<Fact>]
let ``let binding produces one local per binding`` () =
    let lowered = lowerScheme "(let ((x 10) (y 20)) x)"
    Assert.Equal(2, lowered.Locals)

[<Fact>]
let ``local compaction removes uncaptured locals from lambda env`` () =
    // 'unused' is never captured by the inner lambda; only 'x' is
    let lowered = lowerScheme "(lambda (x) (let ((unused 99)) (lambda () x)))"
    let outerBody = lambdaBody "outer" lowered.Body
    // x (Arg 0) should be in env; 'unused' (Local 0) should not
    let envMappings = outerBody.EnvMappings |> Option.defaultValue []

    let envSources =
        envMappings
        |> List.map (function
            | StorageRef.Environment(_, s) -> s
            | s -> s)

    Assert.Contains(StorageRef.Arg 0, envSources)
    Assert.DoesNotContain(StorageRef.Local 0, envSources)
