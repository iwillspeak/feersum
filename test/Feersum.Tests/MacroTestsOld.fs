module MacroTestsOld

open Xunit

open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Macros
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Factories.Convenience
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Text


/// Read a single Expression from a string using the new parser
let private readSingleExpression input =
    let result = Parse.readProgramSimple "program" input

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" input result.Diagnostics

    result.Root.Body |> Seq.exactlyOne

let private parse pattern literals =
    match (readSingleExpression pattern) |> parsePattern "..." literals with
    | Result.Ok p -> p
    | Result.Error e -> failwithf "Could not parse macro pattern %A" e

let private tryMatch macroPat haystack =
    let syntaxTree = readSingleExpression haystack

    match macroMatch macroPat syntaxTree with
    | Result.Ok b -> Some b
    | _ -> None

let private tryExpand macroPat transformer bindings =
    let transformer =
        readSingleExpression transformer
        |> parseTemplate "..." (findBound macroPat)
        |> Result.unwrap

    macroExpand transformer bindings

let rec private pp (syntax: Expression) =
    match syntax with
    | ConstantNode c ->
        match c.Value with
        | Some(NumVal n) -> n.ToString("g")
        | Some(BoolVal b) -> if b then "#t" else "#f"
        | Some(StrVal s) -> sprintf "%A" s
        | Some(CharVal c) -> sprintf "#\\%c" (Option.defaultValue '?' c)
        | None -> "#<error>"
    | SymbolNode s -> s.CookedValue
    | FormNode f ->
        let body = f.Body |> Seq.map pp |> String.concat " "
        sprintf "(%s)" body
    | _ -> failwithf "unsupported syntax kind %A" (syntax.GetType())

let private assertMatches pattern syntax =
    match macroMatch pattern syntax with
    | Result.Ok bindings -> bindings
    | o -> failwithf "Pattern variable did not match %A" o

/// Extract the inner ConstantValue from a constant Expression factory result
let private cv (e: Expression) =
    match e with
    | Constant(Some c) -> c
    | _ -> failwith "Expected constant with value"

[<Fact>]
let ``patterns with constant number`` () =

    let testConstantMatch (exprFactory: unit -> Expression) =
        let expr = exprFactory ()
        let pattern = MacroPattern.Constant(cv expr)

        match macroMatch pattern expr with
        | Result.Ok _ -> ()
        | Result.Error e -> failwithf "%A" e

    testConstantMatch (fun () -> numVal 101.0)
    testConstantMatch (fun () -> numVal 0.0)
    testConstantMatch (fun () -> charVal 'a')
    testConstantMatch (fun () -> boolVal true)
    testConstantMatch (fun () -> boolVal false)
    testConstantMatch (fun () -> strVal "")
    testConstantMatch (fun () -> strVal "§2")

[<Theory>]
[<InlineData("if", "if", true)>]
[<InlineData("if", "else", false)>]
[<InlineData("else", "else", true)>]
[<InlineData("test-thing", "test-thing", true)>]
[<InlineData("...", "...", true)>]
let ``literal identifiers only match their literal values`` pattern id expected =
    let pattern = MacroPattern.Literal pattern

    let actual =
        match macroMatch pattern (symbol id) with
        | Ok _ -> true
        | _ -> false

    Assert.Equal(expected, actual)

[<Fact>]
let ``variable patterns match anything`` () =

    let testVarMatch (syntax: Expression) =
        let pattern = MacroPattern.Variable "test"

        match macroMatch pattern syntax with
        | Result.Ok bindings ->
            let n, s = Assert.Single bindings.Bindings
            Assert.Equal("test", n)
            Assert.Same(syntax, s)
        | o -> failwithf "Pattern variable did not match %A" o

    testVarMatch (numVal 101.0)
    testVarMatch (numVal 0.0)
    testVarMatch (charVal 'a')
    testVarMatch (boolVal true)
    testVarMatch (boolVal false)
    testVarMatch (strVal "")
    testVarMatch (strVal "§2")
    testVarMatch (form [])
    testVarMatch (symbol "test")
    testVarMatch (form [ symbol "foodafdf" ])
    testVarMatch (form [ numVal 123.0 ])

[<Fact>]
let ``underscore patterns match anything`` () =

    let testUnderscoreMatch (syntax: Expression) =
        let pattern = MacroPattern.Underscore

        match macroMatch pattern syntax with
        | Result.Ok _ -> ()
        | o -> failwithf "Pattern variable did not match %A" o

    testUnderscoreMatch (numVal 101.0)
    testUnderscoreMatch (numVal 0.0)
    testUnderscoreMatch (charVal 'a')
    testUnderscoreMatch (boolVal true)
    testUnderscoreMatch (boolVal false)
    testUnderscoreMatch (strVal "")
    testUnderscoreMatch (strVal "§2")
    testUnderscoreMatch (form [])
    testUnderscoreMatch (symbol "test")
    testUnderscoreMatch (form [ symbol "foodafdf" ])
    testUnderscoreMatch (form [ numVal 123.0 ])

[<Fact>]
let ``simple form patterns`` () =

    Assert.Equal(MacroBindings.Empty, assertMatches (MacroPattern.Form []) (form []))

    Assert.Equal(
        MacroBindings.Empty,
        assertMatches
            (MacroPattern.Form
                [ MacroPattern.Constant(cv (boolVal false))
                  MacroPattern.Constant(cv (strVal "frob"))
                  MacroPattern.Constant(cv (numVal 123.56)) ])
            (form [ boolVal false; strVal "frob"; numVal 123.56 ])
    )

    // Check variable binding extracts the correct value (compared by pp since
    // Firethorn creates new red nodes when extracting form body elements)
    let bindings =
        assertMatches (MacroPattern.Form [ MacroPattern.Variable "test" ]) (form [ numVal 123.4 ])

    let n, s = Assert.Single bindings.Bindings
    Assert.Equal("test", n)
    Assert.Equal("123.4", pp s)

[<Fact>]
let ``dotted form patterns`` () =

    // This pattern is nonsense '( . 123.4)' matching (123.4). It's still an
    // interesting test though, so we'll keep it for now.
    Assert.Equal(
        MacroBindings.Empty,
        assertMatches (MacroPattern.DottedForm([], MacroPattern.Constant(cv (numVal 123.4)))) (form [ numVal 123.4 ])
    )

    // (head . tail) - compare bound values by pp since Firethorn creates new
    // red nodes when extracting form body elements
    let bindings =
        assertMatches
            (MacroPattern.DottedForm([ MacroPattern.Variable "head" ], MacroPattern.Variable "tail"))
            (form [ numVal 123.4; numVal 567.8 ])

    let headBinding = bindings.Bindings |> List.find (fun (n, _) -> n = "head") |> snd
    let tailBinding = bindings.Bindings |> List.find (fun (n, _) -> n = "tail") |> snd
    Assert.Equal("123.4", pp headBinding)
    Assert.Equal("567.8", pp tailBinding)

[<Theory>]
[<InlineData("a", "1", true)>]
[<InlineData("1.0", "1", true)>]
[<InlineData("a", "#f", true)>]
[<InlineData("#f", "#f", true)>]
[<InlineData("(a 1)", "(test 1)", true)>]
[<InlineData("(a . 1)", "(test 1)", true)>]
[<InlineData("foo", "foo", true)>]
[<InlineData("foo", "test", false)>]
[<InlineData("(1 ...)", "(1 2 3)", false)>]
[<InlineData("(1 ...)", "(1 1 1 1)", true)>]
[<InlineData("(1 ... 2 . c)", "(1 1 2)", true)>]
[<InlineData("(1 ... 2 . c)", "(1 1 2 3 4)", true)>]
[<InlineData("((a ...)...)", "(() (1 2 3) ((1)(2)(3)))", true)>]
let ``macro parse tests`` pattern syntax shouldMatch =
    let literals = [ "foo"; "bar" ]
    let macroPat = parse pattern literals
    Assert.Equal(shouldMatch, tryMatch macroPat syntax |> Option.isSome)

[<Fact>]
let ``custom elipsis patterns`` () =
    let pattern =
        parsePattern ":::" [] (readSingleExpression "(a :::)") |> Result.unwrap

    Assert.Equal(MacroPattern.Form [ MacroPattern.Repeat(MacroPattern.Variable "a") ], pattern)

[<Fact>]
let ``simple macro expand`` () =
    let numExpr: Expression = numVal 123.0
    let expanded = macroExpand (MacroTemplate.Quoted numExpr) MacroBindings.Empty
    // MacroTemplate.Quoted returns the stored Expression reference unchanged
    Assert.Equal(Ok numExpr, expanded)

    let boolExpr: Expression = boolVal true

    let expanded =
        macroExpand (MacroTemplate.Subst "test") (MacroBindings.FromVariable "test" boolExpr)

    Assert.Equal(Ok boolExpr, expanded)

    let expanded = macroExpand (MacroTemplate.Subst "thing") MacroBindings.Empty

    Assert.True(Result.isError expanded)

[<Theory>]
[<InlineData("(a)", "a", "(1)", "1")>]
[<InlineData("(a ...)", "123", "(1 #f foo)", "123")>]
[<InlineData("(_ (a)...)", "(a ...)", "(test (1)(#f)(foo))", "(1 #f foo)")>]
[<InlineData("(_ (a ...)...)", "((f a ...) ...)", "(test (1 2)(#f))", "((f 1 2) (f #f))")>]
let ``macro expand tests`` pattern template invocation expected =
    let macro = parse pattern []

    let bindings = tryMatch macro invocation |> Option.unwrap

    let expanded = tryExpand macro template bindings |> Result.unwrap

    Assert.Equal(expected, pp expanded)

[<Fact>]
let ``repeated values`` () =
    let macro = parse "(a ...)" []

    let bindings = tryMatch macro "(1 2 3)" |> Option.unwrap

    let expanded =
        macroExpand
            (MacroTemplate.Form(TextLocation.Missing, [ MacroTemplateElement.Repeated(MacroTemplate.Subst "a") ]))
            bindings
        |> Result.unwrap

    Assert.Equal("(1 2 3)", pp expanded)

[<Theory>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1 2) (3))")>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1) (2 3))")>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1) ())")>]
let ``invalid expansions`` pattern template invocation =
    let macro = parse pattern []

    let bindings = tryMatch macro invocation |> Option.unwrap

    let expanded = tryExpand macro template bindings
    Assert.True(Result.isError expanded)
