module MacroTests

open Xunit
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Factories.Convenience
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Utils

// Test the new Stx-based Macro API
// Helper functions for creating Stx values for testing

let private dummyLoc = TextLocation.Missing

let private stxId name = Stx.Id(name, dummyLoc)

let private emptyEnv: StxEnvironment = Map.empty


/// Extract the inner ConstantValue from a constant Expression factory result
let private cv (e: Stx) =
    match e with
    | StxDatum(d, _) -> d
    | _ -> failwith "Expected constant with value"

let private exprToStx (expr: Expression) : Stx =
    let reg = SourceRegistry.empty ()
    let docId = SourceRegistry.register reg "foo.scm" expr.Text
    let diags = DiagnosticBag.Empty
    let stx = Stx.ofExpr reg docId diags expr

    if hasErrors diags.Take then
        failwithf "Failed to convert expression to Stx: %A" diags

    stx

let private macroMatch pattern haystack =
    Macros.matchPattern pattern haystack emptyEnv

let private assertMatches pattern syntax =
    match macroMatch pattern syntax with
    | Some bindings -> bindings
    | None -> failwith "Pattern did not match"

/// Helper to get pretty-printed representation of bound Stx
let private ppStx (stx: Stx) : string =
    let rec pp (stx: Stx) =
        match stx with
        | Stx.Datum(d, _) ->
            match d with
            | StxDatum.Number n -> n.ToString("g")
            | StxDatum.Str s -> sprintf "%A" s
            | StxDatum.Character c -> sprintf "#\\%c" c
            | StxDatum.Boolean b -> if b then "#t" else "#f"
            | StxDatum.ByteVector _ -> "#u8(...)"
        | Stx.Id(name, _) -> name
        | Stx.List(items, tail, _) ->
            let itemsStr = items |> List.map pp |> String.concat " "

            match tail with
            | None -> sprintf "(%s)" itemsStr
            | Some t -> sprintf "(%s . %s)" itemsStr (pp t)
        | Stx.Vec(items, _) ->
            let itemsStr = items |> List.map pp |> String.concat " "
            sprintf "#(%s)" itemsStr
        | Stx.Closure(inner, _, _) -> pp inner
        | Stx.Error _ -> "#<error>"

    pp stx

/// Helper to extract the bound Stx from a binding
let private getBoundStx (bindings: MacroBindings) (name: string) : Stx =
    bindings.Bindings |> List.find (fun (n, _) -> n = name) |> snd |> fst

/// Helper to read a single expression from string and convert to Stx
let private readExprAsStx input : Stx =
    let result = Parse.readProgramSimple "program" input

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" input result.Diagnostics

    let expr = result.Root.Body |> Seq.exactlyOne
    exprToStx expr

/// Helper to read expression from string (not converted)
let private readExpr input : Expression =
    let result = Parse.readProgramSimple "program" input

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" input result.Diagnostics

    result.Root.Body |> Seq.exactlyOne

/// Parse a template string and transcribe with given bindings and pattern
let private tryExpand macroPat transformer bindings : Result<Stx, string> =
    let templateStx = readExprAsStx transformer
    let bound = Macros.findBound macroPat

    match Macros.parseTemplate "..." bound templateStx with
    | Result.Error e -> Result.Error e
    | Result.Ok tmpl ->
        let diag = DiagnosticBag.Empty
        let transcribed = Macros.transcribe tmpl bindings emptyEnv dummyLoc diag

        if hasErrors diag.Take then
            Result.Error(sprintf "Macro expansion failed: %A" diag.Take)
        else
            Result.Ok transcribed

/// Simulate the old macroExpand for testing: transcribe and pretty-print
let private macroExpand (template: MacroTemplate) (bindings: MacroBindings) : Result<string, string> =
    let diag = DiagnosticBag.Empty
    let transcribed = Macros.transcribe template bindings emptyEnv dummyLoc diag

    if hasErrors diag.Take then
        Result.Error(sprintf "Macro expansion failed: %A" diag.Take)
    else
        Result.Ok(ppStx transcribed)

/// Parse a pattern from a string, with given literal keywords
let private parse pattern literals =
    match readExprAsStx pattern |> Macros.parsePattern "_" "..." literals with
    | Result.Ok p -> p
    | Result.Error e -> failwithf "Could not parse macro pattern %A" e

/// Try to match a pattern against a Stx tree
let private tryMatch macroPat haystack =
    let stxTree = readExprAsStx haystack

    match macroMatch macroPat stxTree with
    | Some b -> Some b
    | None -> None

[<Fact>]
let ``patterns with constant number`` () =

    let testConstantMatch (exprFactory: unit -> Expression) =
        let expr = exprFactory () |> exprToStx
        let pattern = MacroPattern.Constant(cv expr)

        macroMatch pattern expr |> Option.unwrap |> ignore

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
        match macroMatch pattern (stxId id) with
        | Some _ -> true
        | None -> false

    Assert.Equal(expected, actual)

[<Fact>]
let ``variable patterns match anything`` () =
    let testVarMatch (syntax: Expression) =
        let pattern = MacroPattern.Variable "test"
        let syntax = exprToStx syntax

        match macroMatch pattern syntax with
        | Some bindings ->
            Assert.Single(bindings.Bindings) |> ignore
            let n, (stx, _) = Assert.Single bindings.Bindings
            Assert.Equal("test", n)
            Assert.Same(syntax, stx)
        | None -> Assert.True(false, "Expected pattern to match")

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
        let syntax = exprToStx syntax

        match macroMatch pattern syntax with
        | Some _ -> ()
        | None -> Assert.True(false, "Expected pattern to match")

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
    let emptyPattern = MacroPattern.Form []
    let emptyForm = form [] |> exprToStx

    Assert.Equal(MacroBindings.Empty, assertMatches emptyPattern emptyForm)

    Assert.Equal(
        MacroBindings.Empty,
        assertMatches
            (MacroPattern.Form
                [ MacroPattern.Constant(cv (boolVal false |> exprToStx))
                  MacroPattern.Constant(cv (strVal "frob" |> exprToStx))
                  MacroPattern.Constant(cv (numVal 123.56 |> exprToStx)) ])
            (form [ boolVal false; strVal "frob"; numVal 123.56 ] |> exprToStx)
    )

    let bindings =
        assertMatches (MacroPattern.Form [ MacroPattern.Variable "test" ]) (form [ numVal 123.4 ] |> exprToStx)

    let n, (stx, _) = Assert.Single bindings.Bindings
    Assert.Equal("test", n)
    Assert.Equal("123.4", ppStx stx)

[<Fact>]
let ``dotted form patterns`` () =
    // This pattern is nonsense '( . 123.4)' matching (123.4). It's still an
    // interesting test though, so we'll keep it for now.
    Assert.Equal(
        MacroBindings.Empty,
        assertMatches
            (MacroPattern.DottedForm([], MacroPattern.Constant(cv (numVal 123.4 |> exprToStx))))
            (form [ numVal 123.4 ] |> exprToStx)
    )

    // (head . tail) - compare bound values by ppStx
    let bindings =
        assertMatches
            (MacroPattern.DottedForm([ MacroPattern.Variable "head" ], MacroPattern.Variable "tail"))
            (form [ numVal 123.4; numVal 567.8 ] |> exprToStx)

    let headBinding = getBoundStx bindings "head"
    let tailBinding = getBoundStx bindings "tail"
    Assert.Equal("123.4", ppStx headBinding)
    Assert.Equal("567.8", ppStx tailBinding)

// MARKER ---- ADD REMAINING TESTS HERE

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
[<InlineData("(1 ... . c)", "(1 1 1 . 1)", true)>]
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
        Macros.parsePattern "_" ":::" [] (readExprAsStx "(a :::)") |> Result.unwrap

    Assert.Equal(MacroPattern.Form [ MacroPattern.Repeat(MacroPattern.Variable "a") ], pattern)

[<Fact>]
let ``simple macro expand`` () =
    let numStx: Stx = numVal 123.0 |> exprToStx
    let expanded = macroExpand (MacroTemplate.Quoted numStx) MacroBindings.Empty
    // MacroTemplate.Quoted returns the stored Stx reference unchanged
    Assert.Equal(Ok "123", expanded)

    let boolStx: Stx = boolVal true |> exprToStx

    let expanded =
        macroExpand (MacroTemplate.Subst "test") (MacroBindings.FromVariable "test" (boolStx, emptyEnv))

    Assert.Equal(Ok "#t", expanded)

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

    Assert.Equal(expected, ppStx expanded)

[<Fact>]
let ``repeated values`` () =
    let macro = parse "(a ...)" []

    let bindings = tryMatch macro "(1 2 3)" |> Option.unwrap

    let expanded =
        macroExpand (MacroTemplate.Form(dummyLoc, [ MacroTemplateElement.Repeated(MacroTemplate.Subst "a") ])) bindings
        |> Result.unwrap

    Assert.Equal("(1 2 3)", expanded)

[<Theory>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1 2) (3))")>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1) (2 3))")>]
[<InlineData("(bug (a ...) (b ...))", "((cons a b) ...)", "(bug (1) ())")>]
let ``invalid expansions`` pattern template invocation =
    let macro = parse pattern []

    let bindings = tryMatch macro invocation |> Option.unwrap

    let expanded = tryExpand macro template bindings
    Assert.True(Result.isError expanded)

// MARKER ---- LEAVE TETS AFTER THIS POINT ALONE

[<Fact>]
let ``pattern list matching`` () =
    let pattern =
        MacroPattern.Form
            [ MacroPattern.Literal "if"
              MacroPattern.Variable "test"
              MacroPattern.Variable "consequent"
              MacroPattern.Variable "alternate" ]

    let testForm = form [ symbol "if"; symbol "x"; symbol "y"; symbol "z" ] |> exprToStx

    match macroMatch pattern testForm with
    | Some bindings ->
        let names = bindings.Bindings |> List.map fst
        Assert.Equal(3, List.length names)
        Assert.Contains("test", names)
        Assert.Contains("consequent", names)
        Assert.Contains("alternate", names)
    | None -> Assert.True(false, "Expected pattern to match")

[<Fact>]
let ``parse simple pattern`` () =
    let patternStx = readExprAsStx "(a b)"
    let result = Macros.parsePattern "_" "..." [] patternStx

    match result with
    | Result.Ok(MacroPattern.Form [ MacroPattern.Variable "a"; MacroPattern.Variable "b" ]) -> ()
    | _ -> Assert.True(false, sprintf "Expected pattern to parse, got %A" result)

[<Fact>]
let ``parse pattern with literal`` () =
    let patternStx = readExprAsStx "(if test then else)"
    let result = Macros.parsePattern "_" "..." [ "if"; "else" ] patternStx

    match result with
    | Result.Ok(MacroPattern.Form patterns) ->
        Assert.Equal(4, List.length patterns)

        match patterns with
        | MacroPattern.Literal "if" :: MacroPattern.Variable "test" :: MacroPattern.Variable "then" :: MacroPattern.Literal "else" :: [] ->
            ()
        | _ -> Assert.True(false, "Pattern items don't match expected literals/variables")
    | _ -> Assert.True(false, "Expected pattern to parse")

[<Fact>]
let ``parse literal list`` () =
    let literalStx = readExprAsStx "(if else define)"
    let result = Macros.parseLiteralList literalStx

    match result with
    | Result.Ok names ->
        Assert.Equal(3, List.length names)
        Assert.Equal<string list>([ "if"; "else"; "define" ], names)
    | Result.Error e -> Assert.True(false, sprintf "Expected parse to succeed: %s" e)

[<Fact>]
let ``parse template`` () =
    let templateStx = readExprAsStx "(cons a b)"
    let result = Macros.parseTemplate "..." [ "a"; "b" ] templateStx

    match result with
    | Result.Ok(MacroTemplate.Form(_, elements)) when List.length elements = 3 -> ()
    | _ -> Assert.True(false, "Expected template to parse")

[<Fact>]
let ``find bound variables in pattern`` () =
    let pattern =
        MacroPattern.Form
            [ MacroPattern.Variable "x"
              MacroPattern.Variable "y"
              MacroPattern.Literal "z" ]

    let bound = Macros.findBound pattern
    Assert.Equal<string list>([ "x"; "y" ], bound)

[<Fact>]
let ``parse syntax-rules form`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx = readExprAsStx "(syntax-rules (if else) ((if a b c) (a)))"

    let result =
        Macros.parseSyntaxRulesStx "test-macro" syntaxRulesStx (Map.empty: StxEnvironment) diags dummyLoc

    match result with
    | Some macro ->
        Assert.Equal(1, List.length macro.Transformers)
        Assert.Equal<StxEnvironment>(emptyEnv, macro.DefScope)
    | None -> Assert.True(false, "Expected syntax-rules to parse")

[<Fact>]
let ``parse syntax-rules with custom ellipsis`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx = readExprAsStx "(syntax-rules ::: () ((a :::) (a)))"

    let result =
        Macros.parseSyntaxRulesStx "test-macro" syntaxRulesStx (Map.empty: StxEnvironment) diags dummyLoc

    match result with
    | Some macro -> Assert.Equal(1, List.length macro.Transformers)
    | None -> Assert.True(false, "Expected custom ellipsis to parse")

[<Fact>]
let ``pattern mismatch`` () =
    let pattern = MacroPattern.Constant(cv (numVal 100.0 |> exprToStx))
    let wrongValue = numVal 200.0 |> exprToStx

    match macroMatch pattern wrongValue with
    | Some _ -> Assert.True(false, "Expected pattern not to match")
    | None -> ()

[<Fact>]
let ``form pattern with wrong length`` () =
    let pattern =
        MacroPattern.Form [ MacroPattern.Variable "a"; MacroPattern.Variable "b" ]

    let testForm = form [ numVal 1.0 ] |> exprToStx

    match macroMatch pattern testForm with
    | Some _ -> Assert.True(false, "Expected pattern not to match (wrong length)")
    | None -> ()
