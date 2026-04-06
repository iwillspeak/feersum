module MacroTests

open Xunit
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Utils

// Test the new Stx-based Macro API
// Helper functions for creating Stx values for testing

let private dummyLoc = TextLocation.Missing

let private id name = Feersum.CompilerServices.Binding.Stx.Id(name, dummyLoc)

let private num n =
    Feersum.CompilerServices.Binding.Stx.Datum(
        Feersum.CompilerServices.Binding.StxDatum.Number n,
        dummyLoc
    )

let private str s =
    Feersum.CompilerServices.Binding.Stx.Datum(
        Feersum.CompilerServices.Binding.StxDatum.Str s,
        dummyLoc
    )

let private list items =
    Feersum.CompilerServices.Binding.Stx.List(items, None, dummyLoc)

let private dotList items tail =
    Feersum.CompilerServices.Binding.Stx.List(items, Some tail, dummyLoc)

let private emptyEnv: StxEnvironment = Map.empty

[<Fact>]
let ``patterns with constant number`` () =
    let testNum = num 101.0
    let pattern = MacroPattern.Constant(Feersum.CompilerServices.Binding.StxDatum.Number 101.0)

    match MacrosNew.matchPattern pattern testNum emptyEnv with
    | Some bindings -> Assert.Equal(MacroBindings.Empty, bindings)
    | None -> Assert.True(false, "Expected pattern to match")

    let wrongNum = num 102.0

    match MacrosNew.matchPattern pattern wrongNum emptyEnv with
    | Some _ -> Assert.True(false, "Expected pattern not to match")
    | None -> ()

[<Theory>]
[<InlineData("if", "if", true)>]
[<InlineData("if", "else", false)>]
[<InlineData("else", "else", true)>]
[<InlineData("test-thing", "test-thing", true)>]
[<InlineData("...", "...", true)>]
let ``literal identifiers only match their literal values`` literalName testName expected =
    let pattern = MacroPattern.Literal literalName
    let testId = id testName

    let actual =
        match MacrosNew.matchPattern pattern testId emptyEnv with
        | Some _ -> true
        | None -> false

    Assert.Equal(expected, actual)

[<Fact>]
let ``variable patterns match anything`` () =
    let testVarMatch (syntax: Feersum.CompilerServices.Binding.Stx) =
        let pattern = MacroPattern.Variable "test"

        match MacrosNew.matchPattern pattern syntax emptyEnv with
        | Some bindings ->
            Assert.Single(bindings.Bindings) |> ignore
            let n, (stx, _) = Assert.Single bindings.Bindings
            Assert.Equal("test", n)
        | None -> Assert.True(false, "Expected pattern to match")

    testVarMatch (num 101.0)
    testVarMatch (num 0.0)
    testVarMatch (str "")
    testVarMatch (list [])
    testVarMatch (id "test")
    testVarMatch (list [ id "foodafdf" ])
    testVarMatch (list [ num 123.0 ])

[<Fact>]
let ``underscore patterns match anything`` () =
    let testUnderscoreMatch (syntax: Feersum.CompilerServices.Binding.Stx) =
        let pattern = MacroPattern.Underscore

        match MacrosNew.matchPattern pattern syntax emptyEnv with
        | Some _ -> ()
        | None -> Assert.True(false, "Expected pattern to match")

    testUnderscoreMatch (num 101.0)
    testUnderscoreMatch (num 0.0)
    testUnderscoreMatch (str "")
    testUnderscoreMatch (list [])
    testUnderscoreMatch (id "test")
    testUnderscoreMatch (list [ id "foodafdf" ])
    testUnderscoreMatch (list [ num 123.0 ])

[<Fact>]
let ``simple form patterns`` () =
    let emptyPattern = MacroPattern.Form []
    let emptyForm = list []

    match MacrosNew.matchPattern emptyPattern emptyForm emptyEnv with
    | Some bindings -> Assert.Equal(MacroBindings.Empty, bindings)
    | None -> Assert.True(false, "Expected pattern to match")

    let complexPattern =
        MacroPattern.Form
            [ MacroPattern.Constant(Feersum.CompilerServices.Binding.StxDatum.Number 123.56)
              MacroPattern.Constant(Feersum.CompilerServices.Binding.StxDatum.Str "frob")
              MacroPattern.Constant(Feersum.CompilerServices.Binding.StxDatum.Number 789.01) ]

    let complexForm = list [ num 123.56; str "frob"; num 789.01 ]

    match MacrosNew.matchPattern complexPattern complexForm emptyEnv with
    | Some _ -> ()
    | None -> Assert.True(false, "Expected complex pattern to match")

    let varPattern = MacroPattern.Form [ MacroPattern.Variable "test" ]
    let varForm = list [ num 123.4 ]

    match MacrosNew.matchPattern varPattern varForm emptyEnv with
    | Some bindings ->
        Assert.Single(bindings.Bindings) |> ignore
        let n, _ = Assert.Single bindings.Bindings
        Assert.Equal("test", n)
    | None -> Assert.True(false, "Expected var pattern to match")

[<Fact>]
let ``dotted form patterns`` () =
    let pattern = MacroPattern.DottedForm([], MacroPattern.Variable "tail")
    let form = dotList [ num 123.4; num 567.8 ] (num 789.0)

    match MacrosNew.matchPattern pattern form emptyEnv with
    | Some bindings ->
        Assert.Single(bindings.Bindings) |> ignore
        let n, _ = Assert.Single bindings.Bindings
        Assert.Equal("tail", n)
    | None -> Assert.True(false, "Expected dotted form to match")

[<Fact>]
let ``pattern list matching`` () =
    let pattern =
        MacroPattern.Form
            [ MacroPattern.Literal "if"
              MacroPattern.Variable "test"
              MacroPattern.Variable "consequent"
              MacroPattern.Variable "alternate" ]

    let form = list [ id "if"; id "x"; id "y"; id "z" ]

    match MacrosNew.matchPattern pattern form emptyEnv with
    | Some bindings ->
        let names = bindings.Bindings |> List.map fst
        Assert.Equal(3, List.length names)
        Assert.Contains("test", names)
        Assert.Contains("consequent", names)
        Assert.Contains("alternate", names)
    | None -> Assert.True(false, "Expected pattern to match")

[<Fact>]
let ``parse simple pattern`` () =
    let patternStx = list [ id "a"; id "b" ]
    let result = MacrosNew.parsePattern "_" "..." [] patternStx

    match result with
    | Result.Ok (MacroPattern.Form [ MacroPattern.Variable "a"; MacroPattern.Variable "b" ]) -> ()
    | _ -> Assert.True(false, sprintf "Expected pattern to parse, got %A" result)

[<Fact>]
let ``parse pattern with literal`` () =
    let patternStx = list [ id "if"; id "test"; id "then"; id "else" ]
    let result = MacrosNew.parsePattern "_" "..." [ "if"; "else" ] patternStx

    match result with
    | Result.Ok (MacroPattern.Form patterns) ->
        Assert.Equal(4, List.length patterns)

        match patterns with
        | MacroPattern.Literal "if"
          :: MacroPattern.Variable "test"
          :: MacroPattern.Variable "then"
          :: MacroPattern.Literal "else"
          :: [] -> ()
        | _ -> Assert.True(false, "Pattern items don't match expected literals/variables")
    | _ -> Assert.True(false, "Expected pattern to parse")

[<Fact>]
let ``parse literal list`` () =
    let literalStx = list [ id "if"; id "else"; id "define" ]
    let result = MacrosNew.parseLiteralList literalStx

    match result with
    | Result.Ok names ->
        Assert.Equal(3, List.length names)
        Assert.Equal<string list>([ "if"; "else"; "define" ], names)
    | Result.Error e -> Assert.True(false, sprintf "Expected parse to succeed: %s" e)

[<Fact>]
let ``parse template`` () =
    let templateStx = list [ id "cons"; id "a"; id "b" ]
    let result = MacrosNew.parseTemplate "..." [ "a"; "b" ] templateStx

    match result with
    | Result.Ok (MacroTemplate.Form(_, elements)) when List.length elements = 3 -> ()
    | _ -> Assert.True(false, "Expected template to parse")

[<Fact>]
let ``find bound variables in pattern`` () =
    let pattern =
        MacroPattern.Form [ MacroPattern.Variable "x"; MacroPattern.Variable "y"; MacroPattern.Literal "z" ]

    let bound = MacrosNew.findBound pattern
    Assert.Equal<string list>([ "x"; "y" ], bound)

[<Fact>]
let ``parse syntax-rules form`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx =
        list
            [ id "syntax-rules"
              list [ id "if"; id "else" ]
              list [ list [ id "if"; id "a"; id "b"; id "c" ]; list [ id "a" ] ] ]

    let result = MacrosNew.parseSyntaxRulesStx "test-macro" syntaxRulesStx (Map.empty: StxEnvironment) diags dummyLoc

    match result with
    | Some macro ->
        Assert.Equal(1, List.length macro.Transformers)
        Assert.Equal<StxEnvironment>(emptyEnv, macro.DefScope)
    | None -> Assert.True(false, "Expected syntax-rules to parse")

[<Fact>]
let ``parse syntax-rules with custom ellipsis`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx =
        list
            [ id "syntax-rules"
              id ":::"
              list []
              list [ list [ id "a"; id ":::" ]; list [ id "a" ] ] ]

    let result = MacrosNew.parseSyntaxRulesStx "test-macro" syntaxRulesStx (Map.empty: StxEnvironment) diags dummyLoc

    match result with
    | Some macro -> Assert.Equal(1, List.length macro.Transformers)
    | None -> Assert.True(false, "Expected custom ellipsis to parse")

[<Fact>]
let ``pattern mismatch`` () =
    let pattern = MacroPattern.Constant(Feersum.CompilerServices.Binding.StxDatum.Number 100.0)
    let wrongValue = num 200.0

    match MacrosNew.matchPattern pattern wrongValue emptyEnv with
    | Some _ -> Assert.True(false, "Expected pattern not to match")
    | None -> ()

[<Fact>]
let ``form pattern with wrong length`` () =
    let pattern = MacroPattern.Form [ MacroPattern.Variable "a"; MacroPattern.Variable "b" ]
    let form = list [ num 1.0 ]

    match MacrosNew.matchPattern pattern form emptyEnv with
    | Some _ -> Assert.True(false, "Expected pattern not to match (wrong length)")
    | None -> ()
