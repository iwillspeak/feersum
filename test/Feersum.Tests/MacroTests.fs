module MacroTests

open Xunit
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Stx
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
        | Stx.Closure(inner, _) -> pp inner
        | Stx.Error _ -> "#<error>"

    pp stx

/// Helper to extract the bound Stx from a binding
let private getBoundStx (bindings: MacroBindings) (name: string) : Stx =
    bindings.Bindings |> List.find (fun (n, _) -> n = name) |> snd |> fst

/// Helper to read a single expression from string and convert to Stx
let private readExprAsStx input : Stx =
    let registry = SourceRegistry.empty ()
    let result = Parse.readProgram registry "program" input

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" input result.Diagnostics

    let expr = result.Root.Body |> Seq.exactlyOne
    exprToStx expr

/// Helper to read expression from string (not converted)
let private readExpr input : Expression =
    let registry = SourceRegistry.empty ()
    let result = Parse.readProgram registry "program" input

    if result |> Parse.ParseResult.hasErrors then
        failwithf "Parse error in '%s': %A" input result.Diagnostics

    result.Root.Body |> Seq.exactlyOne

/// Parse a pattern Stx node via a synthetic syntax-rules form.
/// Returns the MacroPattern on success, or an error string on failure.
let private parsePattern
    (underscore: string)
    (ellipsis: string)
    (literals: string list)
    (patStx: Stx)
    : Result<MacroPattern, string> =
    let diags = DiagnosticBag.Empty
    // Synthesise (syntax-rules <ellipsis-id-if-custom> (lits...) ((<underscore> pat) #f))
    // We wrap patStx in a dummy rule so parseSyntaxRulesStx can parse it.
    let litNames = literals |> List.map (fun l -> Stx.Id(l, dummyLoc))
    let litList = Stx.List(litNames, None, dummyLoc)
    let dummyTemplate = Stx.Id("#f", dummyLoc)
    let dummyHead = Stx.Id(underscore, dummyLoc)
    let rulePat = Stx.List([ dummyHead; patStx ], None, dummyLoc)
    let rule = Stx.List([ rulePat; dummyTemplate ], None, dummyLoc)
    let syntaxRulesHead = Stx.Id("syntax-rules", dummyLoc)
    let ellipsisId = Stx.Id(ellipsis, dummyLoc)

    let form =
        if ellipsis = "..." then
            Stx.List([ syntaxRulesHead; litList; rule ], None, dummyLoc)
        else
            Stx.List([ syntaxRulesHead; ellipsisId; litList; rule ], None, dummyLoc)

    match MacroParse.parseSyntaxRulesStx diags "_" form emptyEnv with
    | None ->
        let msgs = diags.Diagnostics |> List.map (fun d -> d.Message) |> String.concat "; "
        Result.Error msgs
    | Some macro ->
        let (pat, _) = macro.Rules.[0]

        match pat with
        | MacroPattern.Form [ MacroPattern.Underscore; inner ] -> Result.Ok inner
        | MacroPattern.Form [ MacroPattern.Variable _; inner ] -> Result.Ok inner
        | other -> Result.Ok other

/// Parse a pattern+template pair from strings and transcribe with given bindings.
/// Pattern and template are parsed together so ellipsis depth is correctly tracked.
let private tryExpand
    (patternStr: string)
    (literals: string list)
    (templateStr: string)
    (bindings: MacroBindings)
    : Result<Stx, string> =
    let diags = DiagnosticBag.Empty
    let patStx = readExprAsStx patternStr
    let tmplStx = readExprAsStx templateStr
    let ruleStx = Stx.List([ patStx; tmplStx ], None, dummyLoc)
    // Tests supply plain name strings; pair each with None (unbound in empty env)
    let literalPairs = literals |> List.map (fun n -> n, None)
    let (_, tmpl) = MacroParse.parseSyntaxRule diags "..." literalPairs emptyEnv ruleStx

    let parseErrors =
        diags.Diagnostics |> List.filter (fun d -> d.Kind.Level = DiagnosticLevel.Error)

    if not (List.isEmpty parseErrors) then
        let msgs = parseErrors |> List.map (fun d -> d.Message) |> String.concat "; "
        Result.Error msgs
    else
        let transcribeResult, _ = Macros.macroExpandTemplate tmpl bindings

        match transcribeResult with
        | Result.Error e -> Result.Error(sprintf "Macro expansion failed: %s" e)
        | Result.Ok transcribed -> Result.Ok transcribed

/// Simulate the old macroExpand for testing: transcribe and pretty-print
let private macroExpand (template: MacroTemplate) (bindings: MacroBindings) : Result<string, string> =
    let transcribeResult, _ = Macros.macroExpandTemplate template bindings

    match transcribeResult with
    | Result.Error e -> Result.Error(sprintf "Macro expansion failed: %s" e)
    | Result.Ok transcribed -> Result.Ok(ppStx transcribed)

/// Parse a pattern from a string, with given literal keywords
let private parse pattern literals =
    match readExprAsStx pattern |> parsePattern "_" "..." literals with
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
    // Both sides are unbound in emptyEnv (None = None), so name equality alone decides the match.
    let pattern = MacroPattern.Literal(pattern, None)

    let actual =
        match macroMatch pattern (stxId id) with
        | Some _ -> true
        | None -> false

    Assert.Equal(expected, actual)

[<Fact>]
let ``literal patterns reject same-named identifiers with a different binding`` () =
    // Simulate the hygienic case: the definition-site has `else` unbound (None),
    // but the call-site has introduced a local variable for `else`, giving it a
    // fresh Variable binding.  The literal pattern should NOT match.
    let freshIdent = Ident.fresh ()
    let callSiteBinding = StxBinding.Variable freshIdent
    let callSiteEnv: StxEnvironment = Map.ofList [ "else", callSiteBinding ]

    // Pattern was parsed when `else` was unbound — defBinding = None.
    let pattern = MacroPattern.Literal("else", None)
    // Input identifier arrives with the call-site env attached via a Closure.
    let inputStx = Stx.Closure(Stx.Id("else", dummyLoc), callSiteEnv)

    let actual = Macros.matchPattern pattern inputStx emptyEnv

    Assert.True(actual.IsNone, "Expected literal to NOT match a same-named but differently-bound identifier")

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
    // (. 123.4) matching (123.4): the tail after consuming no items is (123.4) — a
    // list, not the atom 123.4 — so the constant pattern must not match.
    Assert.Equal(
        None,
        macroMatch
            (MacroPattern.DottedForm([], MacroPattern.Constant(cv (numVal 123.4 |> exprToStx))))
            (form [ numVal 123.4 ] |> exprToStx)
    )

    // (head . tail) matching (123.4 567.8): tail captures the remainder as a list.
    let bindings =
        assertMatches
            (MacroPattern.DottedForm([ MacroPattern.Variable "head" ], MacroPattern.Variable "tail"))
            (form [ numVal 123.4; numVal 567.8 ] |> exprToStx)

    let headBinding = getBoundStx bindings "head"
    let tailBinding = getBoundStx bindings "tail"
    Assert.Equal("123.4", ppStx headBinding)
    Assert.Equal("(567.8)", ppStx tailBinding)

[<Theory>]
[<InlineData("a", "1", true)>]
[<InlineData("1.0", "1", true)>]
[<InlineData("a", "#f", true)>]
[<InlineData("#f", "#f", true)>]
[<InlineData("(a 1)", "(test 1)", true)>]
// (a . 1) matches lists whose tail is exactly the atom 1, i.e. improper lists like
// (x . 1). A proper list (test 1) has tail (1), not 1, so it does not match.
[<InlineData("(a . 1)", "(test 1)", false)>]
[<InlineData("(a . 1)", "(test . 1)", true)>]
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
    let pattern = parsePattern "_" ":::" [] (readExprAsStx "(a :::)") |> Result.unwrap

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
// The head 'a' is the macro name position - not a pattern variable, so template 'a' is a literal identifier
[<InlineData("(a)", "a", "(1)", "a")>]
[<InlineData("(_ a ...)", "123", "(test 1 #f foo)", "123")>]
[<InlineData("(_ (a)...)", "(a ...)", "(test (1)(#f)(foo))", "(1 #f foo)")>]
[<InlineData("(_ (a ...)...)", "((f a ...) ...)", "(test (1 2)(#f))", "((f 1 2) (f #f))")>]
let ``macro expand tests`` pattern template invocation expected =
    let macro = parse pattern []

    let bindings = tryMatch macro invocation |> Option.unwrap

    let expanded = tryExpand pattern [] template bindings |> Result.unwrap

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

    let expanded = tryExpand pattern [] template bindings
    Assert.True(Result.isError expanded)

[<Fact>]
let ``pattern list matching`` () =
    let pattern =
        MacroPattern.Form
            [ MacroPattern.Literal("if", None)
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
    let result = parsePattern "_" "..." [] patternStx

    match result with
    | Result.Ok(MacroPattern.Form [ MacroPattern.Variable "a"; MacroPattern.Variable "b" ]) -> ()
    | _ -> Assert.True(false, sprintf "Expected pattern to parse, got %A" result)

[<Fact>]
let ``parse pattern with literal`` () =
    let patternStx = readExprAsStx "(if test then else)"
    let result = parsePattern "_" "..." [ "if"; "else" ] patternStx

    match result with
    | Result.Ok(MacroPattern.Form patterns) ->
        Assert.Equal(4, List.length patterns)

        match patterns with
        | MacroPattern.Literal("if", None) :: MacroPattern.Variable "test" :: MacroPattern.Variable "then" :: MacroPattern.Literal("else",
                                                                                                                                   None) :: [] ->
            ()
        | _ -> Assert.True(false, "Pattern items don't match expected literals/variables")
    | _ -> Assert.True(false, "Expected pattern to parse")

[<Fact>]
let ``parse literal list`` () =
    let literalStx = readExprAsStx "(if else define)"
    let diags = DiagnosticBag.Empty
    let lits = MacroParse.parseLiteralList diags emptyEnv literalStx

    if hasErrors diags.Take then
        Assert.True(false, "Expected parse to succeed")
    else
        let names = lits |> List.map fst
        Assert.Equal(3, List.length names)
        Assert.Equal<string list>([ "if"; "else"; "define" ], names)

[<Fact>]
let ``parse template`` () =
    let diags = DiagnosticBag.Empty
    // Build a rule ((_ a b) (cons a b)) and extract the template.
    let patStx = readExprAsStx "(_ a b)"
    let tmplStx = readExprAsStx "(cons a b)"
    let ruleStx = Stx.List([ patStx; tmplStx ], None, dummyLoc)
    let (_, tmpl) = MacroParse.parseSyntaxRule diags "..." [] emptyEnv ruleStx

    match tmpl with
    | MacroTemplate.Form(_, elements) when List.length elements = 3 -> ()
    | _ -> Assert.True(false, "Expected template to parse")

[<Fact>]
let ``find bound variables in pattern`` () =
    // Parsing (x y z) with z as a literal produces Variable nodes for the bound
    // names and a Literal node for z. The MacroPattern structure is the source of
    // truth for which names are bound — there is no separate findBound helper.
    let pat = parsePattern "_" "..." [ "z" ] (readExprAsStx "(x y z)") |> Result.unwrap

    match pat with
    | MacroPattern.Form [ MacroPattern.Variable "x"; MacroPattern.Variable "y"; MacroPattern.Literal("z", None) ] -> ()
    | other -> Assert.True(false, sprintf "Unexpected pattern: %A" other)

[<Fact>]
let ``parse syntax-rules form`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx = readExprAsStx "(syntax-rules (if else) ((if a b c) (a)))"

    let result =
        MacroParse.parseSyntaxRulesStx diags "test-macro" syntaxRulesStx (Map.empty: StxEnvironment)

    match result with
    | Some macro ->
        Assert.Equal(1, List.length macro.Rules)
        Assert.Equal<StxEnvironment>(emptyEnv, macro.DefScope)
    | None -> Assert.True(false, "Expected syntax-rules to parse")

[<Fact>]
let ``parse syntax-rules with custom ellipsis`` () =
    let diags = DiagnosticBag.Empty

    let syntaxRulesStx = readExprAsStx "(syntax-rules ::: () ((a :::) (a)))"

    let result =
        MacroParse.parseSyntaxRulesStx diags "test-macro" syntaxRulesStx (Map.empty: StxEnvironment)

    match result with
    | Some macro -> Assert.Equal(1, List.length macro.Rules)
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

// -- Old `Epxander` Test Cases ------------------------------------------------

/// Parse a `(syntax-rules ...)` Stx node from source into the structured
/// transformer representation (for pattern-inspection tests).
let private parseSyntaxRules (name: string) (source: string) : Macro =
    let registry = SourceRegistry.empty ()
    let prog = Parse.readProgram registry "test" source

    if ParseResult.hasErrors prog then
        failwithf "Parse error: %A" prog.Diagnostics

    let tree =
        prog.Root.Body
        |> Seq.exactlyOne
        |> fun expr -> Stx.ofExpr registry prog.Root.DocId DiagnosticBag.Empty expr

    let diag = DiagnosticBag.Empty
    let env = Environments.emptyStx

    match MacroParse.parseSyntaxRulesStx diag name tree env with
    | Some t -> t
    | None ->
        let msgs = diag.Diagnostics |> List.map (fun d -> d.Message) |> String.concat "; "

        failwithf "parseSyntaxRulesStx returned None: %s" msgs

[<Fact>]
let ``parseSyntaxRulesStx: simple identity rule`` () =
    let t = parseSyntaxRules "id" "(syntax-rules () ((_ x) x))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore; MacroPattern.Variable "x" ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: constant literal pattern`` () =
    let t = parseSyntaxRules "foo" "(syntax-rules () ((_ 123) 'ok))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore; MacroPattern.Constant(StxDatum.Number n) ] -> Assert.Equal(123.0, n)
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: keyword literal in literal list`` () =
    let t = parseSyntaxRules "my-macro" "(syntax-rules (else) ((_ else x) x))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore; MacroPattern.Literal("else", None); MacroPattern.Variable "x" ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: ellipsis pattern`` () =
    let t = parseSyntaxRules "my-list" "(syntax-rules () ((_ x ...) (list x ...)))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore; MacroPattern.Repeat(MacroPattern.Variable "x") ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: dotted rest pattern`` () =
    let t = parseSyntaxRules "f" "(syntax-rules () ((_ a b . c) c))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.DottedForm([ MacroPattern.Underscore; MacroPattern.Variable "a"; MacroPattern.Variable "b" ],
                              MacroPattern.Variable "c") -> ()
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: multiple rules`` () =
    let t =
        parseSyntaxRules "foo" "(syntax-rules () ((_ 123) 'one-two-three) ((_) \"bar\"))"

    Assert.Equal(2, List.length t.Rules)

[<Fact>]
let ``parseSyntaxRulesStx: keyword-name head binds as variable`` () =
    // When the head element matches the keyword name, it becomes Variable kw
    // so the template can refer to it (e.g. recursive `or` expansion).
    let t = parseSyntaxRules "or" "(syntax-rules () ((or a b ...) (if a a (or b ...))))"
    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore
                          MacroPattern.Variable "a"
                          MacroPattern.Repeat(MacroPattern.Variable "b") ] ->
        // Template should have or as Subst (it's in bound)
        match snd t.Rules.[0] with
        | MacroTemplate.Form(_, elems) ->
            // The (if ...) form; just verify it parsed without error
            Assert.NotEmpty(elems)
        | other -> failwithf "Expected Form template, got %A" other
    | other -> failwithf "Unexpected pattern: %A" other

[<Fact>]
let ``parseSyntaxRulesStx: custom ellipsis identifier`` () =
    // R7RS extended form: (syntax-rules <ellipsis> (literals...) rules...)
    // The custom ellipsis `dots` should be usable in place of `...`.
    let t =
        parseSyntaxRules "my-seq" "(syntax-rules dots () ((my-seq expr dots) (begin expr dots)))"

    Assert.Single(t.Rules) |> ignore

    match fst t.Rules.[0] with
    | MacroPattern.Form [ MacroPattern.Underscore; MacroPattern.Repeat(MacroPattern.Variable "expr") ] -> ()
    | other -> failwithf "Unexpected pattern: %A" other
