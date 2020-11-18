module MacroTests

open Xunit
open Macros

open SyntaxUtils
open SyntaxFactory
open Syntax
open Utils

let private tryMatch literals pattern haystack =
    let macroPat =
        match (readSingleNode pattern) |> parsePattern literals with
        | Result.Ok p -> p
        | Result.Error e -> failwithf "Could not parse macro pattern %A" e

    let syntaxTree = readSingleNode haystack
    match macroMatch macroPat syntaxTree with
    | Result.Ok b -> Some b
    | _ -> None

let private tryExpand transformer bindings =
    let transformer =
        readSingleNode transformer
        |> parseTransformer
        |> ResultEx.unwrap

    match macroExpand transformer bindings with
        | Result.Ok expanded -> Some expanded
        | Result.Error e -> failwithf "Error expanding macro %A" e

let private ppConst = function
    | SyntaxConstant.Number n -> n.ToString("g")
    | SyntaxConstant.Boolean b -> if b then "#t" else "#f"
    | c -> failwithf "unsupported constant %A" c

let private pp syntax =
    match syntax.Kind with
        | Constant c -> ppConst c
        | _ -> failwith "unsupported syntax kind"
    
let private assertMatches pattern syntax =
        match macroMatch pattern syntax with
        | Result.Ok bindings -> bindings
        | o -> failwithf "Pattern variable did not match %A" o

[<Fact>]
let ``patterns with constant number`` () =

    let testConstantMatch c =
        let pattern = MacroPattern.Constant c

        match macroMatch pattern (SyntaxFactory.constant c) with
        | Result.Ok _ -> ()
        | Result.Error e -> failwithf "%A" e
    
    testConstantMatch (Number 101.0)
    testConstantMatch (Number 0.0)
    testConstantMatch (Character 'a')
    testConstantMatch (Boolean true)
    testConstantMatch (Boolean false)
    testConstantMatch (Str "")
    testConstantMatch (Str "§2")

[<Theory>]
[<InlineData("if", "if", true)>]
[<InlineData("if", "else", false)>]
[<InlineData("else", "else", true)>]
[<InlineData("test-thing", "test-thing", true)>]
[<InlineData("...", "...", true)>]
let ``literal identifiers only match their literal values`` pattern id expected =
    let pattern = MacroPattern.Literal pattern
    let actual =
        match macroMatch pattern (Ident id |> node) with
        | Ok _ -> true
        | _ -> false
    Assert.Equal(expected, actual)

[<Fact>]
let ``variable patterns match anything`` () =

    let testVarMatch syntax =
        let pattern = MacroPattern.Variable "test"

        match macroMatch pattern syntax with
        | Result.Ok [("test", s)] ->
            Assert.Same(syntax, s)
        | o -> failwithf "Pattern variable did not match %A" o
    
    testVarMatch (Number 101.0 |> constant)
    testVarMatch (Number 0.0 |> constant)
    testVarMatch (Character 'a' |> constant)
    testVarMatch (Boolean true |> constant)
    testVarMatch (Boolean false |> constant)
    testVarMatch (Str "" |> constant)
    testVarMatch (Str "§2" |> constant)
    testVarMatch (Form [] |> node)
    testVarMatch (Ident "test" |> node)
    testVarMatch (Form [ Ident "foodafdf" |> node ] |> node)
    testVarMatch (Form [ number 123.0 ] |> node)

[<Fact>]
let ``underscore patterns match anything`` () =

    let testUnderscoreMatch syntax =
        let pattern = MacroPattern.Underscore

        match macroMatch pattern syntax with
        | Result.Ok [] -> ()
        | o -> failwithf "Pattern variable did not match %A" o
    
    testUnderscoreMatch (Number 101.0 |> constant)
    testUnderscoreMatch (Number 0.0 |> constant)
    testUnderscoreMatch (Character 'a' |> constant)
    testUnderscoreMatch (Boolean true |> constant)
    testUnderscoreMatch (Boolean false |> constant)
    testUnderscoreMatch (Str "" |> constant)
    testUnderscoreMatch (Str "§2" |> constant)
    testUnderscoreMatch (Form [] |> node)
    testUnderscoreMatch (Ident "test" |> node)
    testUnderscoreMatch (Form [ Ident "foodafdf" |> node ] |> node)
    testUnderscoreMatch (Form [ number 123.0 ] |> node)

[<Fact>]
let ``simple form patterns`` () =

    Assert.Equal<_ list>([], assertMatches (MacroPattern.Form []) (Form [] |> node))
    Assert.Equal<_ list>([], assertMatches (MacroPattern.Form [ MacroPattern.Constant (Boolean false); MacroPattern.Constant (Str "frob"); MacroPattern.Constant (Number 123.56)]) (Form [ constant (Boolean false); constant (Str "frob"); number 123.56] |> node))
    let testNode = (number 123.4)
    Assert.Equal<_ list>([("test", testNode)],
                         assertMatches (MacroPattern.Form [ MacroPattern.Variable "test" ]) (Form [ testNode ] |> node))
    
[<Fact>]
let ``dotted form patterns`` () =

    // FIXME: this pattern is nonsense '( . 123.4)' matching (123.4)
    Assert.Equal<_ list>([], assertMatches (MacroPattern.DottedForm([], MacroPattern.Constant (Number 123.4))) (Form [ number 123.4 ] |> node))
    let headNode = (number 123.4)
    let tailNode = (number 567.8)
    // (head . tail)
    Assert.Equal<_ list>([("head", headNode);("tail", tailNode)],
                         assertMatches (MacroPattern.DottedForm([ MacroPattern.Variable "head" ], MacroPattern.Variable "tail")) (Form [ headNode; tailNode ] |> node))

[<Theory>]
[<InlineData("a","1", true)>]
[<InlineData("1.0","1", true)>]
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
let ``macro parse tests`` pattern syntax shouldMatch =
    let literals = [ "foo"; "bar" ]
    Assert.Equal(shouldMatch, tryMatch literals pattern syntax |> Option.isSome)

[<Fact>]
let ``simple macro expand`` () =
    let expanded = macroExpand (MacroTemplate.Quoted (number 123.0)) []
    Assert.Equal(Ok(number 123.0), expanded);

    let expanded = macroExpand (MacroTemplate.Subst "test") [("test", constant (Boolean true))]
    Assert.Equal(Ok(constant (Boolean true)), expanded)

    let expanded = macroExpand (MacroTemplate.Subst "thing") []
    Assert.True(ResultEx.isError expanded)

[<Theory>]
[<InlineData("(a)", "a", "(1)", "1")>]
[<InlineData("(a ...)", "123", "(1 #f foo)", "123")>]
let ``macro expand tests`` pattern transformer invocation expected =
    let bindings = tryMatch [] pattern invocation |> OptionEx.unwrap
    let expanded = tryExpand transformer bindings |> OptionEx.unwrap
    Assert.Equal(expected, pp expanded)
