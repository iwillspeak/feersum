module SyntaxTestsNew

open Xunit
open Firethorn.Red
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Text

[<AutoOpen>]
module private Utils =

    let readScript line =
        let result = Parse.readExpr1 "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root
        else
            failwithf "Expected single expression but got errors: %A in source: %s" result.Diagnostics line

    let readScriptExpr line =
        match (readScript line).Item.Body with
        | Some expr -> expr
        | None -> failwithf "Expected single expression in %s" line

    let readProgExprs line =
        let result = Parse.readProgram "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root.Item.Body
        else
            failwithf "Expected program but got errors: %A in source %s" result.Diagnostics line

    let readSingle line =
        let result = Parse.readRaw Parse.ReadMode.Script "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root.Item.Children() |> Seq.exactlyOne
        else
            failwithf "Expected single expression but got: %A" result.Diagnostics

    let getKind (node: SyntaxNode) = node.Kind |> SyntaxUtils.greenToAst

    let getTokenKind (token: SyntaxToken) = token.Kind |> SyntaxUtils.greenToAst

[<Fact>]
let ``parse happy path`` () =

    match readProgExprs "1 23" |> List.ofSeq with
    | [ Constant(Some(NumVal 1.0)); Constant(Some(NumVal 23.0)) ] -> ()
    | x -> failwithf "Parse test failure, got %A" x

    match readProgExprs "#t" |> List.ofSeq with
    | [ Constant(Some(BoolVal true)) ] -> ()
    | x -> failwithf "Parse test failure, got %A" x

    Assert.Empty(readProgExprs "")

    match readProgExprs "(+ 12 34) #f" |> List.ofSeq with
    | [ Form [ Symbol(Ok "+"); Constant(Some(NumVal 12.0)); Constant(Some(NumVal 34.0)) ]; Constant(Some(BoolVal false)) ] ->
        ()
    | x -> failwithf "Parse test failure, got %A" x

[<Fact>]
let ``parse atoms`` () =
    Assert.Equal(AstKind.CONSTANT, readSingle "123.559" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "789" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle @"""hello\nworld""" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "\"\"" |> getKind)
    Assert.Equal(AstKind.SYMBOL, readSingle "nil" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "#f" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "#t" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "#false" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "#true" |> getKind)
    Assert.Equal(AstKind.VEC, readSingle "#(1 #true (3 4))" |> getKind)
    Assert.Equal(AstKind.BYTEVEC, readSingle "#u8(1 7 0 1)" |> getKind)

[<Theory>]
[<InlineData("?")>]
[<InlineData("+")>]
[<InlineData("*")>]
[<InlineData("/")>]
[<InlineData("-")>]
[<InlineData("a")>]
[<InlineData("test")>]
[<InlineData("test?")>]
[<InlineData("celsius->farenhiet")>]
[<InlineData("things")>]
let ``parse identifiers`` ident =
    let tree = readSingle ident

    Assert.Equal(AstKind.SYMBOL, tree |> getKind)

    let identTok =
        tree.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.exactlyOne

    Assert.Equal(AstKind.IDENTIFIER, identTok |> getTokenKind)
    Assert.Equal(ident, identTok.Green.Text)

[<Theory>]
[<InlineData("!")>]
[<InlineData("$")>]
[<InlineData("%")>]
[<InlineData("&")>]
[<InlineData("*")>]
[<InlineData("+")>]
[<InlineData("-")>]
[<InlineData("/")>]
[<InlineData(":")>]
[<InlineData("<")>]
[<InlineData("=")>]
[<InlineData(">")>]
[<InlineData("?")>]
[<InlineData("@")>]
[<InlineData("^")>]
[<InlineData("_")>]
[<InlineData("~")>]
[<InlineData("..")>]
[<InlineData("extended.")>]
[<InlineData("extended.identifier")>]
[<InlineData("...")>]
[<InlineData("+soup+")>]
[<InlineData("<=?")>]
[<InlineData("->string")>]
[<InlineData("a34kTMNs")>]
[<InlineData("lambda")>]
[<InlineData("list->vector")>]
[<InlineData("q")>]
[<InlineData("V17a")>]
[<InlineData("the-word-recursion-has-many-meanings")>]
let ``extended identifier characters`` ident =
    let tree = readSingle ident

    Assert.Equal(AstKind.SYMBOL, tree |> getKind)

    let identTok =
        tree.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.exactlyOne

    Assert.Equal(AstKind.IDENTIFIER, identTok |> getTokenKind)
    Assert.Equal(ident, identTok.Green.Text)

[<Theory>]
[<InlineData("|two words|", "two words")>]
[<InlineData(@"|two\x20;words|", "two words")>]
[<InlineData(@"|\t\t|", "\t\t")>]
[<InlineData(@"|\x9;\x9;|", "\t\t")>]
[<InlineData(@"|H\x65;llo|", "Hello")>]
[<InlineData(@"|\x3BB;|", "λ")>]
let ``identifier literals`` raw (cooked: string) =
    let script = readScript raw
    let tree = script.Item.RawNode.Children() |> Seq.exactlyOne

    Assert.Equal(AstKind.SYMBOL, tree |> getKind)

    let identTok =
        tree.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.exactlyOne

    Assert.Equal(AstKind.IDENTIFIER, identTok |> getTokenKind)

    match script.Item.Body with
    | Some(SymbolNode s) ->
        match s.CookedValue with
        | Ok value -> Assert.Equal(cooked, value)
        | Error msg -> failwithf "Expected valid identifier but got error: %s" msg
    | x -> failwithf "Expected identifier but got %A" x

[<Theory>]
[<InlineData("\\a", '\a')>]
[<InlineData("\\b", '\b')>]
[<InlineData("\\t", '\t')>]
[<InlineData("\\n", '\n')>]
[<InlineData("\\v", '\v')>]
[<InlineData("\\f", '\f')>]
[<InlineData("\\r", '\r')>]
[<InlineData("\\\\", '\\')>]
[<InlineData("\\\"", '"')>]
[<InlineData("\\x0000A;", '\n')>]
[<InlineData("\\x41;", 'A')>]
[<InlineData("\\x1234;", '\u1234')>]
let ``parse escaped characters`` escaped char =
    match readScriptExpr (sprintf "\"%s\"" escaped) with
    | Constant(Some(StrVal(Ok s))) ->
        Assert.Equal(1, s.Length)
        Assert.Equal(char, s[0])
    | Constant(Some(StrVal(Error msg))) -> failwithf "Unexpected string cook error: %s" msg
    | _ -> failwith "Expected string"

[<Fact>]
let ``parse datum comment`` () =
    let checkFor num (parsed: Expression) =
        match parsed with
        | Constant(Some(NumValNode n)) -> Assert.Equal(num, n.Value)
        | _ -> failwith "Expected constant value"

    "#;(= n 1)
        1        ;Base case: return 1"
    |> readScriptExpr
    |> checkFor 1.0

    "#;(= n 1)123" |> readScriptExpr |> checkFor 123.0
    "#;123 456" |> readScriptExpr |> checkFor 456.0

[<Fact>]
let ``parse block comments`` () =
    Assert.Equal(AstKind.CONSTANT, readSingle "#| this is a comment |#1" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "1#| this is a comment |#" |> getKind)

    Assert.Equal(AstKind.CONSTANT, readSingle "#| this #| is a |# comment |#1" |> getKind)

[<Theory>]
[<InlineData('a')>]
[<InlineData('b')>]
[<InlineData('A')>]
[<InlineData(' ')>]
[<InlineData('#')>]
[<InlineData('\\')>]
[<InlineData('+')>]
[<InlineData('.')>]
[<InlineData('(')>]
[<InlineData('?')>]
[<InlineData('€')>]
[<InlineData('§')>]
[<InlineData('±')>]
let ``parse simple character literals`` char =
    match readScriptExpr (@"#\" + string char) with
    | Constant(Some(CharVal(Some c))) -> Assert.Equal(char, c)
    | _ -> failwith "Expected character value"

[<Theory>]
[<InlineData("alarm", '\u0007')>]
[<InlineData("backspace", '\u0008')>]
[<InlineData("delete", '\u007F')>]
[<InlineData("escape", '\u001B')>]
[<InlineData("newline", '\u000A')>]
[<InlineData("null", '\u0000')>]
[<InlineData("return", '\u000D')>]
[<InlineData("space", ' ')>]
[<InlineData("tab", '\u0009')>]
let ``parse named characters`` name char =
    match readScriptExpr (@"#\" + name) with
    | Constant(Some(CharVal(Some c))) -> Assert.Equal(char, c)
    | _ -> failwith "Expected character value"

[<Theory>]
[<InlineData(@"#\x03BB", 'λ')>]
[<InlineData(@"#\x03bb", 'λ')>]
[<InlineData(@"#\x20", ' ')>]
[<InlineData(@"#\x", 'x')>]
let ``parse hex characters`` hex char =
    match readScriptExpr hex with
    | Constant(Some(CharVal(Some c))) -> Assert.Equal(char, c)
    | _ -> failwith "Expected character value"

[<Fact>]
let ``multiple diagnostics on error`` () =
    let source = "(- 1 § (display \"foo\")"
    let result = Parse.readExpr1 "repl" source
    Assert.True(List.length result.Diagnostics > 1)

[<Fact>]
let ``parse preserves source location`` () =
    let source = "(+ 1 2)"
    let doc = TextDocument.fromParts "test.scm" source
    let result = Parse.readExpr1 "test.scm" source
    Assert.Empty(result.Diagnostics)
    let expr = result.Root.Item.Body.Value
    let loc = TextDocument.rangeToLocation doc expr.SyntaxRange
    Assert.Equal(1, loc.Start.Line)
    Assert.Equal(1, loc.Start.Col)
    Assert.Equal(1, loc.End.Line)
    Assert.Equal(8, loc.End.Col)

// Negative test cases for error handling
module ErrorHandling =

    [<Theory>]
    [<InlineData("\"hello")>]
    [<InlineData("\"unclosed")>]
    [<InlineData("\"multi\nline")>]
    let ``unterminated strings produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Theory>]
    [<InlineData("|unclosed identifier")>]
    let ``unterminated identifier literals produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Theory>]
    [<InlineData("(+ 1 2")>]
    [<InlineData("(list (nested")>]
    [<InlineData("(a (b (c")>]
    let ``unterminated forms produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Theory>]
    [<InlineData("#(1 2")>]
    [<InlineData("#(unclosed")>]
    let ``unterminated vectors produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Theory>]
    [<InlineData("#u8(1 2")>]
    let ``unterminated byte vectors produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Theory>]
    [<InlineData("#| unclosed comment")>]
    [<InlineData("#| outer #| inner |#")>]
    let ``unterminated block comments produce diagnostics`` source =
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Fact>]
    let ``nested unterminated block comments produce diagnostics`` () =
        let source = "#| outer #| inner |# still unclosed"
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Fact>]
    let ``mismatched brackets in nested structures produce diagnostics`` () =
        let source = "(list [1 2)"
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Fact>]
    let ``parser recovers from errors and produces stub nodes`` () =
        let source = "(- 1 § (display \"foo\")"
        let result = Parse.readExpr1 "repl" source
        // Should have diagnostics but still produce a parse tree
        Assert.NotEmpty(result.Diagnostics)
        Assert.NotNull(result.Root)

    [<Fact>]
    let ``unterminated character literal produces diagnostic`` () =
        let source = "#\\"
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Fact>]
    let ``invalid characters in source produce diagnostics`` () =
        let source = "(+ 1 § 2)"
        let result = Parse.readExpr1 "test" source
        Assert.NotEmpty(result.Diagnostics)

    [<Fact>]
    let ``invalid char at beginning`` () =
        let source = ")"
        let result = Parse.readExpr1 "test" source
        let diag = Assert.Single result.Diagnostics
        Assert.Equal(1, diag.Location.Start.Line)
        Assert.Equal(1, diag.Location.Start.Col)
        Assert.Equal(1, diag.Location.End.Line)
        Assert.Equal(2, diag.Location.End.Col)

    [<Fact>]
    let ``unterminated form`` () =
        let source = "(+ 1 2"
        let result = Parse.readExpr1 "test" source
        let diag = Assert.Single result.Diagnostics
        // The parser reports errors on the EOF token on byte offset 0, which
        // we then see as line 1, col 1 in the source document.
        Assert.Equal(1, diag.Location.Start.Line)
        Assert.Equal(1, diag.Location.Start.Col)
        Assert.Equal(1, diag.Location.End.Line)
        Assert.Equal(1, diag.Location.End.Col)
