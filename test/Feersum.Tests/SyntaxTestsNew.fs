module SyntaxTestsNew

open Xunit
open Firethorn.Red
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse

// TODO: negative cases for a lot of these parsers. e.g. unterminated strings,
//       invalid hex escapes, bad identifiers and so on.

[<AutoOpen>]
module private Utils =

    let readScript line =
        let result = Parse.readExpr1 "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root
        else
            failwithf "Expected single expression but got errors: %A in source: %s" result.Diagnostics line

    let readScriptExpr line =
        match (readScript line).Body with
        | Some expr -> expr
        | None -> failwithf "Expected single expression in %s" line

    let readProgExprs line =
        let result = Parse.readProgram "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root.Body
        else
            failwithf "Expected program but got errors: %A in source %s" result.Diagnostics line

    let readSingle line =
        let result = Parse.readRaw Parse.ReadMode.Script "repl" line

        if result.Diagnostics |> List.isEmpty then
            result.Root.Children() |> Seq.exactlyOne
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
    | [ Form [ Symbol "+"; Constant(Some(NumVal 12.0)); Constant(Some(NumVal 34.0)) ]; Constant(Some(BoolVal false)) ] ->
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
// [<InlineData("@")>] TODO: Is this a valid identifier or not?
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
    let tree = script.RawNode.Children() |> Seq.exactlyOne

    Assert.Equal(AstKind.SYMBOL, tree |> getKind)

    let identTok =
        tree.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.exactlyOne

    Assert.Equal(AstKind.IDENTIFIER, identTok |> getTokenKind)

    match script.Body with
    | Some(SymbolNode s) -> Assert.Equal(cooked, s.CookedValue)
    | _ -> failwithf "Expected identifier but got %A" script.Body

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
    | Constant(Some(StrVal s)) ->
        Assert.Equal(1, s.Length)
        Assert.Equal(char, s[0])
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
    let result = Parse.readExpr source
    Assert.True(List.length result.Diagnostics > 1)

[<Fact>]
let ``syntax shim test`` () =
    let body = "(+ 1 2)"
    let doc = TextDocument.fromParts "a/file/path.scm" body

    let tree =
        readProgram doc.Path body
        |> ParseResult.toResult
        |> Result.map (fun x -> x.Body |> Seq.map (SyntaxShim.transformExpr doc) |> Seq.exactlyOne)
        |> Result.unwrap

    Assert.Equal(1L, tree.Location.Start.Line)
    Assert.Equal(1L, tree.Location.Start.Col)
    Assert.Equal(1L, tree.Location.End.Line)
    Assert.Equal(8L, tree.Location.End.Col)
