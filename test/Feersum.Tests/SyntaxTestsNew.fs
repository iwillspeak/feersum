module SyntaxTestsNew

open Xunit
open Firethorn.Red
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse

let readScriptExpr line =
    let result = Parse.readExpr1 "repl" line

    if result.Diagnostics |> List.isEmpty then
        result.Root
    else
        failwithf "Expected single expression but got errors: %A" result.Diagnostics

let readSingle line =
    let result = Parse.readRaw Parse.ReadMode.Script "repl" line

    if result.Diagnostics |> List.isEmpty then
        result.Root.Children() |> Seq.exactlyOne
    else
        failwithf "Expected single expression but got: %A" result.Diagnostics

let getKind (node: SyntaxNode) = node.Kind |> SyntaxUtils.greenToAst

let getTokenKind (token: SyntaxToken) = token.Kind |> SyntaxUtils.greenToAst

// open SyntaxUtils
// open SyntaxFactory

// TODO: negative cases for a lot of these parsers. e.g. unterminated strings,
//       invalid hex escapes, bad identifiers and so on.

// let sanitise =
//     sanitiseNodeWith (function
//         | _ -> dummyLocation)

// [<Fact>]
// let ``parse seqs`` () =
//     Assert.Equal(
//         Seq [ Number 1.0 |> Constant |> node
//               Number 23.0 |> Constant |> node ]
//         |> node,
//         readMany "1 23" |> sanitise
//     )

//     Assert.Equal(Seq [ Boolean true |> Constant |> node ] |> node, readMany "#t" |> sanitise)
//     Assert.Equal(Seq [] |> node, readMany "" |> sanitise)

//     Assert.Equal(
//         Seq [ Form [ Ident "+" |> node
//                      Number 12.0 |> Constant |> node
//                      Number 34.0 |> Constant |> node ]
//               |> node
//               Boolean false |> Constant |> node ]
//         |> node,
//         readMany "(+ 12 34) #f" |> sanitise
//     )

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
    let script = readScriptExpr raw
    let tree = script.RawNode.Children() |> Seq.exactlyOne

    Assert.Equal(AstKind.SYMBOL, tree |> getKind)

    let identTok =
        tree.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.exactlyOne

    Assert.Equal(AstKind.IDENTIFIER, identTok |> getTokenKind)

    match script.Body with
    | Some (Symbol s) -> Assert.Equal(cooked, s.CookedValue)
    | _ -> failwithf "Expected identifier but got %A" script.Body

// [<Theory>]
// [<InlineData("\\a", '\a')>]
// [<InlineData("\\b", '\b')>]
// [<InlineData("\\t", '\t')>]
// [<InlineData("\\n", '\n')>]
// [<InlineData("\\v", '\v')>]
// [<InlineData("\\f", '\f')>]
// [<InlineData("\\r", '\r')>]
// [<InlineData("\\\\", '\\')>]
// [<InlineData("\\\"", '"')>]
// [<InlineData("\\x0000A;", '\n')>]
// [<InlineData("\\x41;", 'A')>]
// [<InlineData("\\x1234;", '\u1234')>]
// let ``parse escaped characters`` escaped char =
//     Assert.Equal(Str(char |> string) |> Constant, readSingle (sprintf "\"%s\"" escaped))

// [<Fact>]
// let ``parse datum comment`` () =
//     Assert.Equal(
//         Number 1.0 |> Constant,
//         readSingle
//             "#;(= n 1)
//             1        ;Base case: return 1"
//     )

//     Assert.Equal(Number 123.0 |> Constant, readSingle "#;(= n 1)123")
//     Assert.Equal(Number 456.0 |> Constant, readSingle "#;123 456")

[<Fact>]
let ``parse block comments`` () =
    Assert.Equal(AstKind.CONSTANT, readSingle "#| this is a comment |#1" |> getKind)
    Assert.Equal(AstKind.CONSTANT, readSingle "1#| this is a comment |#" |> getKind)

    Assert.Equal(
        AstKind.CONSTANT,
        readSingle "#| this #| is a |# comment |#1"
        |> getKind
    )

// [<Theory>]
// [<InlineData('a')>]
// [<InlineData('b')>]
// [<InlineData('A')>]
// [<InlineData(' ')>]
// [<InlineData('#')>]
// [<InlineData('\\')>]
// [<InlineData('+')>]
// [<InlineData('.')>]
// [<InlineData('(')>]
// [<InlineData('?')>]
// [<InlineData('€')>]
// [<InlineData('§')>]
// [<InlineData('±')>]
// let ``parse simple character literals`` char =
//     Assert.Equal(Character char |> Constant, readSingle (@"#\" + string char))

// [<Theory>]
// [<InlineData("alarm", '\u0007')>]
// [<InlineData("backspace", '\u0008')>]
// [<InlineData("delete", '\u007F')>]
// [<InlineData("escape", '\u001B')>]
// [<InlineData("newline", '\u000A')>]
// [<InlineData("null", '\u0000')>]
// [<InlineData("return", '\u000D')>]
// [<InlineData("space", ' ')>]
// [<InlineData("tab", '\u0009')>]
// let ``parse named characters`` name char =
//     Assert.Equal(Character char |> Constant, readSingle (@"#\" + name))

// [<Theory>]
// [<InlineData(@"#\x03BB", 'λ')>]
// [<InlineData(@"#\x03bb", 'λ')>]
// [<InlineData(@"#\x20", ' ')>]
// let ``parse hex characters`` hex char =
//     Assert.Equal(Character char |> Constant, readSingle hex)

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
        |> Result.map (fun x ->
            x.Body
            |> Seq.map (SyntaxShim.transformExpr doc)
            |> Seq.exactlyOne)
        |> Result.unwrap

    printfn "@%A" tree.Location

    Assert.Equal(1L, tree.Location.Start.Line)
    Assert.Equal(0L, tree.Location.Start.Col)
    Assert.Equal(1L, tree.Location.End.Line)
    Assert.Equal(7L, tree.Location.End.Col)
