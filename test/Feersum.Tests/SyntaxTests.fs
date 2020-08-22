module SyntaxTests

open Xunit
open Syntax

let readSingle input =
    match readExpr input with
    | Ok(Seq exprs) -> List.exactlyOne exprs
    | Ok(expr) -> expr
    | o -> failwithf "Expected single expression but got: %A" o

let readMany input =
    match readExpr input with
    | Ok(read) -> read
    | Error(e) -> failwithf "Expected one or more expressions but got: %A" e

[<Fact>]
let ``parse seqs`` () =
    Assert.Equal(Seq [ Number 1.0; Number 23.0], readMany "1 23")
    Assert.Equal(Seq [ Boolean true ], readMany "#t")
    Assert.Equal(Seq [ ], readMany "")
    Assert.Equal(Seq [ Form [ Ident "+"; Number 12.0; Number 34.0 ]; Boolean false], readMany "(+ 12 34) #f")

[<Fact>]
let ``parse atoms`` () =
    Assert.Equal(Number 123.559, readSingle "123.559")
    Assert.Equal(Number 789.0, readSingle "789")
    Assert.Equal(Str "hello\nworld", readSingle @"""hello\nworld""")
    Assert.Equal(Str "", readSingle("\"\""))
    Assert.Equal(Ident "nil", readSingle "nil")
    Assert.Equal(Boolean true, readSingle "#t")
    Assert.Equal(Boolean false, readSingle "#f")
    Assert.Equal(Boolean true, readSingle "#true")
    Assert.Equal(Boolean false, readSingle "#false")
    Assert.Equal(Dot, readSingle ".")

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
    Assert.Equal(Ident ident, readSingle ident)

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
    Assert.Equal(Ident ident, readSingle ident)

[<Theory>]
[<InlineData("|two words|", "two words")>]
[<InlineData(@"|two\x20;words|", "two words")>]
[<InlineData(@"|\t\t|", "\t\t")>]
[<InlineData(@"|\x9;\x9;|", "\t\t")>]
[<InlineData(@"|H\x65;llo|", "Hello")>]
[<InlineData(@"|\x3BB;|", "λ")>]
let ``identifier literals`` raw cooked =
    Assert.Equal(Ident cooked, readSingle raw)

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
    Assert.Equal(Str (char |> string), readSingle (sprintf "\"%s\"" escaped))

[<Fact>]
let ``parse datum comment`` () =
    Assert.Equal(Number 1.0, readSingle "#;(= n 1)
            1        ;Base case: return 1")
    Assert.Equal(Number 123.0, readSingle "#;(= n 1)123")
    Assert.Equal(Number 456.0, readSingle "#;123 456")

[<Fact>]
let ``parse block comments`` () =
    Assert.Equal(Number 1.0, readSingle "#| this is a comment |#1")
    Assert.Equal(Number 1.0, readSingle "1#| this is a comment |#")
    Assert.Equal(Number 1.0, readSingle "#| this #| is a |# comment |#1")
