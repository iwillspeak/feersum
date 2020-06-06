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