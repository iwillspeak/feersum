module SyntaxTests

open Xunit
open Syntax

let readSingle input =
    match readExpr input with
    | Ok(Seq exprs) -> List.exactlyOne exprs
    | Ok(expr) -> expr
    | o -> failwithf "Expected single expression but got: %A" o

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
[<InlineData("test?")>]
[<InlineData("celsius->farenhiet")>]
[<InlineData("things")>]
let ``parse identifiers`` ident =
    Assert.Equal(Ident ident, readSingle ident)