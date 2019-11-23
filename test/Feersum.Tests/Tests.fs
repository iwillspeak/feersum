module Tests

open Xunit
open Program

[<Fact>]
let ``Evaluate atoms`` () =
    Assert.Equal(SchemeValue.Boolean true, execute(Syntax.AstNode.Boolean true))
    Assert.Equal(SchemeValue.Boolean false, execute(Syntax.AstNode.Boolean false))
    Assert.Equal(SchemeValue.Str "hello", execute(Syntax.AstNode.Str "hello"))
    Assert.Equal(SchemeValue.Number 1337L, execute(Syntax.AstNode.Number 1234L))
