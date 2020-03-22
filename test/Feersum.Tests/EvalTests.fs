module EvalTests

open Xunit
open Interpret
open Syntax

[<Fact>]
let ``Evaluate atoms`` () =
    Assert.Equal("#t", execute(Boolean true) |> externalRepr)
    Assert.Equal("#f", execute(Boolean false) |> externalRepr)
    Assert.Equal(@"""hello""", execute(Str "hello") |> externalRepr)
    Assert.Equal("1337", execute(Number 1337.0) |> externalRepr)
    Assert.Equal("123.456", execute(Number 123.456) |> externalRepr)

[<Fact>]
let ``Evaluate lists`` () =
    Assert.Equal("132", execute(Seq [ Boolean false; Number 132.0 ]) |> externalRepr)
    Assert.Equal("NIL", execute(Seq [ ]) |> externalRepr)
    Assert.Equal("#t", execute(Seq [ Boolean true ]) |> externalRepr)

[<Fact>]
let ``Evaluate builtins`` () =
    Assert.Equal("19", execute(Form [ Ident "+"; Number 10.0; Number 9.0 ]) |> externalRepr)
    Assert.Equal("901", execute(Form [ Ident "+"; Number 901.0 ]) |> externalRepr)
    Assert.Equal("90", execute(Form [ Ident "*"; Number 10.0; Number 9.0 ]) |> externalRepr)
    Assert.Equal("901", execute(Form [ Ident "+"; Form [ Ident "*"; Number 100.0; Number 9.0 ]; Number 1.0]) |> externalRepr)
    Assert.Equal("1", execute(Form [ Ident "-"; Number 10.0; Number 9.0 ]) |> externalRepr)
    Assert.Equal("2", execute(Form [ Ident "/"; Number 16.0; Number 8.0 ]) |> externalRepr)
