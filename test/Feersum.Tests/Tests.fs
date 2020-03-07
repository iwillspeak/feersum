module Tests

open Xunit
open Program
open SchemeRuntime
open Syntax

[<Fact>]
let ``Evaluate atoms`` () =
    Assert.Equal("#t", execute(Boolean true) |> externalRepr)
    Assert.Equal("#f", execute(Boolean false) |> externalRepr)
    Assert.Equal(@"""hello""", execute(Str "hello") |> externalRepr)
    Assert.Equal("1337", execute(Number 1337L) |> externalRepr)

[<Fact>]
let ``Evaluate lists`` () =
    Assert.Equal("132", execute(Seq [ Boolean false; Number 132L ]) |> externalRepr)
    Assert.Equal("NIL", execute(Seq [ ]) |> externalRepr)
    Assert.Equal("#t", execute(Seq [ Boolean true ]) |> externalRepr)

[<Fact>]
let ``Evaluate builtins`` () =
    Assert.Equal("19", execute(Form [ Ident "+"; Number 10L; Number 9L ]) |> externalRepr)
    Assert.Equal("901", execute(Form [ Ident "+"; Number 901L ]) |> externalRepr)
    Assert.Equal("90", execute(Form [ Ident "*"; Number 10L; Number 9L ]) |> externalRepr)
    Assert.Equal("901", execute(Form [ Ident "+"; Form [ Ident "*"; Number 100L; Number 9L ]; Number 1L]) |> externalRepr)
    Assert.Equal("1", execute(Form [ Ident "-"; Number 10L; Number 9L ]) |> externalRepr)
    Assert.Equal("2", execute(Form [ Ident "/"; Number 16L; Number 8L ]) |> externalRepr)
