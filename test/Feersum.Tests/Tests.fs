module Tests

open Xunit
open Program
open Syntax

[<Fact>]
let ``Evaluate atoms`` () =
    Assert.Equal("#t", execute(Boolean true) |> externalRepr)
    Assert.Equal("#f", execute(Boolean false) |> externalRepr)
    Assert.Equal(@"""hello""", execute(Str "hello") |> externalRepr)
    Assert.Equal("1337", execute(Number 1337L) |> externalRepr)

