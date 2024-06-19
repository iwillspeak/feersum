module SyntaxFactoriesTests

open Feersum.CompilerServices.Syntax.Factories
open Xunit
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree

[<Fact>]
let ``number nodes`` () =
    let node = numVal 123
    let reparsed = Parse.readExpr1 "testparse" node.Text
    let node2 = reparsed.Root.Body.Value

    Assert.Equal("123", node.Text)

    match node with
    | Constant(Some(NumVal 123.0)) -> ()
    | _ -> failwith "Node structure invalid"

    Assert.Empty(reparsed.Diagnostics)
    Assert.Equal(node2.RawNode.Kind, node.RawNode.Kind)
    Assert.Equal(node2.SyntaxRange, node.SyntaxRange)
