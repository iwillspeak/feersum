module SyntaxFactoriesTests

open Feersum.CompilerServices.Syntax.Factories
open Xunit
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Firethorn.Red

let checkReparse (node: Expression) =
    let reparsed = Parse.readExpr1 "testparse" node.Text
    let node2 = reparsed.Root.Body.Value
    Assert.Empty(reparsed.Diagnostics)
    Assert.Equal(node2.RawNode.Kind, node.RawNode.Kind)
    Assert.Equal(node2.SyntaxRange, node.SyntaxRange)
    Assert.Equal(Debug.debugToStringRaw node2.RawNode, Debug.debugToStringRaw node.RawNode)

[<Fact>]
let ``number nodes`` () =
    let node = numVal 123

    Assert.Equal("123", node.Text)

    match node with
    | Constant(Some(NumVal 123.0)) -> ()
    | _ -> failwith "Node structure invalid"

    checkReparse node

[<Fact>]
let ``bool nodes`` () =
    let tNode = boolVal true
    let fNode = boolVal false

    Assert.Equal("#t", tNode.Text)
    Assert.Equal("#f", fNode.Text)

    match tNode with
    | Constant(Some(BoolVal true)) -> ()
    | _ -> failwith "Node structure invalid"

    match fNode with
    | Constant(Some(BoolVal false)) -> ()
    | _ -> failwith "Node structure invalid"

    checkReparse tNode
    checkReparse fNode
