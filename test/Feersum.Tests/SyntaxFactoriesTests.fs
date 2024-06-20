module SyntaxFactoriesTests

open Feersum.CompilerServices.Syntax.Factories
open Xunit
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Firethorn.Red

/// Validate that a node when re-parsed from the `.Text` will result in an
/// identical expression tree.
let private checkReparse (node: Expression) =
    let reparsed = Parse.readExpr1 "testparse" node.Text
    let node2 = reparsed.Root.Body.Value
    Assert.Empty(reparsed.Diagnostics)
    Assert.Equal(node2.RawNode.Kind, node.RawNode.Kind)
    Assert.Equal(node2.SyntaxRange, node.SyntaxRange)
    Assert.Equal(Debug.debugToStringRaw node2.RawNode, Debug.debugToStringRaw node.RawNode)

[<Fact>]
let ``number constants`` () =
    let node = numVal 123

    Assert.Equal("123", node.Text)

    match node with
    | Constant(Some(NumVal 123.0)) -> ()
    | _ -> failwith "Node structure invalid"

    checkReparse node

[<Fact>]
let ``boolean constants`` () =
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

[<Theory>]
[<InlineData('a', "#\\a")>]
[<InlineData('x', "#\\x")>]
[<InlineData('S', "#\\S")>]
[<InlineData('\n', "#\\x000a")>]
[<InlineData(' ', "#\\x0020")>]
[<InlineData('\\', "#\\\\")>]
let ``character constants`` ch exp =
    let node = charVal ch

    Assert.Equal(exp, node.Text)

    match node with
    | Constant(Some(CharVal(Some(c)))) -> Assert.Equal(ch, c)
    | _ -> failwith "Node structure invalid"

    checkReparse node

[<Theory>]
[<InlineData("a")>]
[<InlineData("hello world")>]
[<InlineData("")>]
[<InlineData("\n\t\a\v\\ \"\";()")>]
let ``string constants`` str =
    let node = strVal str

    match node with
    | Constant(Some(StrVal s)) -> Assert.Equal(str, s)
    | _ -> failwith "Node structure invalid"

    checkReparse node
