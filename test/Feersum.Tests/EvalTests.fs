module EvalTests

open Xunit
open Interpret
open Eval
open Syntax

let interpret = execute >> externalRepr
let feeri = eval >> cilExternalRepr

let private dummyLocation = Point(FParsec.Position("dummy", 0L, 0L, 0L))

let n node =
    { Kind = node; Location = dummyLocation }

let evaluators = [|
        [| interpret |];
        [| feeri |]
    |]

let private tryReadSingle expr =
    match readExpr expr with
    | (o, []) -> o
    | (_, diagnostics) -> failwithf "parse error %A" diagnostics

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate atoms`` evaluator =
    Assert.Equal("#t", evaluator(Boolean true |> n))
    Assert.Equal("#f", evaluator(Boolean false |> n))
    Assert.Equal(@"""hello""", evaluator(Str "hello" |> n))
    Assert.Equal("1337", evaluator(Number 1337.0 |> n))
    Assert.Equal("123.456", evaluator(Number 123.456 |> n))

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate lists`` evaluator =
    Assert.Equal("132", evaluator(Seq [ Boolean false |> n; Number 132.0  |> n] |> n))
    Assert.Equal("#t", evaluator(Seq [ Boolean true |> n ] |> n))

// TODO: Empty program yields NULL for eval, but special undefined value for
//       the interpreter. How can we represent undefined values in .NET?
[<Fact>]
let ``Evaluate empty program`` () =
    Assert.Equal("; unspecified value", interpret(Seq [ ] |> n))
    Assert.Equal("()", feeri(Seq [ ] |> n))

[<Fact>]
let ``Evaluate lambdas returns`` () =
    Assert.Equal("123", feeri(Form [
        Form [ Ident "lambda" |> n ; Form [ Ident "x" |> n ] |> n; Ident "x" |> n] |> n;
        Number 123.0 |> n] |> n))

// TODO: Number builtins are only implemented in the interpreter right now.
[<Fact>]
let ``Evaluate builtins`` () =
    let evaluator = interpret
    Assert.Equal("19", evaluator(Form [ Ident "+" |> n; Number 10.0 |> n; Number 9.0 |> n ] |> n))
    Assert.Equal("901", evaluator(Form [ Ident "+" |> n; Number 901.0 |> n ] |> n))
    Assert.Equal("90", evaluator(Form [ Ident "*" |> n; Number 10.0 |> n; Number 9.0 |> n ] |> n))
    Assert.Equal("901", evaluator(Form [ Ident "+" |> n; Form [ Ident "*" |> n; Number 100.0 |> n; Number 9.0 |> n ]|> n; Number 1.0 |> n] |> n))
    Assert.Equal("1", evaluator(Form [ Ident "-" |> n; Number 10.0 |> n; Number 9.0 |> n ] |> n))
    Assert.Equal("2", evaluator(Form [ Ident "/" |> n; Number 16.0 |> n; Number 8.0 |> n ] |> n))

[<Theory>]
[<InlineData("(+ 3 4)", "7")>]
[<InlineData("(+ 3)", "3")>]
[<InlineData("(+)", "0")>]
[<InlineData("(* 4)", "4")>]
[<InlineData("(*)", "1")>]
[<InlineData("(- 3 4)", "-1")>]
[<InlineData("(- 3 4 5)", "-6")>]
[<InlineData("(- 3)", "-3")>]
// The following two aren't _quite_ right, but
//  ┏┓
//  ┃┃╱╲ In this
//  ┃╱╱╲╲ house
//  ╱╱╭╮╲╲ we only
//  ▔▏┗┛▕▔ perform
//  ╱▔▔▔▔▔▔▔▔▔▔╲
//  inexact maths
//  ╱╱┏┳┓╭╮┏┳┓ ╲╲
//  ▔▏┗┻┛┃┃┗┻┛▕▔
[<InlineData("(/ 3 4 5)", "0.15")>]
[<InlineData("(/ 3)", "0.333333")>]
let ``evaluate artithemtic ops`` expr result =
    let expr = tryReadSingle expr
    Assert.Equal(result, feeri(expr))


[<Theory>]
[<InlineData("=")>]
[<InlineData(">")>]
[<InlineData("<")>]
[<InlineData(">=")>]
[<InlineData("<=")>]
let ``comp ops return true for simple cases`` op =
    Assert.Equal("#t", feeri(Form [ Ident op  |> n] |> n))
    Assert.Equal("#t", feeri(Form [ Ident op |> n; Number 123.456 |> n ] |> n))

[<Theory>]
[<InlineData("(= 23 4234 234)", "#f")>]
[<InlineData("(< 1 2 3)", "#t")>]
[<InlineData("(< 1 2 4 8)", "#t")>]
[<InlineData("(< 1 2 1)", "#f")>]
[<InlineData("(> 5 4 2 1)", "#t")>]
[<InlineData("(>= 5 5 5 4 4 3 3 3 3 1 )", "#t")>]
let ``evaluate comparision ops`` expr result =
    let expr = tryReadSingle expr
    Assert.Equal(result, feeri(expr))