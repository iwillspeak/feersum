module EvalTests

open Xunit

open Interpret
open Eval
open Syntax
open SyntaxUtils
open SyntaxFactory
open Diagnostics

let private expectOk = function
    | Ok o -> o
    | e -> failwithf "Expected OK, but got %A" e

let interpret = execute >> expectOk >> externalRepr
let feeri = eval >> expectOk >> cilExternalRepr

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
    Assert.Equal("#t", evaluator(Boolean true |> constant))
    Assert.Equal("#f", evaluator(Boolean false |> constant))
    Assert.Equal(@"""hello""", evaluator(Str "hello" |> constant))
    Assert.Equal("1337", evaluator(Number 1337.0 |> constant))
    Assert.Equal("123.456", evaluator(Number 123.456 |> constant))

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate lists`` evaluator =
    Assert.Equal("132", evaluator(Seq [ Boolean false |> constant; Number 132.0  |> constant] |> node))
    Assert.Equal("#t", evaluator(Seq [ Boolean true |> constant ] |> node))

// TODO: Empty program yields NULL for eval, but special undefined value for
//       the interpreter. We should emit an instance of `Undefined` in
//       `emitUnspecified`.
[<Fact>]
let ``Evaluate empty program`` () =
    Assert.Equal("; unspecified value", interpret(Seq [ ] |> node))
    Assert.Equal("()", feeri(Seq [ ] |> node))

[<Fact>]
let ``Evaluate lambdas returns`` () =
    Assert.Equal("123", feeri(Form [
        Form [ Ident "lambda" |> node ; Form [ Ident "x" |> node ] |> node; Ident "x" |> node] |> node;
        Number 123.0 |> constant] |> node))

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate builtins`` evaluator =
    Assert.Equal("19", evaluator(Form [ Ident "+" |> node; Number 10.0 |> constant; Number 9.0 |> constant ] |> node))
    Assert.Equal("901", evaluator(Form [ Ident "+" |> node; Number 901.0 |> constant ] |> node))
    Assert.Equal("90", evaluator(Form [ Ident "*" |> node; Number 10.0 |> constant; Number 9.0 |> constant ] |> node))
    Assert.Equal("901", evaluator(Form [ Ident "+" |> node; Form [ Ident "*" |> node; Number 100.0 |> constant; Number 9.0 |> constant ]|> node; Number 1.0 |> constant] |> node))
    Assert.Equal("1", evaluator(Form [ Ident "-" |> node; Number 10.0 |> constant; Number 9.0 |> constant ] |> node))
    Assert.Equal("2", evaluator(Form [ Ident "/" |> node; Number 16.0 |> constant; Number 8.0 |> constant ] |> node))

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
    Assert.Equal("#t", feeri(Form [ Ident op  |> node] |> node))
    Assert.Equal("#t", feeri(Form [ Ident op |> node; Number 123.456 |> constant ] |> node))

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

[<Theory>]
[<InlineData("(let () 123)", "123")>]
[<InlineData("(let ((x 2) (y 3)) (* x y))", "6")>]
[<InlineData("(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))", "35")>]
[<InlineData("(let* () 456)", "456")>]
[<InlineData("(let* ((a 111)) (+ a))", "111")>]
let ``let expressions`` expr result =
    let parsed = 
        sprintf "((lambda () %s ))" expr
        |> tryReadSingle
    Assert.Equal(result, feeri(parsed))
    let parsed = expr |> tryReadSingle
    Assert.Equal(result, feeri(parsed))
