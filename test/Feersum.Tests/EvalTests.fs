module EvalTests

open Xunit
open Interpret
open Eval
open Syntax

let interpret = execute >> externalRepr
let feeri = eval >> cilExternalRepr

let evaluators = [|
        [| interpret |];
        [| feeri |]
    |]

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate atoms`` evaluator =
    Assert.Equal("#t", evaluator(Boolean true))
    Assert.Equal("#f", evaluator(Boolean false))
    Assert.Equal(@"""hello""", evaluator(Str "hello"))
    Assert.Equal("1337", evaluator(Number 1337.0))
    Assert.Equal("123.456", evaluator(Number 123.456))

[<Theory>]
[<MemberData("evaluators")>]
let ``Evaluate lists`` evaluator =
    Assert.Equal("132", evaluator(Seq [ Boolean false; Number 132.0 ]))
    Assert.Equal("#t", evaluator(Seq [ Boolean true ]))

// TODO: Empty program yields NULL for eval, but special undefined value for
//       the interpreter. How can we represent undefined values in .NET?
[<Fact>]
let ``Evaluate empty program`` () =
    Assert.Equal("; unspecified value", interpret(Seq [ ]))
    Assert.Equal("()", feeri(Seq [ ]))

// TODO: Number builtins are only implemented in the interpreter right now.
[<Fact>]
let ``Evaluate builtins`` () =
    let evaluator = interpret
    Assert.Equal("19", evaluator(Form [ Ident "+"; Number 10.0; Number 9.0 ]))
    Assert.Equal("901", evaluator(Form [ Ident "+"; Number 901.0 ]))
    Assert.Equal("90", evaluator(Form [ Ident "*"; Number 10.0; Number 9.0 ]))
    Assert.Equal("901", evaluator(Form [ Ident "+"; Form [ Ident "*"; Number 100.0; Number 9.0 ]; Number 1.0]))
    Assert.Equal("1", evaluator(Form [ Ident "-"; Number 10.0; Number 9.0 ]))
    Assert.Equal("2", evaluator(Form [ Ident "/"; Number 16.0; Number 8.0 ]))
