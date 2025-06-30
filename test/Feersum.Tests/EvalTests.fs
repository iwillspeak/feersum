module EvalTests

open Xunit

open Feersum.CompilerServices.Eval
open Feersum.CompilerServices.Utils
open Feersum.CompilerServices.Syntax.Parse
open Feersum.CompilerServices.Compile
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Factories
open Feersum.CompilerServices.Syntax.Tree

let private feeri = eval >> Result.unwrap >> cilExternalRepr

let private interpProg (prog: Program) =
    let doc = TextDocument.fromParts "test" prog.Text
    CompileInput.Program [ (doc, prog) ] |> feeri

let private interpScr (scr: ScriptProgram) =
    let doc = TextDocument.fromParts "test" scr.Text
    CompileInput.Script(doc, scr) |> feeri

let private tryReadSingle expr =
    let script = readExpr expr |> ParseResult.toResult |> Result.unwrap
    let doc = TextDocument.fromParts "test" expr
    CompileInput.Script(doc, script)

[<Fact>]
let ``evaluate atoms`` () =
    Assert.Equal("#t", interpScr (boolVal true |> scriptProgram))
    Assert.Equal("#f", interpScr (boolVal false |> scriptProgram))
    Assert.Equal(@"""hello""", interpScr (strVal "hello" |> scriptProgram))
    Assert.Equal("1337", interpScr (numVal 1337.0 |> scriptProgram))
    Assert.Equal("123.456", interpScr (numVal 123.456 |> scriptProgram))

[<Fact>]
let ``evaluate quoted atoms`` () =
    Assert.Equal("#t", interpScr (boolVal true |> quoted |> scriptProgram))
    Assert.Equal("#f", interpScr (boolVal false |> quoted |> scriptProgram))
    Assert.Equal(@"""hello""", interpScr (strVal "hello" |> quoted |> scriptProgram))
    Assert.Equal("1337", interpScr (numVal 1337.0 |> quoted |> scriptProgram))
    Assert.Equal("123.456", interpScr (numVal 123.456 |> quoted |> scriptProgram))

[<Fact>]
let ``evaluate seqs`` () =
    Assert.Equal("132", interpProg (program [ boolVal false; numVal 132.0 ]))
    Assert.Equal("#t", interpProg (program [ boolVal true ]))

[<Fact>]
let ``Evaluate empty program`` () =
    Assert.Equal("; Unspecified value", interpProg ([] |> program))

[<Fact>]
let ``Evaluate lambdas returns`` () =
    Assert.Equal(
        "123",
        interpScr (
            form [ form [ symbol "lambda"; form [ symbol "x" ]; symbol "x" ]; numVal 123.0 ]
            |> scriptProgram
        )
    )

[<Fact>]
let ``Evaluate builtins`` () =
    Assert.Equal("19", interpScr (form [ symbol "+"; numVal 10.0; numVal 9.0 ] |> scriptProgram))

    Assert.Equal("901", interpScr (form [ symbol "+"; numVal 901.0 ] |> scriptProgram))

    Assert.Equal("90", interpScr (form [ symbol "*"; numVal 10.0; numVal 9.0 ] |> scriptProgram))

    Assert.Equal(
        "901",
        interpScr (
            form [ symbol "+"; form [ symbol "*"; numVal 100.0; numVal 9.0 ]; numVal 1.0 ]
            |> scriptProgram
        )
    )

    Assert.Equal("1", interpScr (form [ symbol "-"; numVal 10.0; numVal 9.0 ] |> scriptProgram))

    Assert.Equal("2", interpScr (form [ symbol "/"; numVal 16.0; numVal 8.0 ] |> scriptProgram))

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
[<InlineData("(/ 3)", "0.3333333333333333")>]
let ``evaluate artithemtic ops`` expr result =
    let expr = tryReadSingle expr
    Assert.Equal(result, feeri (expr))

[<Theory>]
[<InlineData("=")>]
[<InlineData(">")>]
[<InlineData("<")>]
[<InlineData(">=")>]
[<InlineData("<=")>]
let ``comp ops return true for simple cases`` op =
    Assert.Equal("#t", interpScr (form [ symbol op ] |> scriptProgram))

    Assert.Equal("#t", interpScr (form [ symbol op; numVal 123.456 ] |> scriptProgram))

[<Theory>]
[<InlineData("(= 23 4234 234)", "#f")>]
[<InlineData("(< 1 2 3)", "#t")>]
[<InlineData("(< 1 2 4 8)", "#t")>]
[<InlineData("(< 1 2 1)", "#f")>]
[<InlineData("(> 5 4 2 1)", "#t")>]
[<InlineData("(>= 5 5 5 4 4 3 3 3 3 1 )", "#t")>]
let ``evaluate comparision ops`` expr result =
    let expr = tryReadSingle expr
    Assert.Equal(result, feeri (expr))

[<Theory>]
[<InlineData("(let () 123)", "123")>]
[<InlineData("(let ((x 2) (y 3)) (* x y))", "6")>]
[<InlineData("(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))", "35")>]
[<InlineData("(let* () 456)", "456")>]
[<InlineData("(let* ((a 111)) (+ a))", "111")>]
let ``let expressions`` expr result =
    let parsed = sprintf "((lambda () %s ))" expr |> tryReadSingle

    Assert.Equal(result, feeri (parsed))
    let parsed = expr |> tryReadSingle
    Assert.Equal(result, feeri (parsed))
