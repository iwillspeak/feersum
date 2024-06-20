module Feersum.CompilerServices.Syntax.Factories

open Feersum.CompilerServices.Syntax.Tree
open Firethorn.Green
open Firethorn.Red
open Serehfa

let private constant kind value =
    new Constant(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.CONSTANT |> SyntaxUtils.astToGreen,
                [ GreenToken.Create(kind |> SyntaxUtils.astToGreen, Write.GetExternalRepresentation(value))
                  |> GreenElement.Token ]
            )
        )
    )


/// Create a Numeric Value Constant
///
/// Emits a syntax tree representing a single number
let numVal (n: double) = constant AstKind.NUMBER n

/// Create a Boolean Value Constant
///
/// Emit a syntax tree reprsenting a single boolean value
let boolVal (b: bool) = constant AstKind.BOOLEAN b

/// Create a Character Value Constant
///
/// Emit a syntax tree representing a single character value
let charVal (c: char) = constant AstKind.CHARACTER c

/// Create aString Value Constant
///
/// Emit a syntax tree reperesenting a single string value
let strVal (s: string) = constant AstKind.STRING s

/// Wrap an Expression with a Quotation
///
/// Creates a new `Quotation`, wrapping the innter `Expression` to convert it
/// into a datum.
let quoted (e: Expression) =
    new Quoted(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.QUOTED_DATUM |> SyntaxUtils.astToGreen,
                [ GreenToken.Create(AstKind.QUOTE |> SyntaxUtils.astToGreen, "'")
                  |> GreenElement.Token
                  e.RawNode.Green |> GreenElement.Node ]
            )
        )
    )

/// Wrap Expression as Script
///
/// Creates a new root `ScriptProgram` from the given `Expression`
let scriptProgram (expr: Expression) : ScriptProgram =
    new ScriptProgram(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.SCRIPT_PROGRAM |> SyntaxUtils.astToGreen,
                [ expr.RawNode.Green |> GreenElement.Node ]
            )
        )
    )

/// Wrap Expressions as Full Program
///
/// Creates a new root `Program` from the given `Expression` sequence
let program (exprs: seq<Expression>) : Program =
    new Program(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.PROGRAM |> SyntaxUtils.astToGreen,
                exprs |> Seq.map ((_.RawNode.Green) >> GreenElement.Node) |> List.ofSeq
            )
        )
    )
