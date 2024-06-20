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
