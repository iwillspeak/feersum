module Feersum.CompilerServices.Syntax.Factories

open Feersum.CompilerServices.Syntax.Tree
open Firethorn.Green
open Firethorn.Red

/// Create a Numeric Value Constant
///
/// Emits a syntax tree representing a single number
let numVal n =
    new Constant(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.CONSTANT |> SyntaxUtils.astToGreen,
                [ GreenToken.Create(AstKind.NUMBER |> SyntaxUtils.astToGreen, sprintf "%d" n)
                  |> GreenElement.Token ]
            )
        )
    )

/// Create a Boolean Value Constant
///
/// Emit a syntax tree reprsenting a single boolean value
let boolVal b =
    let literal = if b then "#t" else "#f"

    new Constant(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.CONSTANT |> SyntaxUtils.astToGreen,
                [ GreenToken.Create(AstKind.BOOLEAN |> SyntaxUtils.astToGreen, literal)
                  |> GreenElement.Token ]
            )
        )
    )
