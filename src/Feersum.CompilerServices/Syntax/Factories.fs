module Feersum.CompilerServices.Syntax.Factories

open Feersum.CompilerServices.Syntax.Tree
open Firethorn.Green
open Firethorn.Red

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
