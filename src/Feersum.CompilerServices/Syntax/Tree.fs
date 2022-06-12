namespace Feersum.CompilerServices.Syntax

open Firethorn
open Firethorn.Green
open Firethorn.Red
open Feersum.CompilerServices.Diagnostics

module TreeNew =

    /// Node kind for each element in the raw tree.
    type AstKind =
        | ERROR = -1

        // nodes
        | EXPR_PROGRAM = 1
        | PROGRAM = 2
        | CONSTANT = 3
        | SYMBOL = 4

        // tokens
        | EOF = 101
        | NUMBER = 102
        | STRING = 103
        | BOOLEAN = 104
        | CHARACTER = 105
        | IDENTIFIER = 106
        | ATMOSPHERE = 107

    /// Type to represent parse results.
    type public ParseResult =
        { Errors: Diagnostic list
          Root: SyntaxNode }

    /// Convert an AST Kind to a raw `SyntaxKind`
    let astToGreen (kind: AstKind) = SyntaxKind(int kind)

    /// Convert a raw kind back to a syntax kind
    let greenToAst =
        function
        | SyntaxKind kind -> enum<AstKind> kind

    /// Debug dump. Writes a debug representation of the tree to stdout.
    let dump = Debug.debugDump (Debug.mappedFormatter greenToAst)

    type Number(red: SyntaxToken) =
        let red = red

        static member TryCast(red: SyntaxToken) : Option<Number> =
            if red.Kind = (AstKind.NUMBER |> astToGreen) then
                Some(new Number(red))
            else
                None

        member public x.Value =
            match red.Green.Text |> System.Double.TryParse with
            | (true, value) -> value
            | _ -> 0.0

    type ConstantKind = Number of Number

    type Constant(red: SyntaxNode) =
        let red = red

        static member TryCast(red: SyntaxNode) =
            if red.Kind = (AstKind.CONSTANT |> astToGreen) then
                new Constant(red) |> Some
            else
                None
