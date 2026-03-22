module Feersum.CompilerServices.Syntax.Factories

open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open Firethorn.Green
open Firethorn.Red
open Serehfa

let private tok kind text =
    GreenToken.Create(kind |> SyntaxUtils.astToGreen, text) |> GreenElement.Token

let private atmosphere = tok AstKind.ATMOSPHERE
let private space = atmosphere " "

let private constant docId kind value =
    new Constant(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.CONSTANT |> SyntaxUtils.astToGreen,
                [ tok kind (Write.GetExternalRepresentation(value)) ]
            )
        ),
        docId
    )

/// Create a Numeric Value Constant
///
/// Emits a syntax tree representing a single number
let numVal docId (n: double) = constant docId AstKind.NUMBER n

/// Create a Boolean Value Constant
///
/// Emit a syntax tree representing a single boolean value
let boolVal docId (b: bool) = constant docId AstKind.BOOLEAN b

/// Create a Character Value Constant
///
/// Emit a syntax tree representing a single character value
let charVal docId (c: char) = constant docId AstKind.CHARACTER c

/// Create aString Value Constant
///
/// Emit a syntax tree reperesenting a single string value
let strVal docId (s: string) = constant docId AstKind.STRING s

/// Wrap an Expression with a Quotation
///
/// Creates a new `Quotation`, wrapping the innter `Expression` to convert it
/// into a datum.
let quoted docId (e: Expression) =
    new Quoted(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.QUOTED_DATUM |> SyntaxUtils.astToGreen,
                [ tok AstKind.QUOTE "'"; e.RawNode.Green |> GreenElement.Node ]
            )
        ),
        docId
    )

/// Wrap a String as an Identifier Node
///
/// Produces a single well-formed symbol node contianing a single identifier.
let symbol docId ident =
    new Symbol(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.SYMBOL |> SyntaxUtils.astToGreen,
                [ tok AstKind.IDENTIFIER (Write.GetExternalRepresentation(new Ident(ident))) ]
            )
        ),
        docId
    )

/// Wrap a List of Expressions as a Form
///
/// Produces a simple well-formed form expression from a list of containing expressions.
let form docId (exprs: Expression list) =
    let close = tok AstKind.CLOSE_PAREN ")"
    let toNode (x: Expression) = x.RawNode.Green |> GreenElement.Node

    let rec mapTail (exprs: Expression list) =
        match exprs with
        | [] -> [ close ]
        | [ single ] -> [ single |> toNode; close ]
        | head :: tail -> (head |> toNode) :: (space :: (mapTail tail))

    new Form(
        SyntaxNode.CreateRoot(
            GreenNode.Create(AstKind.FORM |> SyntaxUtils.astToGreen, tok AstKind.OPEN_PAREN "(" :: mapTail exprs)
        ),
        docId
    )

/// Wrap Expression as Script
///
/// Creates a new root `ScriptProgram` from the given `Expression`
let scriptProgram docId (expr: Expression) : ScriptProgram =
    new ScriptProgram(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.SCRIPT_PROGRAM |> SyntaxUtils.astToGreen,
                [ expr.RawNode.Green |> GreenElement.Node ]
            )
        ),
        docId
    )

/// Wrap Expressions as Full Program
///
/// Creates a new root `Program` from the given `Expression` sequence
let program docId (exprs: seq<Expression>) : Program =
    new Program(
        SyntaxNode.CreateRoot(
            GreenNode.Create(
                AstKind.PROGRAM |> SyntaxUtils.astToGreen,
                exprs |> Seq.map ((_.RawNode.Green) >> GreenElement.Node) |> List.ofSeq
            )
        ),
        docId
    )


/// Convenience functions for tests and simple use cases
/// These create nodes with DocId.Synthetic for compiler-generated or test syntax
module Convenience =
    // FIXME: These functions are really just for testing. Should they live in the tests?

    /// Create a Numeric Value Constant with synthetic provenance
    let numVal (n: double) = numVal DocId.Synthetic n

    /// Create a Boolean Value Constant with synthetic provenance
    let boolVal (b: bool) = boolVal DocId.Synthetic b

    /// Create a Character Value Constant with synthetic provenance
    let charVal (c: char) = charVal DocId.Synthetic c

    /// Create a String Value Constant with synthetic provenance
    let strVal (s: string) = strVal DocId.Synthetic s

    /// Wrap an Expression with a Quotation with synthetic provenance
    let quoted (e: Expression) = quoted DocId.Synthetic e

    /// Wrap a String as an Identifier Node with synthetic provenance
    let symbol ident = symbol DocId.Synthetic ident

    /// Wrap a List of Expressions as a Form with synthetic provenance
    let form (exprs: Expression list) = form DocId.Synthetic exprs

    /// Wrap Expression as Script with synthetic provenance
    let scriptProgram (expr: Expression) = scriptProgram DocId.Synthetic expr

    /// Wrap Expressions as Full Program with synthetic provenance
    let program (exprs: seq<Expression>) = program DocId.Synthetic exprs
