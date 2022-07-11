module Feersum.CompilerServices.Syntax.SyntaxShim

open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text

type LegacyNode = Feersum.CompilerServices.Syntax.AstNode
type LegacyNodeKind<'a> = Feersum.CompilerServices.Syntax.AstNodeKind<'a>

/// Transform a single expression into a legcy AST
let rec transformExpr (doc: TextDocument) (expr: Expression): LegacyNode =
    let kind =
        match expr with
        | Form f ->
            f.Body 
            |> Seq.map (transformExpr doc)
            |> List.ofSeq
            |> LegacyNodeKind.Form
        | _ ->
            // TODO: All the other node kinds
            LegacyNodeKind.Error
    { Kind = kind; Location = TextDocument.rangeToLocation doc expr.SyntaxRange }

/// Transform a program into a legacy AST
let transformProgram (doc: TextDocument) (prog: Program): LegacyNode =
    let body =
        prog.Body
        |> Seq.map (transformExpr doc)
        |> List.ofSeq
        |> LegacyNodeKind.Seq
    { Kind = body; Location = TextDocument.rangeToLocation doc prog.SyntaxRange }
