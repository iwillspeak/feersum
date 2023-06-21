module Feersum.CompilerServices.Syntax.SyntaxShim

open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text

type LegacyNode = Feersum.CompilerServices.Syntax.AstNode
type LegacyNodeKind<'a> = Feersum.CompilerServices.Syntax.AstNodeKind<'a>

/// Transform a single expression into a legcy AST
let rec transformExpr (doc: TextDocument) (expr: Expression) : LegacyNode =
    let kind =
        match expr with
        | ByteVecNode b ->
            b.Body
            |> Seq.choose (function
                | Constant c ->
                    match c with
                    | Some(NumVal n) -> Some((byte)n)
                    | _ -> None
                | _ -> None)
            |> List.ofSeq
            |> LegacyNodeKind.ByteVector
        | VecNode n ->
            n.Body
            |> Seq.map (transformExpr doc)
            |> List.ofSeq
            |> LegacyNodeKind.Vector
        | FormNode f -> f.Body |> Seq.map (transformExpr doc) |> List.ofSeq |> LegacyNodeKind.Form
        | ConstantNode c ->
            c.Value
            |> Option.map (
                function
                | NumVal n -> SyntaxConstant.Number n
                | StrVal s -> SyntaxConstant.Str s
                | CharVal c -> SyntaxConstant.Character (Option.defaultValue '\u0000' c)
                | BoolVal b -> SyntaxConstant.Boolean b)
            |> Option.map (LegacyNodeKind.Constant)
            |> Option.defaultValue LegacyNodeKind.Error
        | SymbolNode s ->
            s.CookedValue |> LegacyNodeKind.Ident
        | QuotedNode q ->
            match q.Inner with
            | Some(quoted) ->
                transformExpr doc quoted
                |> LegacyNodeKind.Quoted
            | _ -> LegacyNodeKind.Error

    { Kind = kind
      Location = TextDocument.rangeToLocation doc expr.SyntaxRange }

/// Transform a program into a legacy AST
let transformProgram (doc: TextDocument) (prog: Program) : LegacyNode =
    let body =
        prog.Body |> Seq.map (transformExpr doc) |> List.ofSeq |> LegacyNodeKind.Seq

    { Kind = body
      Location = TextDocument.rangeToLocation doc prog.SyntaxRange }
