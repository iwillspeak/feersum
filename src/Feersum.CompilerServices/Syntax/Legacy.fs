namespace Feersum.CompilerServices.Syntax

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text

/// Constant or literal value in the syntax tree
type LegacySyntaxConstant =
    | Number of double
    | Str of string
    | Boolean of bool
    | Character of char

/// Type of nodes in our syntax tree
type LegacyNodeKind<'t> =
    | Ident of string
    | Constant of LegacySyntaxConstant
    | Dot
    | Form of 't list
    | Seq of 't list
    | Quoted of 't
    | Vector of 't list
    | ByteVector of byte list
    | Error

/// A node in our syntax tree.
type LegacyNode =
    { Kind: LegacyNodeKind<LegacyNode>
      Location: TextLocation }

module SyntaxShim =

    open Feersum.CompilerServices.Syntax.Tree

    /// Transform a single expression into a legcy AST
    let rec transformExpr (doc: TextDocument) (expr: Expression) : LegacyNode =
        let kind =
            match expr with
            | ByteVecNode b ->
                b.Body
                |> Seq.choose (function
                    | Constant c ->
                        match c with
                        | Some(NumVal n) -> Some((byte) n)
                        | _ -> None
                    | _ -> None)
                |> List.ofSeq
                |> LegacyNodeKind.ByteVector
            | VecNode n -> n.Body |> Seq.map (transformExpr doc) |> List.ofSeq |> LegacyNodeKind.Vector
            | FormNode f ->
                let body = f.Body |> Seq.map (transformExpr doc) |> List.ofSeq

                match f.DottedTail with
                | Some tail -> tail |> transformDottedTail doc |> List.append body
                | _ -> body
                |> LegacyNodeKind.Form
            | ConstantNode c ->
                c.Value
                |> Option.map (function
                    | NumVal n -> LegacySyntaxConstant.Number n
                    | StrVal s -> LegacySyntaxConstant.Str s
                    | CharVal c -> LegacySyntaxConstant.Character(Option.defaultValue '\u0000' c)
                    | BoolVal b -> LegacySyntaxConstant.Boolean b)
                |> Option.map (LegacyNodeKind.Constant)
                |> Option.defaultValue LegacyNodeKind.Error
            | SymbolNode s -> s.CookedValue |> LegacyNodeKind.Ident
            | QuotedNode q ->
                match q.Inner with
                | Some(quoted) -> transformExpr doc quoted |> LegacyNodeKind.Quoted
                | _ -> LegacyNodeKind.Error

        { Kind = kind
          Location = TextDocument.rangeToLocation doc expr.SyntaxRange }

    and private transformDottedTail doc tail =
        let dot =
            { Kind = LegacyNodeKind.Dot
              Location = TextDocument.rangeToLocation doc tail.SyntaxRange }

        let expr =
            tail.Body
            |> Option.map (transformExpr doc)
            |> Option.defaultValue
                { Kind = LegacyNodeKind.Error
                  Location = TextDocument.rangeToLocation doc tail.SyntaxRange }

        [ dot; expr ]

    /// Transform a program into a legacy AST
    let transformProgram (doc: TextDocument) (prog: Program) : LegacyNode =
        let body =
            prog.Body |> Seq.map (transformExpr doc) |> List.ofSeq |> LegacyNodeKind.Seq

        { Kind = body
          Location = TextDocument.rangeToLocation doc prog.SyntaxRange }

module LegacyParse =

    open Feersum.CompilerServices.Syntax.Parse
    open System.IO

    let runParserOnString name source =
        let doc = TextDocument.fromParts name source

        let tree =
            readProgram doc.Path source |> ParseResult.map (SyntaxShim.transformProgram doc)

        (tree.Root, tree.Diagnostics)


    /// Read a single expression from the named input text
    let readExpr1 name line : (LegacyNode * Diagnostic list) = runParserOnString name line

    /// Read a single expression from the input text
    let readExpr = readExpr1 "repl"

    /// Read an expression from source code on disk
    let parseFile path : (LegacyNode * Diagnostic list) =
        let source = File.ReadAllText(path)
        runParserOnString path source
