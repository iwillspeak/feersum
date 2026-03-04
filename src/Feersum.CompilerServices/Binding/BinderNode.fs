namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse

/// Constant or literal value in the internal binder syntax tree
type SyntaxConstant =
    | Number of double
    | Str of string
    | Boolean of bool
    | Character of char

/// Type of nodes in the internal binder syntax tree
type NodeKind<'t> =
    | Ident of string
    | Constant of SyntaxConstant
    | Dot
    | Form of 't list
    | Seq of 't list
    | Quoted of 't
    | Vector of 't list
    | ByteVector of byte list
    | Error

/// A node in the internal binder syntax tree.
type SyntaxNode =
    { Kind: NodeKind<SyntaxNode>
      Location: TextLocation }

module SyntaxNode =

    /// Transform a single expression into a SyntaxNode
    let rec ofExpression (doc: TextDocument) (expr: Expression) : SyntaxNode =
        let kind =
            match expr with
            | ByteVecNode b ->
                b.Body
                |> Seq.choose (function
                    | ConstantNode c ->
                        match c.Value with
                        | Some(NumVal n) -> Some((byte) n)
                        | _ -> None
                    | _ -> None)
                |> List.ofSeq
                |> NodeKind.ByteVector
            | VecNode n -> n.Body |> Seq.map (ofExpression doc) |> List.ofSeq |> NodeKind.Vector
            | FormNode f ->
                let body = f.Body |> Seq.map (ofExpression doc) |> List.ofSeq

                match f.DottedTail with
                | Some tail -> tail |> ofDottedTail doc |> List.append body
                | _ -> body
                |> NodeKind.Form
            | ConstantNode c ->
                c.Value
                |> Option.map (function
                    | NumVal n -> SyntaxConstant.Number n
                    | StrVal s -> SyntaxConstant.Str s
                    | CharVal c -> SyntaxConstant.Character(Option.defaultValue '\u0000' c)
                    | BoolVal b -> SyntaxConstant.Boolean b)
                |> Option.map (NodeKind.Constant)
                |> Option.defaultValue NodeKind.Error
            | SymbolNode s -> s.CookedValue |> NodeKind.Ident
            | QuotedNode q ->
                match q.Inner with
                | Some(quoted) -> ofExpression doc quoted |> NodeKind.Quoted
                | _ -> NodeKind.Error

        { Kind = kind
          Location = TextDocument.rangeToLocation doc expr.SyntaxRange }

    and private ofDottedTail doc tail =
        let dot =
            { Kind = NodeKind.Dot
              Location = TextDocument.rangeToLocation doc tail.SyntaxRange }

        let expr =
            tail.Body
            |> Option.map (ofExpression doc)
            |> Option.defaultValue
                { Kind = NodeKind.Error
                  Location = TextDocument.rangeToLocation doc tail.SyntaxRange }

        [ dot; expr ]

    /// Transform a program into a SyntaxNode
    let ofProgram (doc: TextDocument) (prog: Program) : SyntaxNode =
        let body = prog.Body |> Seq.map (ofExpression doc) |> List.ofSeq |> NodeKind.Seq

        { Kind = body
          Location = TextDocument.rangeToLocation doc prog.SyntaxRange }

    let private runParserOnString name source =
        let doc = TextDocument.fromParts name source

        let tree = readProgram doc.Path source |> ParseResult.map (ofProgram doc)

        (tree.Root, tree.Diagnostics)

    /// Read a single expression from the named input text
    let readFromString name source : (SyntaxNode * Diagnostic list) = runParserOnString name source

    /// Read an expression from source code on disk
    let parseFile path : (SyntaxNode * Diagnostic list) =
        let source = System.IO.File.ReadAllText(path)
        runParserOnString path source
