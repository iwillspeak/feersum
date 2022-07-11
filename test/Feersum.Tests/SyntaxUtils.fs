module SyntaxUtils

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.LegacyParse
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open System.IO

/// Helpers for fabricating syntax elements
module SyntaxFactory =

    /// a fabricated location
    let dummyLocation = TextLocation.Point(TextPoint.FromParts("dummy", -1L, -1L))

    /// Build a node with a fabricated position
    let node kind =
        { Kind = kind
          Location = dummyLocation }

    /// Create a syntax node from a constant value
    let constant c = c |> AstNodeKind.Constant |> node

    /// Create a syntax node from a given number
    let number n = n |> SyntaxConstant.Number |> constant

/// Transform the StreamName in a Position
///
/// Re-writes the path in a given `Position` with a given `reWriter`.
let public sanitiseStreamNameWith reWriter (p: TextPoint) =
    TextPoint.FromParts(p.Source |> reWriter, p.Line, p.Col)

/// Path Re-Writer that returns a fixed stream name
let public fixedStreamName name =
    sanitiseStreamNameWith (function
        | _ -> name)

/// Path Re-Writer that normalises paths relative to `base`
let public basedStreamName basePath =
    sanitiseStreamNameWith (function
        | path -> Path.GetRelativePath(basePath, path))

/// Transform a location with the given Path Re-Writer
let public sanitiseLocationWith rewriter =
    function
    | Point (p) -> Point(rewriter p)
    | Span (s, e) -> Span(rewriter s, rewriter e)
    | Missing -> Missing

/// Location Re-writer that uses the `fixedStreamName` Path Re-writer
let public fixedLocaiton path =
    sanitiseLocationWith (fixedStreamName path)

/// Location Re-writer that uses the `basedStreamName` Path Re-writer
let public basedLocation basePath =
    sanitiseLocationWith (basedStreamName basePath)

/// Transform a syntax node with the given Location Re-Writer
let rec public sanitiseNodeWith rewriter (node: AstNode) =
    { AstNode.Kind = node.Kind |> sanitiseKind rewriter
      Location = node.Location |> rewriter }

and private sanitiseKind rewriter =
    function
    | Form (f) -> List.map (sanitiseNodeWith rewriter) f |> Form
    | Seq (s) -> List.map (sanitiseNodeWith rewriter) s |> Seq
    | Quoted (q) -> sanitiseNodeWith rewriter q |> Quoted
    | Vector (v) -> List.map (sanitiseNodeWith rewriter) v |> Vector
    | other -> other

/// Transofrm a diagnostics list with the given Location Re-Writer `rewriter`
let public sanitiseDiagnosticsWith rewriter (diags: Diagnostic list) =
    let sanitiseDiag d =
        { d with Diagnostic.Location = rewriter (d.Location) }

    List.map (sanitiseDiag) diags

/// Read a single expression node from the input string
let public readSingleNode input =
    match readExpr input with
    | ({ Kind = Seq exprs }, []) -> (List.exactlyOne exprs)
    | (expr, []) -> expr
    | (_, diag) -> failwithf "Expected single expression but got: %A" diag

/// Helper to read a single AST kind from the given `input`
let public readSingle input = (readSingleNode input).Kind

let public readMany input =
    match readExpr input with
    | (read, []) -> read
    | (_, diag) -> failwithf "Expected one or more expressions but got: %A" diag
