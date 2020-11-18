module SyntaxUtils

open Syntax
open Diagnostics
open System.IO
open FParsec

module SyntaxFactory =

    /// a fabricated location
    let dummyLocation =
        TextLocation.Point(FParsec.Position("dummy", -1L, -1L, -1L))

    /// Build a node with a fabricated position
    let node kind =
        { Kind = kind; Location = dummyLocation }

    let constant c =
        c
        |> AstNodeKind.Constant
        |> node

    let number n =
        n
        |> SyntaxConstant.Number
        |> constant

let normalisePaths (p: Position) =
    Position("dummy", p.Index, p.Line, p.Column)

let useDummyPoints l =
    SyntaxFactory.dummyLocation

let sanitiseLocation = function
    | Point(p) -> Point(normalisePaths p)
    | Span(s, e) -> Span(normalisePaths s, normalisePaths e)
    | Missing -> Missing

let rec sanitise node =
    sanitiseWith useDummyPoints node
and sanitiseWithPosition node =
    sanitiseWith sanitiseLocation node 
and sanitiseWith rewriter node =
    { Kind = node.Kind |> sanitiseKind rewriter; Location = node.Location |> rewriter }
and sanitiseKind rewriter = function
    | Form(f) -> List.map (sanitiseWith rewriter) f |> Form
    | Seq(s) -> List.map (sanitiseWith rewriter) s |> Seq
    | Quoted(q) -> sanitiseWith rewriter q |> Quoted
    | Vector(v) -> List.map (sanitiseWith rewriter) v |> Vector 
    | other -> other

let sanitiseDiagnostics (b: string) (diags: Diagnostic list) =
    let sanitisePoint (p: FParsec.Position) =
        FParsec.Position(Path.GetRelativePath(b, p.StreamName), p.Index, p.Line, p.Column)
    let santiseLocation l =
        match l with
        | Point p -> sanitisePoint p |> Point
        | Span(s, e) -> Span(sanitisePoint s, sanitisePoint e)
        | Missing -> Missing
    let sanitiseDiag d =
        match d with
        | Diagnostic(l, m) -> Diagnostic(l |> santiseLocation, m)
    List.map (sanitiseDiag) diags

let readSingleNode input =
    match readExpr input with
    | ({ Kind = Seq exprs }, []) -> (List.exactlyOne exprs)
    | (expr, []) -> expr
    | (_, diag) -> failwithf "Expected single expression but got: %A" diag

let readSingle input =
    (readSingleNode input).Kind

let readMany input =
    match readExpr input with
    | (read, []) -> read
    | (_, diag) -> failwithf "Expected one or more expressions but got: %A" diag