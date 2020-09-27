module SyntaxUtils

open Syntax
open Diagnostics

module SyntaxFactory =

    /// a fabricated location
    let dummyLocation =
        TextLocation.Point(FParsec.Position("dummy", -1L, -1L, -1L))

    /// Build a node with a fabricated position
    let node kind =
        { Kind = kind; Location = dummyLocation }

open SyntaxFactory
open System.IO

let rec sanitise node =
    { Kind = node.Kind |> sanitiseKind; Location = dummyLocation }
and sanitiseKind = function
    | Form(f) -> List.map (sanitise) f |> Form
    | Seq(s) -> List.map (sanitise) s |> Seq
    | other -> other

let sanitiseDiagnostics (b: string) (diags: Diagnostic list) =
    let sanitisePoint (p: FParsec.Position) =
        FParsec.Position(Path.GetRelativePath(b, p.StreamName), p.Index, p.Line, p.Column)
    let santiiseLocation l =
        match l with
        | Point p -> sanitisePoint p |> Point
        | Span(s, e) -> Span(sanitisePoint s, sanitisePoint e)
    let sanitiseDiag d =
        match d with
        | Diagnostic(l, m) -> Diagnostic(l |> santiiseLocation, m)
    List.map (sanitiseDiag) diags

let readSingle input =
    match readExpr input with
    | ({ Kind = Seq exprs }, []) -> (List.exactlyOne exprs).Kind
    | (expr, []) -> expr.Kind
    | (_, diag) -> failwithf "Expected single expression but got: %A" diag

let readMany input =
    match readExpr input with
    | (read, []) -> read
    | (_, diag) -> failwithf "Expected one or more expressions but got: %A" diag