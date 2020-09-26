module SyntaxUtils

open Syntax

module SyntaxFactory =

    /// a fabricated location
    let dummyLocation =
        TextLocation.Point(FParsec.Position("dummy", -1L, -1L, -1L))

    /// Build a node with a fabricated position
    let node kind =
        { Kind = kind; Location = dummyLocation }

open SyntaxFactory

let rec sanitise node =
    { Kind = node.Kind |> sanitiseKind; Location = dummyLocation }
and sanitiseKind = function
    | Form(f) -> List.map (sanitise) f |> Form
    | Seq(s) -> List.map (sanitise) s |> Seq
    | other -> other

let readSingle input =
    match readExpr input with
    | ({ Kind = Seq exprs }, []) -> (List.exactlyOne exprs).Kind
    | (expr, []) -> expr.Kind
    | (_, diag) -> failwithf "Expected single expression but got: %A" diag

let readMany input =
    match readExpr input with
    | (read, []) -> read
    | (_, diag) -> failwithf "Expected one or more expressions but got: %A" diag