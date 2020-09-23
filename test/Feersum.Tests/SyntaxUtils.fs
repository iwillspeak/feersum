module SyntaxUtils

open Syntax

type NodeNoPosition =
    | A of AstNodeKind<NodeNoPosition>
    | B of AstNodeKind<AstNode>

let rec stripPosition node =
    match node.Kind with
    | Form f -> List.map (stripPosition) f |> Form |> A
    | Seq s -> List.map (stripPosition) s |> Seq |> A
    | _ -> B(node.Kind)

let readSingle input =
    match readExpr input with
    | ({ Kind = Seq exprs }, []) -> (List.exactlyOne exprs).Kind
    | (expr, []) -> expr.Kind
    | (_, diag) -> failwithf "Expected single expression but got: %A" diag

let readMany input =
    match readExpr input with
    | (read, []) -> read  |> stripPosition
    | (_, diag) -> failwithf "Expected one or more expressions but got: %A" diag