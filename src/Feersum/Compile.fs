module Compile

open Syntax

type Symbol = int32

/// Bound Expression Type
///
/// Bound expressions represent the syntax of a program with all identifier
/// references resolved to the correct storage.
type BoundExpr =
    | Boolean of bool
    | Number of int64
    | Str of string
    | Form of BoundExpr list
    | Nil

let bindIdent scope id =
    failwithf "Unimplemented"

let rec bind scope node =
    match node with
    | AstNode.Number n -> BoundExpr.Number n
    | AstNode.Str s -> BoundExpr.Str s
    | AstNode.Boolean b -> BoundExpr.Boolean b
    | AstNode.Form f -> bindForm scope f
    | AstNode.Ident id -> bindIdent scope id
    | AstNode.Seq s -> bindSequence scope s
and bindSequence scope exprs =
    List.map (bind scope) exprs |> List.tryLast |> Option.defaultValue BoundExpr.Nil
and bindForm scope form =
    let boundForm = List.map (fun f -> bind scope f) form
    BoundExpr.Form boundForm

/// Compile a single AST node into an assembly
///
/// The plan for this is we make multiple passes over the syntax tree. First
/// pass will be to `bind` theh tree. Resulting in a `BoundExpr`. This will
/// attach any type information that _can_ be computed to each node, and
/// resolve variable references to the symbols that they refer to.
/// 
/// Once the expression is bound we will then `lower` the expression to
/// flatten it out into a list of blocks. Once we have all the blocks
/// they can be written out to an `Assembly` with `emit`.
let compile (node: AstNode) =
    let bound = bind Map.empty node
    bound

/// Read a File and Compile
/// 
/// Takes the `path` to an input to read and compile.
let compileFile (path) =
    match parseFile path with
    | Ok ast -> compile ast
    | Error e -> failwithf "error: %s" e