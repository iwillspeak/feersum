module Bind

open Syntax

/// Locals are indices into the local variable table
type Local = int

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
type StorageRef =
    | Local of Local
    | Global of string

/// Bound Expression Type
///
/// Bound expressions represent the syntax of a program with all identifier
/// references resolved to the correct storage.
type BoundExpr =
    | Boolean of bool
    | Number of double
    | Str of string
    | Load of StorageRef
    | Application of BoundExpr * BoundExpr list
    | If of BoundExpr * BoundExpr * BoundExpr option
    | Seq of BoundExpr list
    | Null

/// Create a New Root Scope
/// 
/// The root scope contains the global functions available to the program.
let createRootScope =
    Map.empty

/// Bind an Identifier Reference
///
/// Lookup the identifier in the given scope 
let bindIdent scope id =
    match Map.tryFind id scope with
    | Some storage -> BoundExpr.Load storage
    | None -> failwithf "Reference to undefined symbol `%s`" id

/// Bind a Syntax Node
///
/// Walks the syntax node building up a bound representation. This bound
/// node no longer has direct references to identifiers and instead
/// references storage locations.
let rec bind scope node =
    match node with
    | AstNode.Number n -> BoundExpr.Number n
    | AstNode.Str s -> BoundExpr.Str s
    | AstNode.Boolean b -> BoundExpr.Boolean b
    | AstNode.Seq s -> bindSequence scope s
    | AstNode.Form f -> bindForm scope f
    | AstNode.Ident id -> bindIdent scope id
and bindSequence scope exprs =
    List.map (bind scope) exprs
    |> BoundExpr.Seq
and bindApplication scope head rest =
    BoundExpr.Application(bind scope head, List.map (bind scope) rest)
and bindForm scope form =
    match form with
    | AstNode.Ident("if")::body ->
        let b = bind scope
        match body with
        | [cond;ifTrue;ifFalse] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
        | [cond;ifTrue] -> BoundExpr.If((b cond), (b ifTrue), None)
        | _ -> failwith "Ill-formed 'if' special form"
    | AstNode.Ident("begin")::body ->
        List.map (bind scope) body |> BoundExpr.Seq
    | head::rest -> bindApplication scope head rest
    | [] -> BoundExpr.Null