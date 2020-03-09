module Compile

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
    | Number of int64
    | Str of string
    | Load of StorageRef
    | Application of BoundExpr * BoundExpr list
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
    | None -> failwithf "Reference to undefined symbol"

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
    | head::rest -> bindApplication scope head rest
    | [] -> BoundExpr.Null

/// Lower a Bound Expression to .NET
/// 
/// Creates an assembly and writes out the .NET interpretation of the
/// given bound tree.
let lower bound =
    match bound with
    | _ -> ()

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
    let scope = createRootScope
    bind scope node
    |> lower

/// Read a File and Compile
/// 
/// Takes the `path` to an input to read and compile.
let compileFile (path) =
    match parseFile path with
    | Ok ast -> compile ast
    | Error e -> failwithf "error: %s" e