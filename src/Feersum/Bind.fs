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
    | Definition of string * StorageRef * BoundExpr option
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
let private bindIdent scope id =
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
    | AstNode.Ident("define")::body ->
        // TODO: Storage scopes. The storage ref shouldn't be fabricated here
        //       instead we should ask the scope for the value. The root scope
        //       will generaate a global storage ref for the given name, the
        //       local scope will just bump the locals index.
        match body with
        | [AstNode.Ident id] -> BoundExpr.Definition(id, StorageRef.Global(id), None)        
        | [AstNode.Ident id;value] -> BoundExpr.Definition(id, StorageRef.Global(id), Some(bind scope value))
        | AstNode.Ident id::((AstNode.Form formals)::body) ->
            // TODO: lambda definitions. We need to add the formas to a new scope
            //       level and bind the body in _that_ scope. The definition body
            //       is then the bound lambda rather than `None`.
            let boudnBody = bindSequence scope body
            BoundExpr.Definition(id, StorageRef.Global(id), None)        
        | _ -> failwith "Ill-formed 'define' special form"
    | head::rest -> bindApplication scope head rest
    | [] -> BoundExpr.Null