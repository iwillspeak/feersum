module Bind

open Syntax

/// Locals are indices into the local variable table
type Local = Local of int

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

/// Binder Context Type
///
/// Used to pass the state around during the bind. Holds on to scope information
/// and other transient information about the current `bind` call.
type private BinderCtx = { Scope: Map<string, StorageRef> }

/// Methods for manipulating the bind context
module private BinderCtx =

    /// Create a new binder context for the given root scope
    let createForScope scope =
        { Scope = scope }

/// Bind an Identifier Reference
///
/// Lookup the identifier in the given scope 
let private bindIdent ctx id =
    match Map.tryFind id ctx.Scope with
    | Some storage -> BoundExpr.Load storage
    | None -> failwithf "Reference to undefined symbol `%s`" id

/// Bind a Syntax Node
///
/// Walks the syntax node building up a bound representation. This bound
/// node no longer has direct references to identifiers and instead
/// references storage locations.
let rec private bindInContext ctx node =
    match node with
    | AstNode.Number n -> BoundExpr.Number n
    | AstNode.Str s -> BoundExpr.Str s
    | AstNode.Boolean b -> BoundExpr.Boolean b
    | AstNode.Seq s -> bindSequence ctx s
    | AstNode.Form f -> bindForm ctx f
    | AstNode.Ident id -> bindIdent ctx id
and private bindSequence ctx exprs =
    List.map (bindInContext ctx) exprs
    |> BoundExpr.Seq
and private bindApplication ctx head rest =
    BoundExpr.Application(bindInContext ctx head, List.map (bindInContext ctx) rest)
and private bindForm ctx form =
    match form with
    | AstNode.Ident("if")::body ->
        let b = bindInContext ctx
        match body with
        | [cond;ifTrue;ifFalse] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
        | [cond;ifTrue] -> BoundExpr.If((b cond), (b ifTrue), None)
        | _ -> failwith "Ill-formed 'if' special form"
    | AstNode.Ident("begin")::body ->
        List.map (bindInContext ctx) body |> BoundExpr.Seq
    | AstNode.Ident("define")::body ->
        // TODO: Storage scopes. The storage ref shouldn't be fabricated here
        //       instead we should ask the scope for the value. The root scope
        //       will generaate a global storage ref for the given name, the
        //       local scope will just bump the locals index.
        match body with
        | [AstNode.Ident id] -> BoundExpr.Definition(id, StorageRef.Global(id), None)        
        | [AstNode.Ident id;value] ->
            BoundExpr.Definition(id, StorageRef.Global(id), Some(bindInContext ctx value))
        | AstNode.Ident id::((AstNode.Form formals)::body) ->
            // TODO: lambda definitions. We need to add the formas to a new scope
            //       level and bind the body in _that_ scope. The definition body
            //       is then the bound lambda rather than `None`.
            let boudnBody = bindSequence ctx body
            BoundExpr.Definition(id, StorageRef.Global(id), None)        
        | _ -> failwith "Ill-formed 'define' special form"
    | head::rest -> bindApplication ctx head rest
    | [] -> BoundExpr.Null

// ------------------------------ Public Binder API --------------------------

/// Create a New Root Scope
/// 
/// The root scope contains the global functions available to the program.
let createRootScope =
    Map.empty

/// Bind a syntax node in a given scope
/// 
/// Walks the parse tree and computes semantic information. The result of this
/// call can be passed to the `Compile` or `Emit` API to be lowered to IL.
/// 
/// TODO: This should probably return some kind of `BoundSyntaxTree` containing
///       the tree of bound nodes and a bag of diagnostics generated during the
///       bind. Currently we use `failwith` to raise an error. Ideally more than
///       one diagnostic could be reported.
let bind scope node =
    let ctx = BinderCtx.createForScope scope
    bindInContext ctx node