module Bind

open Syntax
open System.Collections.Generic

/// Locals are indices into the local variable table
type Local = Local of int

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
type StorageRef =
    | Local of Local
    | Global of string

/// Collection of Bound Formal Parameters
/// 
/// Different types of formal parameters accepted by lambda definitions.
type BoundFormals =
    | Simple of string
    | List of string list
    | DottedList of string list * string

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
    | Lambda of BoundFormals * BoundExpr
    | Null

/// Binder Context Type
///
/// Used to pass the state around during the bind. Holds on to scope information
/// and other transient information about the current `bind` call.
type private BinderCtx =
    { Scope: IDictionary<string, StorageRef>
    ; Parent: BinderCtx option }

/// Methods for manipulating the bind context
module private BinderCtx =

    /// Create a new binder context for the given root scope
    let createForScope scope =
        { Scope = new Dictionary<string, StorageRef>(Map.toSeq scope |> dict)
        ; Parent = None }
    
    /// Create a new binder context for a child scope
    let createWithParent parent =
        { Scope = new Dictionary<string, StorageRef>()
        ; Parent = Some(parent) }

    /// Lookup a given ID in the binder scope
    let tryFindBinding ctx id =
        // TODO: Handle more than one scope.
        match ctx.Scope.TryGetValue(id) with
        | (true, value) -> Some(value)
        | _ -> None

    /// Add a new entry to the current scope
    let addBinding ctx id =
        // TODO: Storage scopes. The storage ref shouldn't be fabricated here
        //       instead we should ask the scope for the value. The root scope
        //       will generaate a global storage ref for the given name, the
        //       local scope will just bump the locals index.
        let storage = StorageRef.Global(id)
        ctx.Scope.[id] <- storage
        storage

/// Bind an Identifier Reference
///
/// Lookup the identifier in the given scope 
let private bindIdent ctx id =
    match BinderCtx.tryFindBinding ctx id with
    | Some storage -> BoundExpr.Load storage
    | None -> failwithf "Reference to undefined symbol `%s`" id


/// Bind a Lambda's Formal Arguments
/// 
/// Parses the argument list for a lambda form and returns the
/// list of boudn formal arguments.
let private bindFormals formals =
    let bindFormalsList (acc: string list * bool * Option<string>) formal =
        let (formals, seenDot, afterDot) = acc
        if seenDot then
            if afterDot.IsSome then
                failwith "Only expect single ID after dot"
            match formal with
            | AstNode.Ident(id) -> (formals, true, Some(id))
            | _ -> failwith "Expected ID after dot"
        else
            match formal with
            | AstNode.Dot -> (formals, true, None)
            | AstNode.Ident(id) -> (id::formals, false, None)
            | _ -> failwith "Expected ID or dot in formals"
      
    match formals with
    | AstNode.Ident(id) -> BoundFormals.Simple(id)
    | AstNode.Form(formals) ->
        let (fmls, sawDot, dotted) = List.fold bindFormalsList ([], false, None) formals
        if sawDot then
            match dotted with
            | Some(d) -> BoundFormals.DottedList(fmls, d)
            | None -> failwith "Saw dot but no ID in formals"
        else
            BoundFormals.List(fmls)
    |  _ -> failwith "Unrecognised formal parameter list. Must be an ID or list pattern"

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
    | AstNode.Dot -> failwith "Unexpected dot"
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
        match body with
        | [AstNode.Ident id] ->
            let storage = BinderCtx.addBinding ctx id
            BoundExpr.Definition(id, storage, None)        
        | [AstNode.Ident id;value] ->
            let value = bindInContext ctx value
            let storage = BinderCtx.addBinding ctx id
            BoundExpr.Definition(id, storage, Some(value))
        | (AstNode.Form (AstNode.Ident id::formals))::body ->
            // Add the binding for this lambda to the scope _before_ lowering
            // the body. This makes recursive calls possible.
            let storage = BinderCtx.addBinding ctx id
            // TODO: lambda definitions. We need to add the formas to a new scope
            //       level and bind the body in _that_ scope. The definition body
            //       is then the bound lambda rather than `None`.
            let boundBody = bindSequence ctx body
            BoundExpr.Definition(id, storage, None)        
        | _ -> failwith "Ill-formed 'define' special form"
    | AstNode.Ident("lambda")::body ->
        match body with
        | [formals;body] ->
            let boundFormals = bindFormals formals
            let lambdaCtx = BinderCtx.createWithParent ctx
            match boundFormals with
            | BoundFormals.Simple(id) ->
                BinderCtx.addBinding lambdaCtx id |> ignore
            | BoundFormals.List(fmls) ->
                for f in fmls do
                    BinderCtx.addBinding lambdaCtx f |> ignore
            | BoundFormals.DottedList(fmls, dotted) ->
                for f in fmls do
                    BinderCtx.addBinding lambdaCtx f |> ignore
                BinderCtx.addBinding lambdaCtx dotted |> ignore
            let boundBody = bindInContext lambdaCtx body
            BoundExpr.Lambda(boundFormals, boundBody)
        | _ -> failwith "Ill-formed 'lambda' special form"
    | AstNode.Ident("let")::body ->
        failwith "Let bindings not yet implemented"
    | AstNode.Ident("let*")::body ->
        failwith "Let* bindings not yet implemented"
    | AstNode.Ident("letrec")::body ->
        failwith "Letrec bindings nto yet implemented"
    | AstNode.Ident("set!")::body ->
        failwith "Assignment expressions not yet implemented"
    | AstNode.Ident("quote")::body ->
        failwith "Quote expressions not yet implemented"
    | AstNode.Ident("and")::body ->
        failwith "And expressions not yet implemented"
    | AstNode.Ident("or")::body ->
        failwith "Or expressions not yet implemented"
    | AstNode.Ident("cond")::body ->
        failwith "Condition expressions not yet implemented"
    | AstNode.Ident("case")::body ->
        failwith "Case expressions not yet implemented"
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