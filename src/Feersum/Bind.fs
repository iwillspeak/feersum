module Bind

open System.Collections.Generic

open Diagnostics
open Syntax
open Scope

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
type StorageRef =
    | Builtin of string
    | Local of int
    | Global of string
    | Arg of int
    | Environment of int * StorageRef
    | Captured of StorageRef

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
    | Character of char
    | Number of double
    | Str of string
    | Load of StorageRef
    | Store of StorageRef * BoundExpr option
    | Application of BoundExpr * BoundExpr list
    | If of BoundExpr * BoundExpr * BoundExpr option
    | Seq of BoundExpr list
    | Lambda of BoundFormals * int * StorageRef list * int option * BoundExpr
    | Null
    | Error

/// Binder Context Type
///
/// Used to pass the state around during the bind. Holds on to scope information
/// and other transient information about the current `bind` call.
type private BinderCtx =
    { mutable Scope: Scope<StorageRef>
    ; mutable NextEnvSlot: int
    ; mutable Captures: StorageRef list
    ; mutable EnvSize: int option
    ; Diagnostics: DiagnosticBag
    ; Storage: string -> StorageRef
    ; Parent: BinderCtx option }

/// Add another element into our environment.
let private incEnvSize = function
    | None -> Some(1)
    | Some(size) -> Some(size + 1)

/// Mark the environment as used, even if nothing is stored in it.
let private markEnvUsed = function
    | None -> Some(0)
    | o -> o

/// Methods for manipulating the bind context
module private BinderCtx =

    /// Create a new binder context for the given root scope
    let createForGlobalScope scope =
        { Scope = scope |> Scope.fromMap
        ; NextEnvSlot = 0
        ; Captures = []
        ; Diagnostics = DiagnosticBag.Empty
        ; EnvSize = None
        ; Storage = StorageRef.Global
        ; Parent = None }
    
    /// Create a new binder context for a child scope
    let createWithParent parent storageFact =
        { Scope = Scope.empty
        ; NextEnvSlot = 0
        ; Captures = []
        ; Diagnostics = parent.Diagnostics
        ; EnvSize = None
        ; Storage = storageFact
        ; Parent = Some(parent) }
    
    let private getNextEnvSlot ctx =
        let next = ctx.NextEnvSlot
        ctx.NextEnvSlot <- next + 1
        next

    let rec private parentLookup ctx id =
        match ctx.Parent with
        | Some(parent) ->
            match lookupAndCapture parent id with
            | Some(outer) -> 
                match outer with
                | Captured(_)
                | Environment(_) -> 
                    ctx.Captures <- outer::ctx.Captures
                    ctx.EnvSize <- markEnvUsed ctx.EnvSize
                    Some(StorageRef.Captured(outer))
                | _ -> Some(outer)
            | None -> None
        | None -> None
    and private lookupAndCapture ctx id =
        match Scope.entry ctx.Scope id with
        | Some(entry) ->
            match entry.Value with
            | Local(_)
            | Arg(_) ->  
                let envSlot = getNextEnvSlot ctx
                let captured = StorageRef.Environment(envSlot, entry.Value)
                entry.Value <- captured
                ctx.EnvSize <- incEnvSize ctx.EnvSize
                Some(captured)
            | _ -> Some(entry.Value)
        | _ -> parentLookup ctx id
    

    /// Lookup a given ID in the binder scope
    let tryFindBinding ctx id =
        match Scope.find ctx.Scope id with
        | Some(find) -> Some(find)
        | _ -> parentLookup ctx id
    
    /// Introduce a binding for the given formal argument
    let addArgumentBinding ctx id idx =
        ctx.Scope <- Scope.insert ctx.Scope id (StorageRef.Arg idx)

    /// Add a new entry to the current scope
    let addBinding ctx id =
        let storage = ctx.Storage(id)
        ctx.Scope <- Scope.insert ctx.Scope id storage
        storage

    /// Add a new level to the scopes
    let pushScope ctx = ctx.Scope

    // Set the scope back to a stored value
    let restoreScope ctx scope =
        ctx.Scope <- scope

/// Bind a Formals List Pattern
///
/// This binds the formals list pattern as supported by `(define .. )` forms,
/// or `(lambda)` forms. Its job is to walk the list of nodes from a form and
/// return either a plain or dotted list pattern. The following
/// types of formals patterns are suppoted:
///   * `(<id>, .. )` - to bind each parameter to a unique identifier
///   * `(<id>, .. '.', <id>)` - to bind a fixed set of parameters with an
///     optional list of extra parameters.
let private bindFormalsList ctx formals =
    let f (acc: string list * bool * Option<string>) formal =
        let (formals, seenDot, afterDot) = acc
        if seenDot then
            if afterDot.IsSome then
                ctx.Diagnostics.Emit formal.Location "Only expect single ID after dot"
            match formal.Kind with
            | AstNodeKind.Ident(id) -> (formals, true, Some(id))
            | _ -> 
                ctx.Diagnostics.Emit formal.Location "Expected ID after dot"
                acc
        else
            match formal.Kind with
            | AstNodeKind.Dot -> (formals, true, None)
            | AstNodeKind.Ident(id) -> (id::formals, false, None)
            | _ ->
                ctx.Diagnostics.Emit formal.Location "Expected ID or dot in formals"
                acc
    
    let (fmls, sawDot, dotted) = List.fold f ([], false, None) formals
    let fmls = List.rev fmls
    if sawDot then
        match dotted with
        | Some(d) -> BoundFormals.DottedList(fmls, d)
        | None -> 
            ctx.Diagnostics.Emit (List.last formals).Location "Saw dot but no ID in formals"
            BoundFormals.List(fmls)
    else
        BoundFormals.List(fmls)
      
/// Bind a Lambda's Formal Arguments
/// 
/// Parses the argument list for a lambda form and returns a `BoundFormals`
/// instance describing the formal parameter pattern. The following
/// types of formals patterns are suppoted:
///   * `<id>` - to bind the whole list to the given identifier
///   * Any of the list patterns supported by `bindFormalsList`
let private bindFormals ctx formals =
    match formals.Kind with
    | AstNodeKind.Ident(id) -> BoundFormals.Simple(id)
    | AstNodeKind.Form(formals) -> bindFormalsList ctx formals
    |  _ -> 
        "Unrecognised formal parameter list. Must be an ID or list pattern"
        |> ctx.Diagnostics.Emit formals.Location
        BoundFormals.List([])

/// Bind a Syntax Node
///
/// Walks the syntax node building up a bound representation. This bound
/// node no longer has direct references to identifiers and instead
/// references storage locations.
let rec private bindInContext ctx node =
    match node.Kind with
    | AstNodeKind.Error -> failwithf "ICE: Attempt to bind an error node."
    | AstNodeKind.Number n -> BoundExpr.Number n
    | AstNodeKind.Str s -> BoundExpr.Str s
    | AstNodeKind.Boolean b -> BoundExpr.Boolean b
    | AstNodeKind.Character c -> BoundExpr.Character c
    | AstNodeKind.Dot ->
        ctx.Diagnostics.Emit node.Location "Unexpected dot"
        BoundExpr.Error
    | AstNodeKind.Seq s -> bindSequence ctx s
    | AstNodeKind.Form f -> bindForm ctx f node.Location
    | AstNodeKind.Ident id ->
        match BinderCtx.tryFindBinding ctx id with
        | Some s -> BoundExpr.Load(s)
        | None ->
            sprintf "reference to undefined symbol %s" id
            |> ctx.Diagnostics.Emit node.Location
            BoundExpr.Error

and private bindSequence ctx exprs =
    List.map (bindInContext ctx) exprs
    |> BoundExpr.Seq
and private bindApplication ctx head rest =
    BoundExpr.Application(bindInContext ctx head, List.map (bindInContext ctx) rest)
and private bindLambdaBody ctx formals body =
    let mutable nextLocal = 0
    let createLocal x =
        let local = nextLocal
        nextLocal <- nextLocal + 1
        StorageRef.Local(local)
    let lambdaCtx = BinderCtx.createWithParent ctx createLocal
    let addFormal idx id =
        BinderCtx.addArgumentBinding lambdaCtx id idx
        idx + 1

    match formals with
    | BoundFormals.Simple(id) ->
        addFormal 0 id |> ignore
    | BoundFormals.List(fmls) ->
        (List.fold addFormal 0 fmls)|> ignore
    | BoundFormals.DottedList(fmls, dotted) ->
        let nextFormal = (List.fold addFormal 0 fmls)
        addFormal nextFormal dotted |> ignore
    let boundBody = bindSequence lambdaCtx body
    BoundExpr.Lambda(formals, nextLocal, lambdaCtx.Captures, lambdaCtx.EnvSize, boundBody)
and private bindForm ctx (form: AstNode list) location =
    let illFormed formName =
        formName
        |> sprintf "Ill-formed '%s' special form"
        |> ctx.Diagnostics.Emit location
        BoundExpr.Error
    match form with
    | { Kind = AstNodeKind.Ident("if") }::body ->
        let b = bindInContext ctx
        match body with
        | [cond;ifTrue;ifFalse] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
        | [cond;ifTrue] -> BoundExpr.If((b cond), (b ifTrue), None)
        | _ -> illFormed "if"
    | { Kind = AstNodeKind.Ident("begin") }::body ->
        List.map (bindInContext ctx) body |> BoundExpr.Seq
    | { Kind = AstNodeKind.Ident("define") }::body ->
        match body with
        | [{ Kind = AstNodeKind.Ident id }] ->
            let storage = BinderCtx.addBinding ctx id
            BoundExpr.Store(storage, None)        
        | [{ Kind = AstNodeKind.Ident id };value] ->
            let value = bindInContext ctx value
            let storage = BinderCtx.addBinding ctx id
            BoundExpr.Store(storage, Some(value))
        | ({ Kind = AstNodeKind.Form ({ Kind = AstNodeKind.Ident id}::formals) })::body ->
            // Add the binding for this lambda to the scope _before_ lowering
            // the body. This makes recursive calls possible.
            BinderCtx.addBinding ctx id |> ignore
            let lambda = bindLambdaBody ctx (bindFormalsList ctx formals) body
            // Look the storage back up. This is key as the lambda, or one of
            // the lambdas nested inside it, could have captured the lambda
            // and moved it to the environment.
            let storage = (BinderCtx.tryFindBinding ctx id).Value
            BoundExpr.Store(storage, Some(lambda))
        | _ -> illFormed "define"
    | { Kind = AstNodeKind.Ident("lambda") }::body ->
        match body with
        | formals::body ->
            let boundFormals = bindFormals ctx formals
            bindLambdaBody ctx boundFormals body
        | _ -> illFormed "lambda"
    | { Kind = AstNodeKind.Ident("let") }::body ->
        match body with
        | [bindings;body] ->
            // Save the current scope so we can restore it later
            let savedScope = BinderCtx.pushScope ctx
            let mutable newScope = savedScope
            let bindLetDecl decl =
                match decl with
                | { Kind = AstNodeKind.Form(binding)} ->
                    match binding with
                    | [{ Kind = AstNodeKind.Ident id }; body] ->
                        let bound = bindInContext ctx body
                        let location = ctx.Storage id;
                        newScope <- Scope.insert newScope id location
                        BoundExpr.Store(location, Some bound)
                    | _ ->
                        ctx.Diagnostics.Emit decl.Location "Invalid binding form"
                        BoundExpr.Error
                | _ -> 
                    ctx.Diagnostics.Emit decl.Location "Invalid declaration"
                    BoundExpr.Error

            // Bind each of the definitions
            let boundDecls =
                match bindings with
                | { Kind = AstNodeKind.Form(decls); } ->
                    List.map bindLetDecl decls
                | _ ->
                    ctx.Diagnostics.Emit bindings.Location "Expected binding list"
                    []

            // Bind the body of the lambda in the new scope
            ctx.Scope <- newScope
            let boundBody = bindInContext ctx body
            // Decrement the scope depth
            BinderCtx.restoreScope ctx savedScope

            BoundExpr.Seq(List.append boundDecls [ boundBody ])
        | _ -> illFormed "let"
    | { Kind = AstNodeKind.Ident("let*") }::body ->
        failwith "Let* bindings not yet implemented"
    | { Kind = AstNodeKind.Ident("letrec") }::body ->
        failwith "Letrec bindings nto yet implemented"
    | { Kind = AstNodeKind.Ident("set!"); Location = l }::body ->
        match body with
        | [{ Kind = AstNodeKind.Ident(id) };value] ->
            let value = bindInContext ctx value
            match BinderCtx.tryFindBinding ctx id with
            | Some s -> BoundExpr.Store(s, Some(value))
            | None ->
                sprintf "Attempt to set `%s` that was not yet defined" id
                |> ctx.Diagnostics.Emit l
                BoundExpr.Error
        | _ -> illFormed "set!"
    | { Kind = AstNodeKind.Ident("quote") }::body ->
        failwith "Quote expressions not yet implemented"
    | { Kind = AstNodeKind.Ident("and") }::body ->
        failwith "And expressions not yet implemented"
    | { Kind = AstNodeKind.Ident("or") }::body ->
        failwith "Or expressions not yet implemented"
    | { Kind = AstNodeKind.Ident("cond") }::body ->
        failwith "Condition expressions not yet implemented"
    | { Kind = AstNodeKind.Ident("case") }::body ->
        failwith "Case expressions not yet implemented"
    | head::rest -> bindApplication ctx head rest
    | [] -> BoundExpr.Null

// ------------------------------ Public Binder API --------------------------

/// Create a New Root Scope
/// 
/// The root scope contains the global functions available to the program.
let createRootScope =
    [ "+"; "-"; "*"; "/"
    ; "="; "<"; ">"; "<="; ">="
    ; "display" ]
    |> Seq.map (fun s -> (s, StorageRef.Builtin(s)))
    |> Map.ofSeq

/// Bind a syntax node in a given scope
/// 
/// Walks the parse tree and computes semantic information. The result of this
/// call can be passed to the `Compile` or `Emit` API to be lowered to IL.
/// 
/// TODO: This should probably return some kind of `BoundSyntaxTree` containing
///       the tree of bound nodes and a bag of diagnostics generated during the
///       bind.
let bind scope node: BoundExpr * Diagnostic list =
    let ctx = BinderCtx.createForGlobalScope scope
    (bindInContext ctx node, ctx.Diagnostics.Take)