module Bind

open System.Collections.Generic

open Diagnostics
open Syntax
open Macros
open Scope

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
type StorageRef =
    | Macro of Macro
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

type BoundLiteral =
    | Boolean of bool
    | Character of char
    | Number of double
    | Str of string
    | Vector of AstNode list
    | ByteVector of byte list
    | Null

with
    static member FromConstant = function
        | SyntaxConstant.Number n -> BoundLiteral.Number n
        | SyntaxConstant.Character c -> BoundLiteral.Character c
        | SyntaxConstant.Boolean b -> BoundLiteral.Boolean b
        | SyntaxConstant.Str s -> BoundLiteral.Str s
    
/// Bound Expression Type
///
/// Bound expressions represent the syntax of a program with all identifier
/// references resolved to the correct storage.
type BoundExpr =
    | Literal of BoundLiteral
    | Quoted of AstNode
    | SequencePoint of BoundExpr * TextLocation
    | Load of StorageRef
    | Store of StorageRef * BoundExpr option
    | Application of BoundExpr * BoundExpr list
    | If of BoundExpr * BoundExpr * BoundExpr option
    | Seq of BoundExpr list
    | Lambda of BoundFormals * int * StorageRef list * StorageRef list option * BoundExpr
    | Error

/// Root type returned by the binder.
type BoundSyntaxTree = { Root: BoundExpr
                       ; LocalsCount: int
                       ; EnvMappings: StorageRef list option
                       ; Diagnostics: Diagnostic list }

/// Binder Context Type
///
/// Used to pass the state around during the bind. Holds on to scope information
/// and other transient information about the current `bind` call.
type private BinderCtx =
    { mutable Scope: Scope<StorageRef>
    ; mutable OuterScopes: Scope<StorageRef> list
    ; mutable LocalCount: int
    ; mutable Captures: StorageRef list
    ; mutable HasDynamicEnv: bool
    ; Diagnostics: DiagnosticBag
    ; Parent: BinderCtx option }

/// Add another element into our environment.
let private incEnvSize = function
    | None -> Some(1)
    | Some(size) -> Some(size + 1)

/// Methods for manipulating the bind context
module private BinderCtx =

    /// Create a new binder context for the given root scope
    let createForGlobalScope scope =
        { Scope = scope |> Scope.fromMap
        ; OuterScopes = []
        ; LocalCount = 0
        ; Captures = []
        ; Diagnostics = DiagnosticBag.Empty
        ; HasDynamicEnv = false
        ; Parent = None }
    
    /// Create a new binder context for a child scope
    let createWithParent parent =
        { Scope = Scope.empty
        ; OuterScopes = []
        ; LocalCount = 0
        ; Captures = []
        ; Diagnostics = parent.Diagnostics
        ; HasDynamicEnv = false
        ; Parent = Some(parent) }

    let private getNextLocal ctx =
        let next = ctx.LocalCount
        ctx.LocalCount <- next + 1
        next

    /// Lookup a given ID in the binder scope
    let rec tryFindBinding ctx id =
        Scope.find ctx.Scope id
        |> Option.orElseWith (fun () -> parentLookup ctx id)
    and private parentLookup ctx id =
        match ctx.Parent with
        | Some(parent) ->
            match tryFindBinding parent id with
            | Some(outer) -> 
                match outer with
                | Captured(_)
                | Arg(_)
                | Local(_)
                | Environment(_) -> 
                    ctx.Captures <- outer::ctx.Captures
                    ctx.HasDynamicEnv <- true
                    Some(StorageRef.Captured(outer))
                | _ -> Some(outer)
            | None -> None
        | None -> None

    
    /// Introduce a binding for the given formal argument
    let addArgumentBinding ctx id idx =
        ctx.Scope <- Scope.insert ctx.Scope id (StorageRef.Arg idx)

    /// Adds a macro definition to the current scope.
    let addMacro ctx id macro =
        ctx.Scope <- Scope.insert ctx.Scope id (StorageRef.Macro macro)

    /// Add a new entry to the current scope
    let addBinding ctx id =
        let storage =
            if ctx.Parent.IsNone && ctx.OuterScopes.IsEmpty then
                StorageRef.Global(id)
            else
                StorageRef.Local(getNextLocal ctx)
        ctx.Scope <- Scope.insert ctx.Scope id storage
        storage

    /// Add a new level to the scopes
    let pushScope ctx =
        ctx.OuterScopes <- ctx.Scope::ctx.OuterScopes

    // Set the scope back to a stored value
    let popScope ctx =
        match ctx.OuterScopes with
        | scope::scopes ->
            ctx.Scope <- scope
            ctx.OuterScopes <- scopes
        | [] ->
            failwith "ICE: Unbalanced scope pop"

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

/// Recognise a given form as a list of let binding specifications. This expects
/// the `node` to be a form `()`, or `((id init) ...)`.
let private parseBindingSpecs ctx node =
    // Bind each of the definitions
    let parseBindingSpec decl bindings =
        match decl with
        | { Kind = AstNodeKind.Form(binding)} ->
            match binding with
            | [{ Kind = AstNodeKind.Ident id }; body] ->
                (id, body)::bindings
            | _ ->
                ctx.Diagnostics.Emit decl.Location "Invalid binding form"
                bindings
        | _ -> 
            ctx.Diagnostics.Emit decl.Location "Expeted a binding form"
            bindings

    match node with
    | { Kind = AstNodeKind.Form(decls); } ->
        List.foldBack (parseBindingSpec) decls []
    | _ ->
        ctx.Diagnostics.Emit node.Location "Expected binding list"
        []

/// Emit a diagnostic for an ill-formed special form
let private illFormedInCtx ctx location formName =
    formName
    |> sprintf "Ill-formed '%s' special form"
    |> ctx.Diagnostics.Emit location
    BoundExpr.Error

/// Bind a Syntax Node
///
/// Walks the syntax node building up a bound representation. This bound
/// node no longer has direct references to identifiers and instead
/// references storage locations.
let rec private bindInContext ctx node =
    match node.Kind with
    | AstNodeKind.Error -> failwithf "ICE: Attempt to bind an error node."
    | AstNodeKind.Constant c ->
        BoundLiteral.FromConstant c
        |> BoundExpr.Literal
    | AstNodeKind.Vector v -> BoundExpr.Literal(BoundLiteral.Vector v)
    | AstNodeKind.ByteVector bv -> BoundExpr.Literal(BoundLiteral.ByteVector bv)
    | AstNodeKind.Dot ->
        ctx.Diagnostics.Emit node.Location "Unexpected dot"
        BoundExpr.Error
    | AstNodeKind.Seq s -> bindSequence ctx s
    | AstNodeKind.Form f -> bindForm ctx f node
    | AstNodeKind.Ident id ->
        match BinderCtx.tryFindBinding ctx id with
        | Some s -> BoundExpr.Load(s)
        | None ->
            sprintf "reference to undefined symbol %s" id
            |> ctx.Diagnostics.Emit node.Location
            BoundExpr.Error
    | AstNodeKind.Quoted q -> bindQuoted ctx q

and private bindQuoted ctx quoted =
    BoundExpr.Quoted quoted

and private bindWithSequencePoint ctx expr =
    let inner = bindInContext ctx expr
    match inner with
    | BoundExpr.If _ 
    | BoundExpr.Seq _ -> inner
    | _ -> BoundExpr.SequencePoint(inner, expr.Location)

and private bindSequence ctx exprs =
    List.map (bindWithSequencePoint ctx) exprs
    |> BoundExpr.Seq

and private bindApplication ctx head rest node =
    let applicant = bindInContext ctx head
    match applicant with
    | BoundExpr.Load(StorageRef.Macro m) ->
        match macroApply m node with
        | Ok ast -> bindInContext ctx ast
        | Result.Error diag ->
            ctx.Diagnostics.Add diag
            BoundExpr.Error
    | _ -> BoundExpr.Application(applicant, List.map (bindInContext ctx) rest)

and private bindLambdaBody ctx formals body =
    let lambdaCtx = BinderCtx.createWithParent ctx
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
    let env =
        if lambdaCtx.HasDynamicEnv then
            Some([])
        else
            None
    BoundExpr.Lambda(formals, lambdaCtx.LocalCount, lambdaCtx.Captures, env, boundBody)
    
and private bindLet ctx name body location declBinder =
    match body with
    | bindings::body ->
        let bindingSpecs = parseBindingSpecs ctx bindings
        // Save the current scope so we can restore it later
        let savedScope = BinderCtx.pushScope ctx
        // Bind the declarations
        let decls = declBinder bindingSpecs
        // Bind the body of the lambda in the new scope
        let boundBody = List.map (bindInContext ctx) body
        // Decrement the scope depth
        BinderCtx.popScope ctx
        BoundExpr.Seq(List.append decls boundBody)
    | _ -> illFormedInCtx ctx location name

and private bindForm ctx (form: AstNode list) node =
    let illFormed formName = illFormedInCtx ctx node.Location formName
    match form with
    | { Kind = AstNodeKind.Ident("if") }::body ->
        let b = bindWithSequencePoint ctx
        match body with
        | [cond;ifTrue;ifFalse] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
        | [cond;ifTrue] -> BoundExpr.If((b cond), (b ifTrue), None)
        | _ -> illFormed "if"
    | { Kind = AstNodeKind.Ident("begin") }::body ->
        bindSequence ctx body
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
        bindLet ctx "let" body node.Location (fun bindingSpecs  ->
            
            // Bind the body of each binding spec first
            let decls =
                bindingSpecs
                |> List.map (fun (id, body) ->
                    (id, bindInContext ctx body))
                
            // Once the bodies are bound, we can create assignments and
            // initialise the environment
            let boundDecls =
                decls
                |> List.map (fun (id, body) ->
                    let storage = BinderCtx.addBinding ctx id
                    BoundExpr.Store(storage, Some(body)))

            boundDecls)
    | { Kind = AstNodeKind.Ident("let*") }::body ->
        bindLet ctx "let*" body node.Location (
            // let* binds each spec sequentially
            List.map (fun (id, body) ->
                    let body = bindInContext ctx body
                    let storage = BinderCtx.addBinding ctx id
                    BoundExpr.Store(storage, Some(body))))
    | { Kind = AstNodeKind.Ident("letrec") }::body ->
        bindLet ctx "letrec" body node.Location (fun bindingSpecs  ->

            // Get storage for each of the idents first into scope.
            let boundIdents =
                bindingSpecs
                |> List.map (fun (id, body) ->
                    ((BinderCtx.addBinding ctx id), body))

            // Now all the IDs are in scope, bind the initialisers
            let boundDecls =
                boundIdents
                |> List.map (fun (storage, body) ->
                    BoundExpr.Store(storage, Some(bindInContext ctx body)))

            boundDecls)
    | { Kind = AstNodeKind.Ident("let-syntax")}::body ->
        bindLet ctx "let-syntax" body node.Location (fun bindingSpecs ->

            Seq.iter (fun (id, syntaxRules) ->
                match Macros.parseSyntaxRules id syntaxRules with
                | Ok(macro) ->
                    BinderCtx.addMacro ctx id macro
                | Result.Error e ->
                    ctx.Diagnostics.Add e) bindingSpecs
            [])
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
        match body with
        | [ item ] -> bindQuoted ctx item
        | _ -> illFormed "quote"
    | { Kind = AstNodeKind.Ident("define-syntax")}::body ->
        match body with
        | [{ Kind = AstNodeKind.Ident(id) } as idSyn; syntaxRules] ->
            match Macros.parseSyntaxRules id syntaxRules with
            | Ok(macro) ->
                BinderCtx.addMacro ctx id macro
                BoundExpr.Quoted idSyn
            | Result.Error e ->
                ctx.Diagnostics.Add e
                BoundExpr.Error
        | _ -> illFormed "define-syntax"
    | { Kind = AstNodeKind.Ident("cond") }::body ->
        failwith "Condition expressions not yet implemented"
    | { Kind = AstNodeKind.Ident("case") }::body ->
        failwith "Case expressions not yet implemented"
    | head::rest -> 
        bindApplication ctx head rest node
    | [] -> BoundExpr.Literal BoundLiteral.Null

// ------------------------------ Public Binder API --------------------------

/// Create a New Root Scope
/// 
/// The root scope contains the global functions available to the program.
let createRootScope =
    let builtinProcs =
        Builtins.coreProcNames
        |> Seq.map (fun s -> (s, StorageRef.Builtin(s)))
    let builtinMacros =
        Builtins.coreMacros
        |> Seq.map (fun m -> (m.Name, StorageRef.Macro(m)))
    Seq.append builtinProcs builtinMacros
    |> Map.ofSeq

/// Bind a syntax node in a given scope
/// 
/// Walks the parse tree and computes semantic information. The result of this
/// call can be passed to the `Compile` or `Emit` API to be lowered to IL.
let bind scope node: BoundSyntaxTree =
    let ctx = BinderCtx.createForGlobalScope scope
    let bound = bindInContext ctx node
    { Root = bound
    ; LocalsCount = ctx.LocalCount
    ; EnvMappings = None
    ; Diagnostics = ctx.Diagnostics.Take }
