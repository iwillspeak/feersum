namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices
open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Utils

module private BinderDiagnostics =

    let patternBindError =
        DiagnosticKind.Create DiagnosticLevel.Error 30 "Invalid or incomplete pattern syntax"

    let missingExport =
        DiagnosticKind.Create DiagnosticLevel.Warning 31 "Missing export"

    let illFormedSpecialForm =
        DiagnosticKind.Create DiagnosticLevel.Error 32 "Ill-formed special form"

    let invalidParameterPattern =
        DiagnosticKind.Create DiagnosticLevel.Error 33 "Invalid formal parameter pattern"

    let letBindError =
        DiagnosticKind.Create DiagnosticLevel.Error 34 "Invalid let binding"

    let undefinedSymbol =
        DiagnosticKind.Create DiagnosticLevel.Error 35 "Reference to undefined symbol"

    let invalidImport =
        DiagnosticKind.Create DiagnosticLevel.Error 36 "Invalid import declaration"

    let uninitialisedVariale =
        DiagnosticKind.Create DiagnosticLevel.Error 37 "Use of uninitialised variable"

    let malformedDatum =
        DiagnosticKind.Create DiagnosticLevel.Error 38 "Invalid datum value"

/// Global binding Type
///
/// For code we generate globals are stored in static fields. For some imported
/// references globals may be stored in static fields instead.
type GlobalType =
    | Method of string
    | Field of string

/// Storage Reference
///
/// Reference to a given storage location. Used to express reads and writes
/// of values to storage locations.
type StorageRef =
    | Macro of Macro
    | Local of int
    | Global of string * GlobalType
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

/// Literal or Constant Value
type BoundLiteral =
    | Boolean of bool
    | Character of char
    | Number of double
    | Str of string
    | Vector of BoundDatum list
    | ByteVector of byte list
    | Null

    static member FromConstant =
        function
        | LegacySyntaxConstant.Number n -> BoundLiteral.Number n
        | LegacySyntaxConstant.Character c -> BoundLiteral.Character c
        | LegacySyntaxConstant.Boolean b -> BoundLiteral.Boolean b
        | LegacySyntaxConstant.Str s -> BoundLiteral.Str s

/// Bound Datum Element
///
/// Represents a form or atom when used as a data element rather than as a
/// program element. Used to hold the contents of quoted expressions, as well
/// as the lements winthin a vector literal.
and BoundDatum =
    | Compound of BoundDatum list
    | SelfEval of BoundLiteral
    | Ident of string
    | Quoted of BoundDatum

/// Bound Expression Type
///
/// Bound expressions represent the syntax of a program with all identifier
/// references resolved to the correct storage.
type BoundExpr =
    | Literal of BoundLiteral
    | Quoted of BoundDatum
    | SequencePoint of BoundExpr * TextLocation
    | Load of StorageRef
    | Store of StorageRef * BoundExpr option
    | Application of BoundExpr * BoundExpr list
    | If of BoundExpr * BoundExpr * BoundExpr option
    | Seq of BoundExpr list
    | Lambda of BoundFormals * BoundBody
    | Library of string list * string * (string * StorageRef) list * BoundBody
    | Import of string
    | Nop
    | Error

and BoundBody =
    { Body: BoundExpr
      Locals: int
      Captures: StorageRef list
      EnvMappings: StorageRef list option }

/// Root type returned by the binder.
type BoundSyntaxTree =
    { Root: BoundBody
      MangledName: string
      Diagnostics: Diagnostic list }

/// Binder Context Type
///
/// Used to pass the state around during the bind. Holds on to scope information
/// and other transient information about the current `bind` call.
type private BinderCtx =
    { mutable Scope: Scope<StorageRef>
      mutable OuterScopes: Scope<StorageRef> list
      mutable Libraries: LibrarySignature<StorageRef> list
      mutable LocalCount: int
      mutable Captures: StorageRef list
      mutable HasDynamicEnv: bool
      MangledName: string
      Diagnostics: DiagnosticBag
      Parent: BinderCtx option }


/// Methods for manipulating the bind context
module private BinderCtx =

    /// Add another element into our environment.
    let private incEnvSize =
        function
        | None -> Some(1)
        | Some(size) -> Some(size + 1)

    /// Transofmr the name to make it safe for .NET.
    let mangleName name =
        let mangleNamePart = id

        name |> Seq.map (mangleNamePart) |> String.concat "::"

    /// Create a new binder context for the given root scope
    let createForGlobalScope scope libs name =
        { Scope = scope |> Scope.fromMap
          OuterScopes = []
          Libraries = libs
          MangledName = name |> mangleName
          LocalCount = 0
          Captures = []
          Diagnostics = DiagnosticBag.Empty
          HasDynamicEnv = false
          Parent = None }

    /// Create a new binder context for a child scope
    let createWithParent parent =
        { Scope = Scope.empty
          OuterScopes = []
          Libraries = []
          MangledName = parent.MangledName
          LocalCount = 0
          Captures = []
          Diagnostics = parent.Diagnostics
          HasDynamicEnv = false
          Parent = Some(parent) }

    let private getNextLocal ctx =
        let next = ctx.LocalCount
        ctx.LocalCount <- next + 1
        next

    /// Lookup a given ID in the binder scope
    let rec tryFindBinding ctx id =
        Scope.find ctx.Scope id |> Option.orElseWith (fun () -> parentLookup ctx id)

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
                    ctx.Captures <- outer :: ctx.Captures
                    ctx.HasDynamicEnv <- true
                    Some(StorageRef.Captured(outer))
                | _ -> Some(outer)
            | None -> None
        | None -> None

    /// Insert an element directly into the current scope.
    let scopeInsert ctx id storage =
        ctx.Scope <- Scope.insert ctx.Scope id storage

    /// Introduce a binding for the given formal argument.
    let addArgumentBinding ctx id idx = scopeInsert ctx id (StorageRef.Arg idx)

    /// Adds a macro definition to the current scope.
    let addMacro ctx id macro =
        scopeInsert ctx id (StorageRef.Macro macro)

    /// Add a new entry to the current scope
    let addBinding ctx id =
        let storage =
            if ctx.Parent.IsNone && ctx.OuterScopes.IsEmpty then
                StorageRef.Global(ctx.MangledName, Field id)
            else
                StorageRef.Local(getNextLocal ctx)

        scopeInsert ctx id storage
        storage

    /// Add a library declaration to the current context
    let addLibrary ctx name exports =
        let signature =
            { LibrarySignature.LibraryName = name
              LibrarySignature.Exports = exports }

        ctx.Libraries <- signature :: ctx.Libraries

    /// Import the bindings from a given libraryto the binder context
    let importLibrary ctx (library: LibrarySignature<StorageRef>) =
        List.iter (fun (id, storage) -> scopeInsert ctx id storage) library.Exports
        library.LibraryName |> mangleName

    /// Add a new level to the scopes
    let pushScope ctx =
        ctx.OuterScopes <- ctx.Scope :: ctx.OuterScopes

    // Set the scope back to a stored value
    let popScope ctx =
        match ctx.OuterScopes with
        | scope :: scopes ->
            ctx.Scope <- scope
            ctx.OuterScopes <- scopes
        | [] -> ice "Unbalanced scope pop"

    /// Convert the binder context into a bound root around the given expression
    let intoRoot ctx expr =
        let env = if ctx.HasDynamicEnv then Some([]) else None

        { Body = expr
          Locals = ctx.LocalCount
          Captures = ctx.Captures
          EnvMappings = env }

[<AutoOpen>]
module private Impl =

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
                    ctx.Diagnostics.Emit
                        BinderDiagnostics.patternBindError
                        formal.Location
                        "Only expect single ID after dot"

                match formal.Kind with
                | LegacyNodeKind.Ident(id) -> (formals, true, Some(id))
                | _ ->
                    ctx.Diagnostics.Emit BinderDiagnostics.patternBindError formal.Location "Expected ID after dot"
                    acc
            else
                match formal.Kind with
                | LegacyNodeKind.Dot -> (formals, true, None)
                | LegacyNodeKind.Ident(id) -> (id :: formals, false, None)
                | _ ->
                    ctx.Diagnostics.Emit
                        BinderDiagnostics.patternBindError
                        formal.Location
                        "Expected ID or dot in formals"

                    acc

        let (fmls, sawDot, dotted) = List.fold f ([], false, None) formals
        let fmls = List.rev fmls

        if sawDot then
            match dotted with
            | Some(d) -> BoundFormals.DottedList(fmls, d)
            | None ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (List.last formals).Location
                    "Saw dot but no ID in formals"

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
        | LegacyNodeKind.Ident(id) -> BoundFormals.Simple(id)
        | LegacyNodeKind.Form(formals) -> bindFormalsList ctx formals
        | _ ->
            "Unrecognised formal parameter list. Must be an ID or list pattern"
            |> ctx.Diagnostics.Emit BinderDiagnostics.invalidParameterPattern formals.Location

            BoundFormals.List([])

    /// Recognise a given form as a list of let binding specifications. This expects
    /// the `node` to be a form `()`, or `((id init) ...)`.
    let private parseBindingSpecs ctx node =
        // Bind each of the definitions
        let parseBindingSpec decl bindings =
            match decl with
            | { Kind = LegacyNodeKind.Form(binding) } ->
                match binding with
                | [ { Kind = LegacyNodeKind.Ident id }; body ] -> (id, body) :: bindings
                | _ ->
                    ctx.Diagnostics.Emit BinderDiagnostics.letBindError decl.Location "Invalid binding form"
                    bindings
            | _ ->
                ctx.Diagnostics.Emit BinderDiagnostics.letBindError decl.Location "Expeted a binding form"
                bindings

        match node with
        | { Kind = LegacyNodeKind.Form(decls) } -> List.foldBack (parseBindingSpec) decls []
        | _ ->
            ctx.Diagnostics.Emit BinderDiagnostics.letBindError node.Location "Expected binding list"
            []

    /// Emit a diagnostic for an ill-formed special form
    let private illFormedInCtx ctx location formName =
        formName
        |> sprintf "Ill-formed '%s' special form"
        |> ctx.Diagnostics.Emit BinderDiagnostics.illFormedSpecialForm location

        BoundExpr.Error

    /// Bind a Syntax Node
    ///
    /// Walks the syntax node building up a bound representation. This bound
    /// node no longer has direct references to identifiers and instead
    /// references storage locations.
    let rec bindInContext ctx node =
        match node.Kind with
        | LegacyNodeKind.Error -> ice "Attempt to bind an error node."
        | LegacyNodeKind.Constant c -> BoundLiteral.FromConstant c |> BoundExpr.Literal
        | LegacyNodeKind.Vector v -> List.map (bindDatum ctx) v |> BoundLiteral.Vector |> BoundExpr.Literal
        | LegacyNodeKind.ByteVector bv -> BoundExpr.Literal(BoundLiteral.ByteVector bv)
        | LegacyNodeKind.Dot ->
            ctx.Diagnostics.Emit BinderDiagnostics.patternBindError node.Location "Unexpected dot"
            BoundExpr.Error
        | LegacyNodeKind.Seq s -> bindSequence ctx s
        | LegacyNodeKind.Form f -> bindForm ctx f node
        | LegacyNodeKind.Ident id ->
            match BinderCtx.tryFindBinding ctx id with
            | Some s -> BoundExpr.Load(s)
            | None ->
                sprintf "The symbol %s is not defined in the current context" id
                |> ctx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol node.Location

                BoundExpr.Error
        | LegacyNodeKind.Quoted q -> bindQuoted ctx q

    and private bindDatum ctx node =
        match node.Kind with
        | LegacyNodeKind.Ident i -> BoundDatum.Ident i
        | LegacyNodeKind.Constant c -> BoundDatum.SelfEval(BoundLiteral.FromConstant c)
        | LegacyNodeKind.Dot -> BoundDatum.Ident "." // FIXME: This is definitely not right
        | LegacyNodeKind.Form f
        | LegacyNodeKind.Seq f -> List.map (bindDatum ctx) f |> BoundDatum.Compound
        | LegacyNodeKind.Quoted q -> bindDatum ctx q |> BoundDatum.Quoted
        | LegacyNodeKind.Vector v -> List.map (bindDatum ctx) v |> BoundLiteral.Vector |> BoundDatum.SelfEval
        | LegacyNodeKind.ByteVector v -> BoundLiteral.ByteVector v |> BoundDatum.SelfEval
        | LegacyNodeKind.Error ->
            ctx.Diagnostics.Emit BinderDiagnostics.malformedDatum node.Location "invalid item in quoted expression"
            BoundDatum.Ident("<ERROR>")

    and private bindQuoted ctx quoted =

        bindDatum ctx quoted |> BoundExpr.Quoted

    and private bindWithSequencePoint ctx expr =
        let inner = bindInContext ctx expr

        match inner with
        | BoundExpr.If _
        | BoundExpr.Seq _ -> inner
        | _ -> BoundExpr.SequencePoint(inner, expr.Location)

    and private bindSequence ctx exprs =
        List.map (bindWithSequencePoint ctx) exprs |> BoundExpr.Seq

    and private bindApplication ctx head rest node =
        let applicant = bindInContext ctx head

        match applicant with
        | BoundExpr.Load(StorageRef.Macro m) ->
            match Macros.macroApply m node with
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
        | BoundFormals.Simple(id) -> addFormal 0 id |> ignore
        | BoundFormals.List(fmls) -> (List.fold addFormal 0 fmls) |> ignore
        | BoundFormals.DottedList(fmls, dotted) ->
            let nextFormal = (List.fold addFormal 0 fmls)
            addFormal nextFormal dotted |> ignore

        let boundBody = bindSequence lambdaCtx body |> BinderCtx.intoRoot lambdaCtx

        BoundExpr.Lambda(formals, boundBody)

    and private bindLet ctx name body location declBinder =
        match body with
        | bindings :: body ->
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

    and private bindLibrary ctx location (library: LibraryDefinition) =
        // Process `(import ...)`
        let libCtx =
            library.LibraryName |> BinderCtx.createForGlobalScope Map.empty ctx.Libraries

        let imports =
            library.Declarations
            |> List.choose (function
                | LibraryDeclaration.Import i ->
                    i
                    |> List.choose (
                        Libraries.resolveImport ctx.Libraries
                        >> Result.map (BinderCtx.importLibrary libCtx >> BoundExpr.Import)
                        >> Result.mapError (libCtx.Diagnostics.Emit BinderDiagnostics.invalidImport location)
                        >> Option.ofResult
                    )
                    |> BoundExpr.Seq
                    |> Some
                | _ -> None)

        // Process the bodies of the library.
        let boundBodies =
            List.choose
                (function
                | LibraryDeclaration.Begin block -> block |> bindSequence libCtx |> Some
                | _ -> None)
                library.Declarations

        // Process `(export ...)` declarations.
        let lookupExport id extId =
            match BinderCtx.tryFindBinding libCtx id with
            | Some(x) -> Some((extId, x))
            | _ ->
                sprintf "Could not find exported item %s" id
                |> Diagnostic.Create BinderDiagnostics.missingExport location
                |> libCtx.Diagnostics.Add

                None

        let exports =
            library.Declarations
            |> List.choose (function
                | LibraryDeclaration.Export exp ->
                    exp
                    |> List.choose (function
                        | ExportSet.Plain p -> lookupExport p p
                        | ExportSet.Renamed rename -> lookupExport rename.From rename.To)
                    |> Some
                | _ -> None)
            |> List.concat

        BinderCtx.addLibrary ctx library.LibraryName exports
        ctx.Diagnostics.Append libCtx.Diagnostics.Take

        BoundExpr.Library(
            library.LibraryName,
            library.LibraryName |> BinderCtx.mangleName,
            exports,
            List.append imports boundBodies |> BoundExpr.Seq |> BinderCtx.intoRoot libCtx
        )

    and private bindForm ctx (form: LegacyNode list) node =
        let illFormed formName =
            illFormedInCtx ctx node.Location formName

        match form with
        | { Kind = LegacyNodeKind.Ident("if") } :: body ->
            let b = bindWithSequencePoint ctx

            match body with
            | [ cond; ifTrue; ifFalse ] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
            | [ cond; ifTrue ] -> BoundExpr.If((b cond), (b ifTrue), None)
            | _ -> illFormed "if"
        | { Kind = LegacyNodeKind.Ident("begin") } :: body -> bindSequence ctx body
        | { Kind = LegacyNodeKind.Ident("define") } :: body ->
            match body with
            | [ { Kind = LegacyNodeKind.Ident id } ] ->
                let storage = BinderCtx.addBinding ctx id
                BoundExpr.Store(storage, None)
            | [ { Kind = LegacyNodeKind.Ident id }; value ] ->
                let storage = BinderCtx.addBinding ctx id
                let value = bindInContext ctx value
                BoundExpr.Store(storage, Some(value))
            | ({ Kind = LegacyNodeKind.Form({ Kind = LegacyNodeKind.Ident id } :: formals) }) :: body ->
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
        | { Kind = LegacyNodeKind.Ident("lambda") } :: body ->
            match body with
            | formals :: body ->
                let boundFormals = bindFormals ctx formals
                bindLambdaBody ctx boundFormals body
            | _ -> illFormed "lambda"
        | { Kind = LegacyNodeKind.Ident("let") } :: body ->
            bindLet ctx "let" body node.Location (fun bindingSpecs ->

                // Bind the body of each binding spec first
                let decls =
                    bindingSpecs |> List.map (fun (id, body) -> (id, bindInContext ctx body))

                // Once the bodies are bound, we can create assignments and
                // initialise the environment
                let boundDecls =
                    decls
                    |> List.map (fun (id, body) ->
                        let storage = BinderCtx.addBinding ctx id
                        BoundExpr.Store(storage, Some(body)))

                boundDecls)
        | { Kind = LegacyNodeKind.Ident("let*") } :: body ->
            bindLet
                ctx
                "let*"
                body
                node.Location
                (
                // let* binds each spec sequentially
                List.map (fun (id, body) ->
                    let body = bindInContext ctx body
                    let storage = BinderCtx.addBinding ctx id
                    BoundExpr.Store(storage, Some(body))))
        | ({ Kind = LegacyNodeKind.Ident("letrec") } as head) :: body
        | ({ Kind = LegacyNodeKind.Ident("letrec*") } as head) :: body ->
            bindLet ctx "letrec" body node.Location (fun bindingSpecs ->


                // Get storage for each of the idents first into scope.
                let boundIdents =
                    bindingSpecs
                    |> List.map (fun (id, body) -> ((BinderCtx.addBinding ctx id), body))

                //        Validate that bindings don't read from
                //        un-initialised variables. The `letrec*` is allowed to
                //        reference previous variables. Plain `letrec` Isn't allowed
                //        to reference any.
                let mutable uniitialisedStorage = boundIdents |> List.map (fun (id, _) -> id)

                let rec checkUses location =
                    function
                    | Application(app, args) ->
                        checkUses location app
                        List.iter (checkUses location) args
                    | If(cond, cons, els) ->
                        checkUses location cond
                        checkUses location cons
                        Option.iter (checkUses location) els
                    | Seq(exprs) -> List.iter (checkUses location) exprs
                    | Lambda _ -> ()
                    | Load(s) -> checkStorage location s
                    | Store(s, v) ->
                        checkStorage location s
                        Option.iter (checkUses location) v
                    | SequencePoint(inner, _) -> checkUses location inner
                    | _ -> ()

                and checkStorage location s =
                    List.tryFindIndex ((=) s) uniitialisedStorage
                    |> Option.iter (fun idx ->
                        let (name, _) = bindingSpecs[idx]

                        name
                        |> sprintf "Reference to uninitialised variable '%s' in letrec binding"
                        |> ctx.Diagnostics.Emit BinderDiagnostics.uninitialisedVariale location)

                let isLetrecStar = head.Kind = LegacyNodeKind.Ident("letrec*")

                // Now all the IDs are in scope, bind the initialisers
                let boundDecls =
                    boundIdents
                    |> List.map (fun (storage, body) ->
                        let bound = bindInContext ctx body
                        checkUses body.Location bound

                        if isLetrecStar then
                            uniitialisedStorage <- List.tail uniitialisedStorage

                        BoundExpr.Store(storage, Some(bound)))

                boundDecls)
        | { Kind = LegacyNodeKind.Ident("let-syntax") } :: body ->
            bindLet ctx "let-syntax" body node.Location (fun bindingSpecs ->

                Seq.iter
                    (fun (id, syntaxRules) ->
                        match Macros.parseSyntaxRules id syntaxRules with
                        | Ok(macro) -> BinderCtx.addMacro ctx id macro
                        | Result.Error e -> ctx.Diagnostics.Add e)
                    bindingSpecs

                [])
        | { Kind = LegacyNodeKind.Ident("set!")
            Location = l } :: body ->
            match body with
            | [ { Kind = LegacyNodeKind.Ident(id) }; value ] ->
                let value = bindInContext ctx value

                match BinderCtx.tryFindBinding ctx id with
                | Some s -> BoundExpr.Store(s, Some(value))
                | None ->
                    sprintf "Attempt to set `%s` that was not yet defined" id
                    |> ctx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol l

                    BoundExpr.Error
            | _ -> illFormed "set!"
        | { Kind = LegacyNodeKind.Ident("quote") } :: body ->
            match body with
            | [ item ] -> bindQuoted ctx item
            | _ -> illFormed "quote"
        | { Kind = LegacyNodeKind.Ident("define-syntax") } :: body ->
            match body with
            | [ { Kind = LegacyNodeKind.Ident(id) }; syntaxRules ] ->
                match Macros.parseSyntaxRules id syntaxRules with
                | Ok(macro) ->
                    BinderCtx.addMacro ctx id macro
                    BoundExpr.Quoted(BoundDatum.Ident id)
                | Result.Error e ->
                    ctx.Diagnostics.Add e
                    BoundExpr.Error
            | _ -> illFormed "define-syntax"
        | { Kind = LegacyNodeKind.Ident("define-library") } :: body ->
            match body with
            | name :: body ->
                match Libraries.parseLibraryDefinition name body with
                | Ok(library, diags) ->
                    ctx.Diagnostics.Append diags
                    bindLibrary ctx node.Location library
                | Result.Error diags ->
                    ctx.Diagnostics.Append diags
                    BoundExpr.Error
            | _ -> illFormed "define-library"
        | { Kind = LegacyNodeKind.Ident("import") } :: body ->
            body
            |> List.map (fun item ->
                Libraries.parseImport ctx.Diagnostics item
                |> Libraries.resolveImport ctx.Libraries
                |> Result.mapError (ctx.Diagnostics.Emit BinderDiagnostics.invalidImport item.Location)
                |> Result.map (BinderCtx.importLibrary ctx >> BoundExpr.Import)
                |> Result.okOr BoundExpr.Error)
            |> BoundExpr.Seq
        | { Kind = LegacyNodeKind.Ident("case") } :: body -> unimpl "Case expressions not yet implemented"
        | head :: rest -> bindApplication ctx head rest node
        | [] -> BoundExpr.Literal BoundLiteral.Null

// ------------------------------ Public Binder API --------------------------

module Binder =

    /// Create a New Root Scope
    ///
    /// The root scope contains the global functions available to the program.
    let scopeFromLibraries (libs: seq<LibrarySignature<StorageRef>>) =
        libs |> Seq.collect (fun lib -> lib.Exports) |> Map.ofSeq

    /// The empty scope
    let emptyScope = Map.empty

    /// Bind a syntax node in a given scope
    ///
    /// Walks the parse tree and computes semantic information. The result of this
    /// call can be passed to the `Compile` or `Emit` API to be lowered to IL.
    let bind scope libraries node : BoundSyntaxTree =
        let ctx = BinderCtx.createForGlobalScope scope libraries [ "LispProgram" ]

        let bound = bindInContext ctx node |> BinderCtx.intoRoot ctx

        { Root = bound
          MangledName = ctx.MangledName
          Diagnostics = ctx.Diagnostics.Take }
