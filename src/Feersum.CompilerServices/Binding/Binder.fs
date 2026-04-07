namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices
open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
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
[<CustomComparison; CustomEquality>]
type StorageRef =
    | Macro of Macro
    | Local of int
    | Global of string * GlobalType
    | Arg of int
    | Environment of int * StorageRef
    | Captured of StorageRef

    /// Ordinal for a given StorageRef variant (used in comparison)
    static member private Ordinal =
        function
        | Macro _ -> 0
        | Local _ -> 1
        | Global _ -> 2
        | Arg _ -> 3
        | Environment _ -> 4
        | Captured _ -> 5

    interface System.IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? StorageRef as other ->
                match this, other with
                | Macro m1, Macro m2 ->
                    compare
                        (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m1))
                        (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m2))
                | Local a, Local b -> compare a b
                | Global(n1, t1), Global(n2, t2) ->
                    let c = compare n1 n2
                    if c <> 0 then c else compare t1 t2
                | Arg a, Arg b -> compare a b
                | Environment(n, s), Environment(m, t) ->
                    let c = compare n m
                    if c <> 0 then c else (s :> System.IComparable).CompareTo(t)
                | Captured s, Captured t -> (s :> System.IComparable).CompareTo(t)
                | _ -> compare (StorageRef.Ordinal this) (StorageRef.Ordinal other)
            | _ -> 0

    override this.Equals(obj) =
        match obj with
        | :? StorageRef as other ->
            match this, other with
            | Macro m1, Macro m2 -> System.Object.ReferenceEquals(m1, m2)
            | Local a, Local b -> a = b
            | Global(n1, t1), Global(n2, t2) -> n1 = n2 && t1 = t2
            | Arg a, Arg b -> a = b
            | Environment(n, s), Environment(m, t) -> n = m && s = t
            | Captured s, Captured t -> s = t
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Macro m -> System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(m)
        | Local i -> hash ("Local", i)
        | Global(n, t) -> hash ("Global", n, t)
        | Arg i -> hash ("Arg", i)
        | Environment(n, s) -> hash ("Env", n, s)
        | Captured s -> hash ("Captured", s)

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

    static member FromConstantValue(cv: ConstantValue option) =
        match cv with
        | Some(NumVal n) -> BoundLiteral.Number n
        | Some(StrVal s) -> BoundLiteral.Str s
        | Some(BoolVal b) -> BoundLiteral.Boolean b
        | Some(CharVal c) -> BoundLiteral.Character(Option.defaultValue '\u0000' c)
        | None -> BoundLiteral.Null

/// Bound Datum Element
///
/// Represents a form or atom when used as a data element rather than as a
/// program element. Used to hold the contents of quoted expressions, as well
/// as the elements within a vector literal.
and BoundDatum =
    | Compound of BoundDatum list
    | Pair of BoundDatum list * BoundDatum
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
      mutable CurrentDocument: TextDocument option
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
          CurrentDocument = None
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
          CurrentDocument = parent.CurrentDocument
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

    /// Get the TextLocation for a given expression using the binder context's document
    let private getNodeLocation ctx (expr: Expression) =
        match ctx.CurrentDocument with
        | Some doc -> TextDocument.rangeToLocation doc expr.SyntaxRange
        | None -> TextLocation.Missing

    /// Bind a Formals List Pattern
    ///
    /// This binds the formals list pattern as supported by `(define .. )` forms,
    /// or `(lambda)` forms. Takes the body elements and optional tail element
    /// (from a DottedForm). Returns either a plain or dotted list formals pattern:
    ///   * `(<id>, .. )` - to bind each parameter to a unique identifier
    ///   * `(<id>, .. '.', <id>)` - to bind a fixed set of parameters with an
    ///     optional list of extra parameters.
    let private bindFormalsList ctx (body: Expression list) (tail: Expression option) =
        let parseFormal acc (formal: Expression) =
            match formal with
            | Symbol id -> id :: acc
            | _ ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (getNodeLocation ctx formal)
                    "Expected identifier in formals"

                acc

        let fmls = List.fold parseFormal [] body |> List.rev

        match tail with
        | None -> BoundFormals.List(fmls)
        | Some tailExpr ->
            match tailExpr with
            | Symbol id -> BoundFormals.DottedList(fmls, id)
            | _ ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (getNodeLocation ctx tailExpr)
                    "Expected identifier after dot"

                BoundFormals.List(fmls)

    /// Bind a Lambda's Formal Arguments
    ///
    /// Parses the argument list for a lambda form and returns a `BoundFormals`
    /// instance describing the formal parameter pattern. The following
    /// types of formals patterns are suppoted:
    ///   * `<id>` - to bind the whole list to the given identifier
    ///   * Any of the list patterns supported by `bindFormalsList`
    let private bindFormals ctx (formals: Expression) =
        match formals with
        | Symbol id -> BoundFormals.Simple(id)
        | Form body -> bindFormalsList ctx body None
        | DottedForm(body, tail) -> bindFormalsList ctx body tail
        | _ ->
            "Unrecognised formal parameter list. Must be an ID or list pattern"
            |> ctx.Diagnostics.Emit BinderDiagnostics.invalidParameterPattern (getNodeLocation ctx formals)

            BoundFormals.List([])

    /// Recognise a given form as a list of let binding specifications. This expects
    /// the `node` to be a form `()`, or `((id init) ...)`.
    let private parseBindingSpecs ctx (node: Expression) =
        // Bind each of the definitions
        let parseBindingSpec (decl: Expression) bindings =
            match decl with
            | Form [ Symbol id; body ] -> (id, body) :: bindings
            | _ ->
                ctx.Diagnostics.Emit BinderDiagnostics.letBindError (getNodeLocation ctx decl) "Invalid binding form"
                bindings

        match node with
        | Form decls -> List.foldBack (parseBindingSpec) decls []
        | _ ->
            ctx.Diagnostics.Emit BinderDiagnostics.letBindError (getNodeLocation ctx node) "Expected binding list"
            []

    /// Emit a diagnostic for an ill-formed special form
    let private illFormedInCtx ctx location formName =
        formName
        |> sprintf "Ill-formed '%s' special form"
        |> ctx.Diagnostics.Emit BinderDiagnostics.illFormedSpecialForm location

        BoundExpr.Error

    let private bindByteVecInner ctx node (bv: ByteVec) =
        match bv.Body |> Seq.map (fun x -> x.Value) |> Result.collectAll with
        | Ok bytes -> bytes |> BoundLiteral.ByteVector |> Ok

        | Result.Error messages ->
            messages
            |> List.iter (fun msg ->
                ctx.Diagnostics.Emit BinderDiagnostics.malformedDatum (getNodeLocation ctx node) msg)

            Result.Error()

    /// Bind a Syntax Node
    ///
    /// Walks the syntax node building up a bound representation. This bound
    /// node no longer has direct references to identifiers and instead
    /// references storage locations.
    let rec bindInContext ctx (node: Expression) =
        match node with
        | ByteVecNode b ->
            match bindByteVecInner ctx node b with
            | Ok bv -> bv |> BoundExpr.Literal
            | Result.Error() -> BoundExpr.Error
        | VecNode v ->
            v.Body
            |> Seq.map (bindDatum ctx)
            |> List.ofSeq
            |> BoundLiteral.Vector
            |> BoundExpr.Literal
        | FormNode f ->
            let body = f.Body |> List.ofSeq

            match f.DottedTail with
            | Some _ ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (getNodeLocation ctx node)
                    "Unexpected dotted form in expression position"

                BoundExpr.Error
            | None -> bindForm ctx body node
        | ConstantNode c -> BoundLiteral.FromConstantValue c.Value |> BoundExpr.Literal
        | SymbolNode s ->
            let id = s.CookedValue

            match BinderCtx.tryFindBinding ctx id with
            | Some st -> BoundExpr.Load(st)
            | None ->
                sprintf "The symbol %s is not defined in the current context" id
                |> ctx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol (getNodeLocation ctx node)

                BoundExpr.Error
        | QuotedNode q ->
            match q.Inner with
            | Some inner -> bindQuoted ctx inner
            | None ->
                ctx.Diagnostics.Emit BinderDiagnostics.patternBindError (getNodeLocation ctx node) "Empty quotation"

                BoundExpr.Error

    and private bindDatum ctx (node: Expression) =
        match node with
        | SymbolNode s -> BoundDatum.Ident s.CookedValue
        | ConstantNode c -> BoundDatum.SelfEval(BoundLiteral.FromConstantValue c.Value)
        | FormNode f ->
            let body = f.Body |> Seq.map (bindDatum ctx) |> List.ofSeq

            match f.DottedTail with
            | None -> BoundDatum.Compound body
            | Some tail ->
                match tail.Body with
                | Some tailExpr -> BoundDatum.Pair(body, bindDatum ctx tailExpr)
                | None ->
                    ctx.Diagnostics.Emit
                        BinderDiagnostics.malformedDatum
                        (getNodeLocation ctx node)
                        "invalid dotted pair in quoted expression"

                    BoundDatum.Compound body
        | VecNode v ->
            v.Body
            |> Seq.map (bindDatum ctx)
            |> List.ofSeq
            |> BoundLiteral.Vector
            |> BoundDatum.SelfEval
        | ByteVecNode b ->
            match bindByteVecInner ctx node b with
            | Ok bv -> BoundDatum.SelfEval(bv)
            | Result.Error() -> BoundDatum.Ident "<ERROR>"
        | QuotedNode q ->
            match q.Inner with
            | Some inner -> bindDatum ctx inner |> BoundDatum.Quoted
            | None ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.malformedDatum
                    (getNodeLocation ctx node)
                    "invalid item in quoted expression"

                BoundDatum.Ident "<ERROR>"

    and private bindQuoted ctx (quoted: Expression) =
        bindDatum ctx quoted |> BoundExpr.Quoted

    and private bindWithSequencePoint ctx (expr: Expression) =
        let inner = bindInContext ctx expr

        match inner with
        | BoundExpr.If _
        | BoundExpr.Seq _ -> inner
        | _ -> BoundExpr.SequencePoint(inner, getNodeLocation ctx expr)

    and bindSequence ctx (exprs: Expression list) =
        List.map (bindWithSequencePoint ctx) exprs |> BoundExpr.Seq

    and private bindApplication ctx (head: Expression) (rest: Expression list) (node: Expression) =
        let applicant = bindInContext ctx head

        match applicant with
        | BoundExpr.Load(StorageRef.Macro m) ->
            match Macros.macroApply m node (getNodeLocation ctx node) with
            | Ok ast -> bindInContext ctx ast
            | Result.Error diag ->
                ctx.Diagnostics.Add diag
                BoundExpr.Error
        | _ -> BoundExpr.Application(applicant, List.map (bindInContext ctx) rest)

    and private bindLambdaBody ctx formals (body: Expression list) =
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

    and private bindLet ctx name (body: Expression list) location declBinder =
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

        // Propagate the current document so sequence points inside the library
        // have valid source locations.
        libCtx.CurrentDocument <- ctx.CurrentDocument

        let imports =
            library.Declarations
            |> List.choose (function
                | LibraryDeclaration.Import i ->
                    i
                    |> List.choose (
                        Libraries.resolveImport ctx.Libraries
                        >> Result.map (BinderCtx.importLibrary libCtx >> (fun _ -> BoundExpr.Nop))
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

    and private bindForm ctx (form: Expression list) (node: Expression) =
        let illFormed formName =
            illFormedInCtx ctx (getNodeLocation ctx node) formName

        match form with
        | Symbol "if" :: body ->
            let b = bindWithSequencePoint ctx

            match body with
            | [ cond; ifTrue; ifFalse ] -> BoundExpr.If((b cond), (b ifTrue), Some(b ifFalse))
            | [ cond; ifTrue ] -> BoundExpr.If((b cond), (b ifTrue), None)
            | _ -> illFormed "if"
        | Symbol "begin" :: body -> bindSequence ctx body
        | Symbol "define" :: body ->
            match body with
            | [ Symbol id ] ->
                let storage = BinderCtx.addBinding ctx id
                BoundExpr.Store(storage, None)
            | [ Symbol id; value ] ->
                let storage = BinderCtx.addBinding ctx id
                let value = bindInContext ctx value
                BoundExpr.Store(storage, Some(value))
            | Form(Symbol id :: formals) :: body ->
                // Add the binding for this lambda to the scope _before_ lowering
                // the body. This makes recursive calls possible.
                BinderCtx.addBinding ctx id |> ignore

                let lambda = bindLambdaBody ctx (bindFormalsList ctx formals None) body
                // Look the storage back up. This is key as the lambda, or one of
                // the lambdas nested inside it, could have captured the lambda
                // and moved it to the environment.
                let storage = (BinderCtx.tryFindBinding ctx id).Value
                BoundExpr.Store(storage, Some(lambda))
            | DottedForm(Symbol id :: formals, tail) :: body ->
                BinderCtx.addBinding ctx id |> ignore

                let lambda = bindLambdaBody ctx (bindFormalsList ctx formals tail) body
                let storage = (BinderCtx.tryFindBinding ctx id).Value
                BoundExpr.Store(storage, Some(lambda))
            | _ -> illFormed "define"
        | Symbol "lambda" :: body ->
            match body with
            | formals :: body ->
                let boundFormals = bindFormals ctx formals
                bindLambdaBody ctx boundFormals body
            | _ -> illFormed "lambda"
        | Symbol "let" :: body ->
            bindLet ctx "let" body (getNodeLocation ctx node) (fun bindingSpecs ->

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
        | Symbol "let*" :: body ->
            bindLet
                ctx
                "let*"
                body
                (getNodeLocation ctx node)
                (
                // let* binds each spec sequentially
                List.map (fun (id, body) ->
                    let body = bindInContext ctx body
                    let storage = BinderCtx.addBinding ctx id
                    BoundExpr.Store(storage, Some(body))))
        | Symbol id :: body when id = "letrec" || id = "letrec*" ->
            let isLetrecStar = id = "letrec*"

            bindLet ctx "letrec" body (getNodeLocation ctx node) (fun bindingSpecs ->

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

                // Now all the IDs are in scope, bind the initialisers
                let boundDecls =
                    boundIdents
                    |> List.map (fun (storage, body) ->
                        let bound = bindInContext ctx body
                        checkUses (getNodeLocation ctx body) bound

                        if isLetrecStar then
                            uniitialisedStorage <- List.tail uniitialisedStorage

                        BoundExpr.Store(storage, Some(bound)))

                boundDecls)
        | Symbol "let-syntax" :: body ->
            bindLet ctx "let-syntax" body (getNodeLocation ctx node) (fun bindingSpecs ->

                Seq.iter
                    (fun (id, syntaxRules) ->
                        match Macros.parseSyntaxRules id syntaxRules with
                        | Ok(macro) -> BinderCtx.addMacro ctx id macro
                        | Result.Error e -> ctx.Diagnostics.Add e)
                    bindingSpecs

                [])
        | (Symbol "set!" as setNode) :: body ->
            match body with
            | [ Symbol id; value ] ->
                let value = bindInContext ctx value

                match BinderCtx.tryFindBinding ctx id with
                | Some s -> BoundExpr.Store(s, Some(value))
                | None ->
                    sprintf "Attempt to set `%s` that was not yet defined" id
                    |> ctx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol (getNodeLocation ctx setNode)

                    BoundExpr.Error
            | _ -> illFormed "set!"
        | Symbol "quote" :: body ->
            match body with
            | [ item ] -> bindQuoted ctx item
            | _ -> illFormed "quote"
        | Symbol "define-syntax" :: body ->
            match body with
            | [ Symbol id; syntaxRules ] ->
                match Macros.parseSyntaxRules id syntaxRules with
                | Ok(macro) ->
                    BinderCtx.addMacro ctx id macro
                    BoundExpr.Quoted(BoundDatum.Ident id)
                | Result.Error e ->
                    ctx.Diagnostics.Add e
                    BoundExpr.Error
            | _ -> illFormed "define-syntax"
        | Symbol "define-library" :: body ->
            match body with
            | name :: body ->
                match Libraries.parseLibraryDefinition ctx.CurrentDocument name body with
                | Ok(library, diags) ->
                    ctx.Diagnostics.Append diags
                    bindLibrary ctx (getNodeLocation ctx node) library
                | Result.Error diags ->
                    ctx.Diagnostics.Append diags
                    BoundExpr.Error
            | _ -> illFormed "define-library"
        | Symbol "import" :: body ->
            body
            |> List.map (fun item ->
                Libraries.parseImport ctx.Diagnostics ctx.CurrentDocument item
                |> Libraries.resolveImport ctx.Libraries
                |> Result.mapError (ctx.Diagnostics.Emit BinderDiagnostics.invalidImport (getNodeLocation ctx item))
                |> Result.map (BinderCtx.importLibrary ctx >> (fun _ -> BoundExpr.Nop))
                |> Result.okOr BoundExpr.Error)
            |> BoundExpr.Seq
        | Symbol "case" :: _ -> unimpl "Case expressions not yet implemented"
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

    /// Bind a list of compilation units in a given scope
    ///
    /// Takes a list of (TextDocument * Expression list) pairs, one per
    /// compilation unit, and binds them sequentially in a shared scope.
    /// The result can be passed to the `Compile` or `Emit` API to be lowered to IL.
    let bind scope libraries (units: (TextDocument * Expression list) list) : BoundSyntaxTree =
        let ctx = BinderCtx.createForGlobalScope scope libraries [ "LispProgram" ]

        let bound =
            units
            |> List.collect (fun (doc, exprs) ->
                ctx.CurrentDocument <- Some doc

                match bindSequence ctx exprs with
                | BoundExpr.Seq stmts -> stmts
                | x -> [ x ])
            |> BoundExpr.Seq
            |> BinderCtx.intoRoot ctx

        { Root = bound
          MangledName = ctx.MangledName
          Diagnostics = ctx.Diagnostics.Take }
