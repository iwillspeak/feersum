namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Ice

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

    let malformedCall =
        DiagnosticKind.Create DiagnosticLevel.Error 39 "Malformed procedure call"

// -- Global Binder Context ----------------------------------------------------

/// Binder Context Type
///
/// A binder context holds the shared state for binding a set of units in a
/// given bind pass. This is effectively the "environment" for the binder and
/// contains a the accumulated global state for the current binding pass.
type private BinderCtx =
    { Diagnostics: DiagnosticBag
      mutable Libraries: LibrarySignature<StorageRef> list
      SourceRegistry: SourceRegistry }

module private Binderctx =

    /// Create a new BinderCtx with the given scope and name
    ///
    /// Seeds the scope from the given `scope`.
    let create sourceRegistry libs =
        { SourceRegistry = sourceRegistry
          Libraries = libs
          Diagnostics = DiagnosticBag.Empty }

/// A single frame in the scope stack of a given binding frame. This is used to
/// record the resolved storage locations of values which are currently
/// accessable in the given context.
type ResolutionScope = Map<Ident, StorageRef>

/// The full resolution enviroment for a given frame. A stakc of scopes where
/// the head is the innermost scope and the tail is the outermost. Resolution
/// walks the scopes until an identifier is found or, if no binding resolution
/// is found, the outer frame should be searched.
type private ResolutionEnv = S of ResolutionScope list

module private ResolutionEnv =

    /// An empty resolution environment. This contains a single scope without
    /// any bindings.
    let empty = S [ Map.empty ]

    /// Push a new scope onto the resolution environment. This is used when
    /// entering a new `let` body or similar.
    let pushScope (S scopes) = S(Map.empty :: scopes)

    /// Pop a scope from the resolution environment. Used when leaving a scope
    /// body to remove bindings from the resolution environment.
    let popScope (S scopes) =
        match scopes with
        | [] -> ice "ResolutionEnv.popScope: no scopes to pop"
        | _ :: rest -> S rest

    /// Try to resolve an identifier in the resolution environment. This walks
    /// the scopes from innermost to outermost and returns the first binding
    /// found for the given identifier, if any.
    let tryResolve (S scopes) (ident: Ident) : StorageRef option =
        scopes |> List.tryPick (Map.tryFind ident)

    /// Extend the given scope with a new binding `ident` -> `var`
    let extend (S scopes) (ident: Ident) (var: StorageRef) =
        match scopes with
        | active :: tail -> S(Map.add ident var active :: tail)
        | _ -> ice "ResolutionEnvironment has no active scope"

/// Binder Frame Context
///
/// A frame context represents the state of the binder as it processes a
/// particular item. This includes the kind of item being processed and the
/// "resolution contet", a map from source identifiers to storage.
type private FrameCtx =
    { Kind: FrameKind
      BinderCtx: BinderCtx
      mutable Env: ResolutionEnv
      mutable LocalCount: int }

/// State specific to the kind of frame being bound. This reprsesents the
/// different types of items we can process.
and private FrameKind =
    /// Root frame. This represents a top level script o program frame being
    /// bound into the type with the given `mangledName`.
    | Global of mangledName: string

    /// Libraries behave similarly to a Global frame, but also have a parent
    /// frame which they can refer to for name resolution.
    | Library of mangledName: string * parent: FrameCtx

    /// Lamdba frames have mutable capture state which is used to track the
    /// variables captured from outer frames.
    | Lambda of LambdaState

/// Lamdba specific frame state. Needed because we must mutate the captures
/// as we process the body of the lambda so this can't live directly on the
/// `FrameKind.Lambda` case.
and private LambdaState =
    {
        Parent: FrameCtx
        /// Captures state of the lambda. If `None` then no environment captures
        /// exist. If `Some` then the list shows the captured variables.
        ///
        /// A `Some []` indicates a link environment.
        mutable Captures: StorageRef list option
    }


/// A helper module for working with `FrameCtx`. This contains functions for
/// creating and manipulating `FrameCtx` values, such as pushing and popping
/// frames and managing the resolution environment.
module private FrameCtx =

    let private createWithKind binderCtx kind =
        { Kind = kind
          BinderCtx = binderCtx
          Env = ResolutionEnv.empty
          LocalCount = 0 }

    /// Create a new `FrameCtx` for a global frame with the given `mangledName`.
    let createGlobal binderCtx mangledName scope =
        createWithKind binderCtx (Global mangledName)
        |> fun ctx -> { ctx with Env = S [ scope ] }

    /// Create a new `FrameCtx` for a library frame with the given `mangledName`
    /// and `parent` frame.
    let createLibrary binderCtx mangledName parent =
        createWithKind binderCtx (Library(mangledName, parent))

    /// Create a new `FrameCtx` for a lambda frame with the given parent frame.
    let createLambda binderCtx parent =
        createWithKind binderCtx (Lambda { Parent = parent; Captures = None })

    /// Resolve an identifier in the given `FrameCtx`.
    ///
    /// This first tries to resolve the identifier in the current frame's
    /// resolution environment. If that fails, it then tries to resolve the
    /// identifier in the parent frame, if one exists. If the identifier is
    /// found in the parent frame, it is registered as a capture on the current
    /// frame if the parent storage is a local, argument, captured variable, or
    /// environment variable. If the identifier is not found in the parent
    /// frame, then `None` is returned to indicate an unresolved identifier.
    let rec resolve (ctx: FrameCtx) (ident: Ident) : StorageRef option =
        match ResolutionEnv.tryResolve ctx.Env ident with
        | Some storage -> Some storage
        | None ->
            // Not in this frame's scopes. Try the parent (captures).
            match ctx.Kind with
            | Lambda state ->
                match resolve state.Parent ident with
                | Some outerStorage ->
                    // Register capture on this lambda frame.
                    match outerStorage with
                    | Captured _
                    | Arg _
                    | Local _
                    | Environment _ ->
                        state.Captures <-
                            match state.Captures with
                            | Some captures -> Some(outerStorage :: captures)
                            | None -> Some [ outerStorage ]

                        Some(StorageRef.Captured outerStorage)
                    | _ -> Some outerStorage
                | None -> None
            | Library(_, parent) -> resolve parent ident
            | Global _ ->
                // Global frames have no parent to search.
                None

    /// Finish a `FrameCtx` by producing a `BoundBody` with the given `body`
    /// expression and capture information resolved from the `ctx`'s state.
    let finish (ctx: FrameCtx) (body: BoundExpr) : BoundBody =
        let captures, env =
            match ctx.Kind with
            | Lambda state ->
                match state.Captures with
                | Some captures -> captures, Some([])
                | None -> [], None
            | _ -> [], None

        { Body = body
          Locals = ctx.LocalCount
          Captures = captures
          EnvMappings = env }


/// Resolution State for a Variable
type private NameResolution =
    | Resolved of StorageRef
    | SpecialForm of SpecialFormKind
    | Macro of Ident
    | Unbound

// -- Binder Implementation ----------------------------------------------------

[<AutoOpen>]
module private Impl =

    open System.Text.RegularExpressions

    /// Create a "Mangled" Name for a Given Source name
    ///
    /// This converts a sequecne of name parts, such as those found in a library
    /// or module in Scheme into a single CIL identifier safe string.
    let mangleName (name: string seq) : string =
        let mangleNamePart (part: string) =
            Regex.Replace(part, @"[^a-zA-Z0-9]+", "_")

        name |> Seq.map mangleNamePart |> String.concat "::"

    // Get the Syntax Location for a Given Syntax Node
    let private getSyntaxLocation (_ctx: BinderCtx) (stx: Stx) : TextLocation =
        // FIXME: We _should_ have to look the stx up in the source registry
        //        so we can `rangeToLocation` the span. But the current slop
        //        means the locations are actually just denormalised on each stx
        stx.Loc


    /// Parse a Formals List Pattern
    ///
    /// This parses the formals list pattern as supported by `(define .. )` forms,
    /// or `(lambda)` forms. Takes the body elements and optional tail element
    /// (from a DottedForm). Returns either a plain or dotted list formals pattern:
    ///   * `(<id>, .. )` - to bind each parameter to a unique identifier
    ///   * `(<id>, .. '.', <id>)` - to bind a fixed set of parameters with an
    ///     optional list of extra parameters.
    let private parseFormalsList ctx (body: Stx list) (tail: Stx option) =
        let parseFormal acc (formal: Stx) =
            match formal with
            | StxId(id, _, _) -> id :: acc
            | _ ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (getSyntaxLocation ctx formal)
                    "Expected identifier in formals"

                acc

        let fmls = List.fold parseFormal [] body |> List.rev

        match tail with
        | None -> BoundFormals.List(fmls)
        | Some tailExpr ->
            match tailExpr with
            | StxId(id, _, _) -> BoundFormals.DottedList(fmls, id)
            | _ ->
                ctx.Diagnostics.Emit
                    BinderDiagnostics.patternBindError
                    (getSyntaxLocation ctx tailExpr)
                    "Expected identifier after dot"

                BoundFormals.List(fmls)

    /// Parse a Lambda's Formal Arguments
    ///
    /// Parses the argument list for a lambda form and returns a `BoundFormals`
    /// instance describing the formal parameter pattern. The following
    /// types of formals patterns are suppoted:
    ///   * `<id>` - to bind the whole list to the given identifier
    ///   * Any of the list patterns supported by `parseFormalsList`
    let private parseFormals ctx (formals: Stx) =
        match formals with
        | StxId(id, _, _) -> BoundFormals.Simple(id)
        | StxList(body, tail, _, _) -> parseFormalsList ctx body tail
        | _ ->
            "Unrecognised formal parameter list. Must be an ID or list pattern"
            |> ctx.Diagnostics.Emit BinderDiagnostics.invalidParameterPattern (getSyntaxLocation ctx formals)

            BoundFormals.List([])


    /// Convert a syntactic datum to its bound-tree counterpart.
    let private datumToLiteral =
        function
        | StxDatum.Boolean b -> BoundLiteral.Boolean b
        | StxDatum.Number n -> BoundLiteral.Number n
        | StxDatum.Character c -> BoundLiteral.Character c
        | StxDatum.Str s -> BoundLiteral.Str s
        | StxDatum.ByteVector bs -> BoundLiteral.ByteVector bs

    /// Convert a Stx node to a BoundDatum (for quoted expressions).
    /// Returns None if the Stx tree contains a reader-level error node;
    /// the diagnostic was already emitted by Stx.ofExpr.
    let rec private stxToDatum (stx: Stx) : BoundDatum option =
        let mapItems items : BoundDatum list option =
            List.foldBack
                (fun item acc ->
                    match acc, stxToDatum item with
                    | Some ds, Some d -> Some(d :: ds)
                    | _ -> None)
                items
                (Some [])

        match stx with
        | StxId(name, _, _) -> Some(BoundDatum.Ident name)
        | StxDatum(d, _) -> Some(BoundDatum.SelfEval(datumToLiteral d))
        | StxList(items, None, _, _) -> mapItems items |> Option.map BoundDatum.Compound
        | StxList(items, Some tail, _, _) ->
            match mapItems items, stxToDatum tail with
            | Some ds, Some t -> Some(BoundDatum.Pair(ds, t))
            | _ -> None
        | StxVec(items, _, _) ->
            mapItems items
            |> Option.map (fun ds -> BoundDatum.SelfEval(BoundLiteral.Vector ds))
        | StxError _ -> None

    /// Resolve a Name to A Syntax or Variable
    ///
    /// This is the two-phase nested lookup required to translate a raw name
    /// from the syntax contex into the resolution of its binding.
    let private resolveName (ctx: FrameCtx) (stxEnv: StxEnvironment) (name: string) : NameResolution =
        match Stx.resolve name stxEnv with
        | Some(StxBinding.Variable id) ->
            match FrameCtx.resolve ctx id with
            | Some storage -> Resolved storage
            | _ -> Unbound
        | Some(StxBinding.Macro id) -> NameResolution.Macro id
        | Some(StxBinding.Special sp) -> NameResolution.SpecialForm sp
        | None -> Unbound

    /// Create a new Variable Binding in the Syntax Environent and the Frame Context
    let private mintFrameItem
        (ctx: FrameCtx)
        (stxEnv: StxEnvironment)
        (name: string)
        (storage: StorageRef)
        : Ident * StxEnvironment =
        let id = Ident.fresh ()
        let stxEnv = Map.add name (StxBinding.Variable id) stxEnv

        ctx.Env <- ResolutionEnv.extend ctx.Env id storage

        id, stxEnv

    /// Create a new Variable Binding in the Syntax Environent and the Frame Context
    let private mintVar (ctx: FrameCtx) (stxEnv: StxEnvironment) (name: string) : Ident * StorageRef * StxEnvironment =
        let storage =
            match ctx.Env, ctx.Kind with
            | S([ _ ]), FrameKind.Global libName
            | S([ _ ]), FrameKind.Library(libName, _) -> StorageRef.Global(libName, Field name)
            | _ ->
                let idx = ctx.LocalCount
                ctx.LocalCount <- ctx.LocalCount + 1
                StorageRef.Local idx

        let id, stxEnv = mintFrameItem ctx stxEnv name storage
        id, storage, stxEnv

    /// Create a new Variable Binding in the Syntax Environent and the Frame Context
    let private mintArg
        (ctx: FrameCtx)
        (stxEnv: StxEnvironment)
        (name: string)
        (idx: int)
        : Ident * StorageRef * StxEnvironment =
        let storage = StorageRef.Arg idx
        let id, stxEnv = mintFrameItem ctx stxEnv name storage

        id, storage, stxEnv


    /// Bind a Single Expression
    let rec bindInContext (ctx: FrameCtx) (stxEnv: StxEnvironment) (stx: Stx) : BoundExpr * StxEnvironment =
        let emitBinderError (stx: Stx) (diagKind: DiagnosticKind) (message: string) =
            message
            |> ctx.BinderCtx.Diagnostics.Emit diagKind (getSyntaxLocation ctx.BinderCtx stx)

            BoundExpr.Error, stxEnv

        match stx with
        | StxId(name, _, idEnv) ->
            match resolveName ctx stxEnv name with
            | Resolved storage -> BoundExpr.Load storage, stxEnv
            | SpecialForm _
            | Macro _ ->
                $"Invalid use of syntax item '{name}'"
                |> emitBinderError stx BinderDiagnostics.undefinedSymbol
            | Unbound ->
                $"The symbol '{name}' is not defined in the current context"
                |> emitBinderError stx BinderDiagnostics.undefinedSymbol
        | StxDatum(value, loc) -> datumToLiteral value |> BoundExpr.Literal, stxEnv

        | StxList(head, tail, loc, env) -> bindForm ctx (env |> Option.defaultValue stxEnv) head tail loc

        | StxVec(items, _, _) -> items |> List.choose stxToDatum |> BoundLiteral.Vector |> BoundExpr.Literal, stxEnv
        | StxError _ -> BoundExpr.Error, stxEnv

    and bindForm (ctx: FrameCtx) (stxEnv: StxEnvironment) (formBody: Stx list) (tail: Stx option) loc =
        match formBody with
        | StxId(name, _, env) :: args ->
            let nameEnv = env |> Option.defaultValue stxEnv

            match resolveName ctx nameEnv name with
            | NameResolution.SpecialForm kind ->
                tail
                |> Option.iter (fun x ->
                    $"Unexpected dotted tail in '{name}' form"
                    |> ctx.BinderCtx.Diagnostics.Emit
                        BinderDiagnostics.illFormedSpecialForm
                        (getSyntaxLocation ctx.BinderCtx x))

                bindSpecialForm ctx stxEnv kind args loc
            | NameResolution.Macro id -> unimpl "Macros not yet implemented"
            | NameResolution.Resolved storage ->
                BoundExpr.Load(storage) |> fun x -> bindApplication ctx stxEnv x args tail
            | NameResolution.Unbound ->
                $"Procedure call to '{name}' which is not bound in the current context"
                |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol loc

                BoundExpr.Error, stxEnv
        | head :: args ->
            let operator = bindInContext ctx stxEnv head |> fst
            bindApplication ctx stxEnv operator args tail
        | [] ->
            "Procedure call has no operator. Did you mean `'()`?"
            |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.malformedCall loc

            BoundExpr.Error, stxEnv

    and bindSpecialForm (ctx: FrameCtx) (stxEnv: StxEnvironment) (kind: SpecialFormKind) (args: Stx list) loc =
        let illFormed formName =
            $"Ill-formed '{formName}' special form"
            |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.illFormedSpecialForm loc

            BoundExpr.Error, stxEnv

        match kind with
        | SpecialFormKind.If ->
            let b = bindTopLevel ctx stxEnv >> fst

            match args with
            | [ cond; ifTrue; ifFalse ] -> BoundExpr.If(b cond, b ifTrue, Some(b ifFalse)), stxEnv
            | [ cond; ifTrue ] -> BoundExpr.If(b cond, b ifTrue, None), stxEnv
            | _ -> illFormed "if"

        | SpecialFormKind.Begin -> bindSeq ctx stxEnv args

        | SpecialFormKind.Define ->
            // FIXME: All of these bind in the outer syntax context rather than
            // resepecting the environemnt on the name. The fix here is proper
            // syntax sets rather than closures.
            match args with
            | [ StxId(name, _, _) ] ->
                let _id, storage, stxEnv = mintVar ctx stxEnv name
                BoundExpr.Store(storage, None), stxEnv
            | [ StxId(name, _, _); value ] ->
                let _id, storage, stxEnv = mintVar ctx stxEnv name
                let value, stxEnv = bindInContext ctx stxEnv value
                BoundExpr.Store(storage, Some value), stxEnv
            | StxList(StxId(name, _, _) :: formals, formalTail, formalsLoc, fromalsEnv) :: body ->

                let _id, storage, stxEnv = mintVar ctx stxEnv name

                let formals = parseFormalsList ctx.BinderCtx formals formalTail
                let lambda = bindLambdaBody ctx stxEnv formals body

                // TODO: We _used_ to re-laod the binding here to move items
                // that had been captured into their enviroment storage. I'm
                // not convinced we need it any longer as `Lower` looks to
                // lift stores as requried.
                BoundExpr.Store(storage, Some lambda), stxEnv

            | _ -> illFormed "define"

        | SpecialFormKind.Lambda ->
            match args with
            | formals :: body ->
                let formals = parseFormals ctx.BinderCtx formals
                let lambda = bindLambdaBody ctx stxEnv formals body
                lambda, stxEnv

            | _ -> illFormed "lambda"

        | SpecialFormKind.Quote ->
            match args with
            | [ datum ] ->
                stxToDatum datum
                |> Option.map (fun d -> BoundExpr.Quoted d, stxEnv)
                |> Option.defaultWith (fun () ->
                    $"Malformed datum in quote form"
                    |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.malformedDatum loc

                    BoundExpr.Error, stxEnv)
            | _ -> illFormed "quote"

        | SpecialFormKind.SetBang ->
            match args with
            | [ StxId(name, loc, nameEnv); value ] ->
                let nameEnv = nameEnv |> Option.defaultValue stxEnv

                match resolveName ctx nameEnv name with
                | NameResolution.Resolved storage ->
                    let value, stxEnv = bindInContext ctx stxEnv value
                    BoundExpr.Store(storage, Some value), stxEnv
                | NameResolution.SpecialForm _
                | NameResolution.Macro _ ->
                    $"Invalid use of syntax item '{name}' in set! form"
                    |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol loc

                    BoundExpr.Error, stxEnv
                | NameResolution.Unbound ->
                    $"The symbol '{name}' is not defined in the current context and cannot be assigned to"
                    |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.undefinedSymbol loc

                    BoundExpr.Error, stxEnv
            | _ -> illFormed "set!"

        | _ -> unimpl $"Form '{kind}' not yet implemented"

    and bindLambdaBody (ctx: FrameCtx) (stxEnv: StxEnvironment) (formals: BoundFormals) (body: Stx list) : BoundExpr =
        let lambdaCtx = FrameCtx.createLambda ctx.BinderCtx ctx

        let addFormal stxEnv idx name =
            let _, _, stxEnv = mintArg lambdaCtx stxEnv name idx
            stxEnv, idx + 1

        let stxEnv =
            match formals with
            | BoundFormals.Simple name -> addFormal stxEnv 0 name |> fst
            | BoundFormals.List names ->
                List.fold (fun (env, idx) (name: string) -> addFormal env idx name) (stxEnv, 0) names
                |> fst
            | BoundFormals.DottedList(names, tail) ->
                let stxEnv, nextFormal =
                    List.fold (fun (env, idx) (name: string) -> addFormal env idx name) (stxEnv, 0) names

                addFormal stxEnv nextFormal tail |> fst

        // Bind the body of the lambda in the new context.
        let body, _ = bindSeq lambdaCtx stxEnv body

        // Finish the lambda frame to get the capture information and produce
        // a `BoundBody` for the lambda.
        let boundBody = FrameCtx.finish lambdaCtx body

        BoundExpr.Lambda(formals, boundBody)

    and bindApplication
        (ctx: FrameCtx)
        (stxEnv: StxEnvironment)
        (boundOp: BoundExpr)
        (args: Stx list)
        (tail: Stx option)
        =
        tail
        |> Option.iter (fun x ->
            "Procedure calls may not use a dotted tail"
            |> ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.malformedCall (getSyntaxLocation ctx.BinderCtx x))

        let args, stxEnv = args |> List.mapFold (bindInContext ctx) stxEnv
        BoundExpr.Application(boundOp, args), stxEnv

    /// Bind a Top Level Item
    ///
    /// Top level items can be commands or definitions
    and bindTopLevel (ctx: FrameCtx) (stxEnv: StxEnvironment) (stx: Stx) : BoundExpr * StxEnvironment =
        let inner, stxEnv = bindInContext ctx stxEnv stx

        let WithSp =
            match inner with
            | BoundExpr.If _
            | BoundExpr.Seq _ -> inner
            | _ -> BoundExpr.SequencePoint(inner, getSyntaxLocation ctx.BinderCtx stx)

        WithSp, stxEnv

    /// Bind a Sequence of Expressions
    ///
    /// Returns the sequence as a `BoundExpr.Seq`, along with the updated syntax
    /// environment.
    and bindSeq (ctx: FrameCtx) (stxEnv: StxEnvironment) (stxs: Stx list) : BoundExpr * StxEnvironment =
        let exprs, env = stxs |> List.mapFold (bindTopLevel ctx) stxEnv

        BoundExpr.Seq(exprs), env


    /// Bind the Import Declarations in a Program
    ///
    /// Updates the `StxEnvironment` with the imported bindings and returns the
    /// remaining body of the program to be bound.
    let bindImports (ctx: FrameCtx) (stxEnv: StxEnvironment) (stxs: Stx list) : Stx list * StxEnvironment =
        let rec addImportsToEnv (stxEnv: StxEnvironment) (stx: Stx) : StxEnvironment =
            let imp = Libraries.parseImport ctx.BinderCtx.Diagnostics stx

            match Libraries.resolveImport ctx.BinderCtx.Libraries imp with
            | Ok library ->
                let newScope =
                    library.Exports
                    |> List.fold
                        (fun s (name, storage) ->
                            let id = Ident.fresh ()
                            ctx.Env <- ResolutionEnv.extend ctx.Env id storage
                            Map.add name (StxBinding.Variable id) s)
                        stxEnv

                newScope
            | Result.Error msg ->
                ctx.BinderCtx.Diagnostics.Emit BinderDiagnostics.invalidImport stx.Loc msg
                stxEnv

        let rec loop stxEnv remainingStx =
            match remainingStx with
            | StxList(StxId(id, _, envInner) :: args, None, _, envOuter) :: rest ->
                let envOuter = Option.defaultValue stxEnv envOuter
                let envInner = Option.defaultValue envOuter envInner
                let resolved = Stx.resolve id envInner

                match resolved with
                | Some(StxBinding.Special SpecialFormKind.Import) ->
                    let stxEnv = args |> List.fold addImportsToEnv stxEnv
                    loop stxEnv rest
                | _ ->
                    // Not an import, stop processing and return the remaining stx
                    remainingStx, stxEnv
            | _ -> remainingStx, stxEnv

        loop stxEnv stxs

    /// Bind a Program Root
    ///
    /// Walks the list of items and binds each one as a top level item
    let bindProgramRoot (ctx: FrameCtx) (stxEnv: StxEnvironment) (stxs: Stx list) : BoundBody =
        let stxs, stxEnv = bindImports ctx stxEnv stxs

        let body = bindSeq ctx stxEnv stxs |> fst

        { Body = body
          Locals = ctx.LocalCount
          Captures = []
          EnvMappings = None }


/// Public Binder API
///
/// This module contains the user-facing API for the Binder. Users call
/// `Binder.bind` to convera a sequence of `Expression` representgin the files
/// within a program or script into a `BoundExpr` which can then be passed to
/// `Lower` and `Emit`.
module Binder =

    /// Bind a List of Syntax Nodes in a Given Scope
    ///
    /// This is the main entry point for the binder. It takes a list of syntax
    /// nodes representing the program to be bound, along with a source registry
    /// and a scope for name resolution, and produces a `BoundSyntaxTree` which
    /// contains the bound syntax tree along with diagnostics and a mangled name.
    let private bindStx
        (ctx: BinderCtx)
        (stxEnv: StxEnvironment)
        (scope: Map<Ident, StorageRef>)
        (stx: Stx list)
        : BoundSyntaxTree =
        let name = "LispProgram"
        let rootFrame = FrameCtx.createGlobal ctx name scope

        let bound = bindProgramRoot rootFrame stxEnv stx

        { Root = bound
          MangledName = name
          Diagnostics = ctx.Diagnostics.Take }


    /// Bind a List of Compilation Units in a Given Scope
    ///
    /// Takes a list of dcocument and expression pairs and binds them in the
    /// given scope.
    let bindProgram
        (sourceRegistry: SourceRegistry)
        (stxEnv: StxEnvironment)
        (scope: Map<Ident, StorageRef>)
        (libs: seq<LibrarySignature<StorageRef>>)
        (units: Tree.Program list)
        : BoundSyntaxTree =
        let ctx = Binderctx.create sourceRegistry (List.ofSeq libs)

        let toBind =
            units
            |> Seq.collect (fun unit -> unit.Body |> Seq.map (Stx.ofExpr sourceRegistry unit.DocId ctx.Diagnostics))

        bindStx ctx stxEnv scope (List.ofSeq toBind)
