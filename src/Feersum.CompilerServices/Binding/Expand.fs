namespace Feersum.CompilerServices.NewBindingTest

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.New
open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Binding.New.Libraries

// -- Diagnostics --------------------------------------------------------------

module private Diag =
    let expandError = DiagnosticKind.Create DiagnosticLevel.Error 50 "Expansion error"

    let undefinedSymbol =
        DiagnosticKind.Create DiagnosticLevel.Error 51 "Reference to undefined symbol"

    let illFormedForm =
        DiagnosticKind.Create DiagnosticLevel.Error 52 "Ill-formed special form"

    let invalidFormals =
        DiagnosticKind.Create DiagnosticLevel.Error 53 "Invalid formal parameter list"

    let illFormedBinding =
        DiagnosticKind.Create DiagnosticLevel.Error 54 "Ill-formed binding specification"

    let uninitialisedVar =
        DiagnosticKind.Create DiagnosticLevel.Error 55 "Reference to uninitialised variable in letrec binding"

    let invalidMacro =
        DiagnosticKind.Create DiagnosticLevel.Error 56 "Invalid macro definition"


// -- Transformer and BindingMeaning types -------------------------------------

/// What a Ident resolves to at compile time.
type BindingMeaning = Variable of StorageRef * lambdaDepth: int

/// Ident → its compile-time meaning.
type BindingMap = Map<Ident, BindingMeaning>

/// Shared mutable registry mapping macro Idents to their SyntaxTransformer
/// implementations. A single instance is created per compilation unit and
/// shared by reference across all ExpandCtx children.
///
/// The two-step protocol (reserve → register) allows `letrec-syntax` to place
/// all macro Idents into the definition-site scope before parsing any
/// transformer, so each transformer's `syntax-rules` sees the full mutually-
/// recursive scope. For `let-syntax` only the register step is used (macros
/// are parsed against the outer scope and reserved+registered together).
type MacroRegistry() =
    let mutable map: Map<Ident, SyntaxTransformer> = Map.empty

    /// Register a transformer implementation under a previously-reserved Ident.
    member _.Register (id: Ident) (transformer: SyntaxTransformer) =
        map <- Map.add id transformer map

    /// Look up the transformer for a macro Ident. Returns None if not yet
    /// registered (e.g. a forward-declared but unresolved macro handle).
    member _.TryFind(id: Ident) : SyntaxTransformer option = Map.tryFind id map

// -- ExpandCtx ----------------------------------------------------------------

/// Mutable per-lambda state threaded through the expander.
type ExpandCtx =
    {
        mutable LocalCount: int
        mutable Captures: StorageRef list
        mutable HasDynEnv: bool
        /// Nesting depth of lambda frames. 0 = global/library scope.
        /// Incremented in `childForLambda`. Used to detect cross-lambda captures
        /// via comparison with `Variable(_, lambdaDepth)` in BindingMap.
        LambdaDepth: int
        /// Mutable map from Ident to its compile-time meaning.
        /// Initialized from the parent's BindingMap (inheriting all visible bindings);
        /// extended as new bindings are introduced in this frame.
        mutable BindingMap: BindingMap
        /// Number of let/let*/letrec body scopes enclosing the current point.
        /// `define` within a let body should produce a Local even when IsGlobal
        /// is true (mirrors the old binder's pushScope/popScope logic).
        mutable ScopeDepth: int
        Diagnostics: DiagnosticBag
        Registry: SourceRegistry
        MangledName: string
        IsGlobal: bool
        mutable Libraries: LibrarySignature<StorageRef> list
        /// Shared macro registry for this compilation unit. A single
        /// MacroRegistry is created in createGlobal and passed by reference
        /// to all child contexts so that macros registered anywhere are
        /// visible everywhere the scope entry points.
        MacroRegistry: MacroRegistry
        Parent: ExpandCtx option
    }

module ExpandCtx =

    /// Create a root context for global-scope expansion.
    let createGlobal
        (registry: SourceRegistry)
        (mangledName: string)
        (libraries: Feersum.CompilerServices.Binding.LibrarySignature<StorageRef> list)
        =
        let convertedLibs: LibrarySignature<StorageRef> list =
            libraries
            |> List.map (fun s ->
                { LibraryName = s.LibraryName
                  Exports = s.Exports })

        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          LambdaDepth = 0
          BindingMap = Map.empty
          ScopeDepth = 0
          Diagnostics = DiagnosticBag.Empty
          Registry = registry
          MangledName = mangledName
          IsGlobal = true
          Libraries = convertedLibs
          MacroRegistry = MacroRegistry()
          Parent = None }

    /// Create a child context for a lambda body.
    let childForLambda (parent: ExpandCtx) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          LambdaDepth = parent.LambdaDepth + 1
          BindingMap = parent.BindingMap
          ScopeDepth = 0
          Diagnostics = parent.Diagnostics
          Registry = parent.Registry
          MangledName = parent.MangledName
          IsGlobal = false
          Libraries = parent.Libraries
          MacroRegistry = parent.MacroRegistry
          Parent = Some parent }

    /// Create a child context for a library body (own global scope).
    let childForLibrary (parent: ExpandCtx) (mangledName: string) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          LambdaDepth = 0
          BindingMap = parent.BindingMap
          ScopeDepth = 0
          Diagnostics = parent.Diagnostics
          Registry = parent.Registry
          MangledName = mangledName
          IsGlobal = true
          Libraries = parent.Libraries
          MacroRegistry = parent.MacroRegistry
          Parent = Some parent }

    let resolveLocation (_ctx: ExpandCtx) (loc: TextLocation) = loc

    let emitError (ctx: ExpandCtx) (kind: DiagnosticKind) (loc: TextLocation) (msg: string) =
        ctx.Diagnostics.Emit kind loc msg

    let nextLocal (ctx: ExpandCtx) =
        let idx = ctx.LocalCount
        ctx.LocalCount <- idx + 1
        idx

    let mintStorage (ctx: ExpandCtx) (name: string) : StorageRef =
        if ctx.IsGlobal && ctx.ScopeDepth = 0 then
            StorageRef.Global(ctx.MangledName, Field name)
        else
            StorageRef.Local(nextLocal ctx)

    let intoBody (ctx: ExpandCtx) (exprs: BoundExpr list) : BoundBody =
        let env = if ctx.HasDynEnv then Some [] else None

        { Body = BoundExpr.Seq exprs
          Locals = ctx.LocalCount
          Captures = ctx.Captures
          EnvMappings = env }

    let capture (ctx: ExpandCtx) (outer: StorageRef) : StorageRef =
        ctx.Captures <- outer :: ctx.Captures
        ctx.HasDynEnv <- true
        StorageRef.Captured outer

    /// Walk the parent context chain, registering captures at each lambda
    /// boundary crossed, until we reach the frame where `definedAt` was
    /// introduced.  Replaces the old `tryFindInScope` parent-chain walk.
    let rec captureAcrossLambdas (storage: StorageRef) (definedAt: int) (ctx: ExpandCtx) : StorageRef =
        if ctx.LambdaDepth <= definedAt then
            // We are at or above the defining frame — no capture needed here.
            storage
        else
            match ctx.Parent with
            | Some parent ->
                let outerStorage = captureAcrossLambdas storage definedAt parent

                match outerStorage with
                | Captured _
                | Arg _
                | Local _
                | Environment _ ->
                    ctx.Captures <- outerStorage :: ctx.Captures
                    ctx.HasDynEnv <- true
                    StorageRef.Captured outerStorage
                | _ -> outerStorage
            | None -> storage

    /// Introduce a new variable binding: fresh Ident, storage allocation,
    /// register in ctx.BindingMap, return (id, storage, extended scope).
    let mintVar (ctx: ExpandCtx) (name: string) (scope: StxEnvironment) : Ident * StorageRef * StxEnvironment =
        let id = Ident.fresh ()
        let storage = mintStorage ctx name
        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
        id, storage, Map.add name (StxBinding.Variable id) scope

    /// Reserve a macro name in scope: allocates a fresh Ident, adds
    /// `Macro id` to the scope, and returns `(id, extendedScope)`.
    /// The transformer implementation must be registered separately via
    /// `registerMacro` before the macro is actually invoked at expansion time.
    /// This two-step approach lets `letrec-syntax` pre-populate all macro
    /// Idents into the definition-site scope before parsing any transformer.
    let reserveMacro (name: string) (scope: StxEnvironment) : Ident * StxEnvironment =
        let id = Ident.fresh ()
        id, Map.add name (StxBinding.Macro id) scope

    /// Store the transformer implementation for a previously-reserved macro Ident.
    let registerMacro (ctx: ExpandCtx) (id: Ident) (transformer: SyntaxTransformer) =
        ctx.MacroRegistry.Register id transformer

    /// Register an existing StorageRef as a visible variable in scope.
    /// Used to seed preloaded library bindings into the initial scope
    /// without allocating new storage (e.g. for Script / REPL mode).
    let registerStorage (ctx: ExpandCtx) (name: string) (storage: StorageRef) (scope: StxEnvironment) : StxEnvironment =
        let id = Ident.fresh ()
        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
        Map.add name (StxBinding.Variable id) scope

// -- Built-in scope -----------------------------------------------------------

module StxEnvironment =

    /// Map a well-known keyword name to its `SpecialFormKind`, or return `None`
    /// if the name is not a built-in special form.
    let tryResolveSpecial (name: string) : SpecialFormKind option =
        match name with
        | "if" -> Some SpecialFormKind.If
        | "lambda" -> Some SpecialFormKind.Lambda
        | "define" -> Some SpecialFormKind.Define
        | "set!" -> Some SpecialFormKind.SetBang
        | "begin" -> Some SpecialFormKind.Begin
        | "quote" -> Some SpecialFormKind.Quote
        | "let" -> Some SpecialFormKind.Let
        | "let*" -> Some SpecialFormKind.LetStar
        | "letrec" -> Some SpecialFormKind.Letrec
        | "letrec*" -> Some SpecialFormKind.LetrecStar
        | "define-syntax" -> Some SpecialFormKind.DefineSyntax
        | "let-syntax" -> Some SpecialFormKind.LetSyntax
        | "letrec-syntax" -> Some SpecialFormKind.LetrecSyntax
        | "define-library" -> Some SpecialFormKind.DefineLibrary
        | "import" -> Some SpecialFormKind.Import
        | _ -> None

    /// The empty initial scope. Special forms are not stored in the scope;
    /// they are recognised by name in `resolveHead` via `tryResolveSpecial`.
    /// Callers extend this with macro and variable bindings before expansion.
    let builtin: StxEnvironment = Map.empty

// Backward-compatibility alias so existing call-sites can still write SyntaxEnv.builtin.
module SyntaxEnv =
    let builtin = StxEnvironment.builtin

// -- Expander -----------------------------------------------------------------

module private Expander =

    // -- Helpers --------------------------------------------------------------

    /// Peel any `Stx.Closure` wrappers and look up the head binding in the
    /// appropriate scope.  Returns `None` for non-identifier heads.
    ///
    /// Special forms (`if`, `lambda`, `define`, etc.) are recognised by name
    /// when not overridden in scope, via `StxEnvironment.tryResolveSpecial`.
    let rec resolveHead (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : StxBinding option =
        match stx with
        | StxId(name, _, maybeScope) ->
            match Map.tryFind name (maybeScope |> Option.defaultValue scope) with
            | Some _ as found -> found
            | None -> StxEnvironment.tryResolveSpecial name |> Option.map StxBinding.Special
        | _ -> None

    /// Convert a syntactic datum to its bound-tree counterpart.
    let datumToLiteral =
        function
        | StxDatum.Boolean b -> BoundLiteral.Boolean b
        | StxDatum.Number n -> BoundLiteral.Number n
        | StxDatum.Character c -> BoundLiteral.Character c
        | StxDatum.Str s -> BoundLiteral.Str s
        | StxDatum.ByteVector bs -> BoundLiteral.ByteVector bs

    /// Convert a Stx node to a BoundDatum (for quoted expressions).
    /// Returns None if the Stx tree contains a reader-level error node;
    /// the diagnostic was already emitted by Stx.ofExpr.
    let rec stxToDatum (stx: Stx) : BoundDatum option =
        let mapItems items : BoundDatum list option =
            List.foldBack
                (fun item acc ->
                    match acc, stxToDatum item with
                    | Some ds, Some d -> Some(d :: ds)
                    | _ -> None)
                items
                (Some [])

        match stx with
        | Stx.Id(name, _) -> Some(BoundDatum.Ident name)
        | Stx.Datum(d, _) -> Some(BoundDatum.SelfEval(datumToLiteral d))
        | Stx.List(items, None, _) -> mapItems items |> Option.map BoundDatum.Compound
        | Stx.List(items, Some tail, _) ->
            match mapItems items, stxToDatum tail with
            | Some ds, Some t -> Some(BoundDatum.Pair(ds, t))
            | _ -> None
        | Stx.Closure(inner, _, _) -> stxToDatum inner
        | Stx.Vec(items, _) ->
            mapItems items
            |> Option.map (fun ds -> BoundDatum.SelfEval(BoundLiteral.Vector ds))
        | Stx.Error _ -> None

    /// Parse formal parameters from a Stx node into a `BoundFormals` value.
    let rec parseFormals (stx: Stx) (ctx: ExpandCtx) : BoundFormals =
        match stx with
        | Stx.Id(name, _) -> BoundFormals.Simple name
        | Stx.List(items, None, _) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"

                        None)

            BoundFormals.List names
        | Stx.List(items, Some tail, loc) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"

                        None)

            match tail with
            | StxId(rest, _, _) -> BoundFormals.DottedList(names, rest)
            | _ ->
                ExpandCtx.emitError ctx Diag.invalidFormals loc "expected identifier after dot in formals"

                BoundFormals.List names
        | Stx.Closure(inner, _, _) -> parseFormals inner ctx
        | other ->
            ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "ill-formed formals"
            BoundFormals.List []

    /// Return the flat list of parameter names from a BoundFormals.
    let formalsNames (formals: BoundFormals) =
        match formals with
        | BoundFormals.Simple name -> [ name ]
        | BoundFormals.List names -> names
        | BoundFormals.DottedList(names, rest) -> names @ [ rest ]

    /// Parse a list of `((id init) ...)` binding specs from a Stx node.
    let rec parseBindingSpecs (stx: Stx) (ctx: ExpandCtx) : (string * Stx) list =
        let parseOne node =
            match node with
            | Stx.List([ StxId(name, _, _) ; init ], _, _) -> Some(name, init)
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "ill-formed binding specification"

                None

        match stx with
        | Stx.List(items, _, _) -> items |> List.choose parseOne
        | Stx.Closure(inner, _, _) -> parseBindingSpecs inner ctx
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "expected binding list"
            []

    // -- Variable lookup with capture tracking --------------------------------

    /// Resolve a variable name to its (potentially capture-wrapped) StorageRef.
    /// Uses Idents to distinguish same-named variables from different scopes,
    /// enabling hygienic macro expansion.
    let resolveVar (name: string) (loc: TextLocation) (scope: StxEnvironment) (ctx: ExpandCtx) : StorageRef option =
        match Map.tryFind name scope with
        | Some id ->
            match id with
            | StxBinding.Macro _
            | StxBinding.Special _ ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc $"keyword '{name}' used in value position"
                None
            | StxBinding.Variable id ->
                match Map.tryFind id ctx.BindingMap with
                | Some(Variable(storage, definedAt)) ->
                    if definedAt < ctx.LambdaDepth then
                        Some(ExpandCtx.captureAcrossLambdas storage definedAt ctx)
                    else
                        Some storage
                | None ->
                    ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
                    None
        | None ->
            // Check if the name is a special form used in value position.
            match StxEnvironment.tryResolveSpecial name with
            | Some _ ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc $"keyword '{name}' used in value position"
                None
            | None ->
                ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
                None

    /// Look up a name in the scope, tracking captures for variables from
    /// outer lambda scopes.
    let lookupVar (name: string) (loc: TextLocation) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match resolveVar name loc scope ctx with
        | Some storage -> BoundExpr.Load storage
        | None -> BoundExpr.Error

    // -- Main recursive expander ----------------------------------------------

    /// Expand a single Stx node in the given scope.
    let rec expand (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            expand inner closedScope ctx

        | Stx.Id(name, loc) -> lookupVar name loc scope ctx

        | Stx.Datum(d, _) -> BoundExpr.Literal(datumToLiteral d)

        | Stx.Vec(items, _) ->
            let datums =
                List.foldBack
                    (fun item acc ->
                        match acc, stxToDatum item with
                        | Some ds, Some d -> Some(d :: ds)
                        | _ -> None)
                    items
                    (Some [])

            match datums with
            | Some ds -> BoundExpr.Literal(BoundLiteral.Vector ds)
            | None -> BoundExpr.Error

        | Stx.List(_, Some _, loc) ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "dotted pair in expression position"
            BoundExpr.Error

        | Stx.List([], None, _) -> BoundExpr.Literal BoundLiteral.Null

        | Stx.List(head :: args, None, loc) -> expandForm head args loc scope ctx

        | Stx.Error _ -> BoundExpr.Error

    /// Dispatch a compound form based on what the head resolves to.
    and expandForm
        (head: Stx)
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr =
        match resolveHead head scope ctx with
        | Some(StxBinding.Special kind) -> expandSpecialForm kind args loc scope ctx
        | Some(StxBinding.Macro id) -> expandMacro id (Stx.List(head :: args, None, loc)) scope ctx
        | _ ->
            let fn = expand head scope ctx
            let actuals = args |> List.map (fun a -> expand a scope ctx)
            BoundExpr.Application(fn, actuals)

    /// Expand a special form.
    and expandSpecialForm
        (kind: SpecialFormKind)
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr =
        let illFormed name =
            ExpandCtx.emitError ctx Diag.illFormedForm loc $"ill-formed '{name}'"
            BoundExpr.Error

        match kind with
        | SpecialFormKind.Quote ->
            match args with
            | [ datum ] ->
                match stxToDatum datum with
                | Some d -> BoundExpr.Quoted d
                | None -> BoundExpr.Error
            | _ -> illFormed "quote"

        | SpecialFormKind.If ->
            match args with
            | [ test; thenExpr ] ->
                let b = expand >> (fun f -> f scope ctx)
                BoundExpr.If(b test, b thenExpr, None)
            | [ test; thenExpr; elseExpr ] ->
                let b = expand >> (fun f -> f scope ctx)
                BoundExpr.If(b test, b thenExpr, Some(b elseExpr))
            | _ -> illFormed "if"

        | SpecialFormKind.Begin ->
            let exprs, _ = expandSeq args scope ctx
            BoundExpr.Seq exprs

        | SpecialFormKind.Lambda -> expandLambda args loc scope ctx

        | SpecialFormKind.Define ->
            // define in expression position: env extension is discarded.
            let expr, _ = expandDefine args loc scope ctx
            expr

        | SpecialFormKind.SetBang ->
            match args with
            | [ target; value ] ->
                // Peel any hygiene closures on the target to find its captured
                // scope, then resolve the variable name in that scope.
                //
                // FIXME: This _feels_ like another area where better active patterns could help us.
                let rec resolveSetTarget (stx: Stx) (targetScope: StxEnvironment) =
                    match stx with
                    | Stx.Id(name, idLoc) -> resolveVar name idLoc targetScope ctx
                    | Stx.Closure(inner, innerScope, _) -> resolveSetTarget inner innerScope
                    | _ ->
                        ExpandCtx.emitError ctx Diag.illFormedForm stx.Loc "ill-formed 'set!'"
                        None

                match resolveSetTarget target scope with
                | Some storage -> BoundExpr.Store(storage, Some(expand value scope ctx))
                | None -> BoundExpr.Error
            | _ -> illFormed "set!"

        | SpecialFormKind.Let -> expandLet args loc scope ctx
        | SpecialFormKind.LetStar -> expandLetStar args loc scope ctx
        | SpecialFormKind.Letrec -> expandLetrec args loc scope ctx false
        | SpecialFormKind.LetrecStar -> expandLetrec args loc scope ctx true

        | SpecialFormKind.DefineSyntax ->
            let _, _ = expandDefineSyntax args loc scope ctx
            BoundExpr.Nop

        | SpecialFormKind.LetSyntax -> expandLetSyntax args loc scope ctx false

        | SpecialFormKind.LetrecSyntax -> expandLetSyntax args loc scope ctx true

        | SpecialFormKind.Import ->
            let expr, _ = expandImport args loc scope ctx
            expr

        | SpecialFormKind.DefineLibrary -> fst (expandLibrary args loc scope ctx)

    // -- Lambda ---------------------------------------------------------------

    and expandLambda (args: Stx list) (loc: TextLocation) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | formals :: body when not (List.isEmpty body) ->
            let bFormals = parseFormals formals ctx
            let names = formalsNames bFormals
            let childCtx = ExpandCtx.childForLambda ctx

            // Add formals to childCtx's BindingMap and extend the scope.
            let scopeWithFormals =
                names
                |> List.mapi (fun i name ->
                    let id = Ident.fresh ()

                    childCtx.BindingMap <-
                        Map.add id (Variable(StorageRef.Arg i, childCtx.LambdaDepth)) childCtx.BindingMap

                    name, id)
                |> List.fold (fun s (name, id) -> Map.add name (StxBinding.Variable id) s) scope

            let bodyExprs, _ = expandSeq body scopeWithFormals childCtx
            let bd = ExpandCtx.intoBody childCtx bodyExprs
            BoundExpr.Lambda(bFormals, bd)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'lambda'"
            BoundExpr.Error

    // -- Define ---------------------------------------------------------------

    /// Expand a `define` form, returning the bound expression and the extended
    /// `StxEnvironment` for subsequent sibling forms.
    and expandDefine
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        let illFormed () =
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define'"
            BoundExpr.Error, scope

        match args with
        | [ StxId(name, _, _) ] ->
            // (define id) — uninitialized
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            BoundExpr.Store(storage, None), scope'

        | [ StxId(name, _, _) ; value ] ->
            // (define id value)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            // Name is in scope' so self-recursion works.
            let boundValue = expand value scope' ctx
            BoundExpr.Store(storage, Some boundValue), scope'

        | Stx.List(StxId(name, _, _) :: formals, None, _) :: body ->
            // (define (name formals...) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            let lambdaExpr = expandLambda (Stx.List(formals, None, loc) :: body) loc scope' ctx
            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | Stx.List(StxId(name, _, _) :: formals, Some tail, _) :: body ->
            // (define (name formals... . rest) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope

            let lambdaExpr =
                expandLambda (Stx.List(formals, Some tail, loc) :: body) loc scope' ctx

            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | _ -> illFormed ()

    // -- Let forms ------------------------------------------------------------

    and expandLet (args: Stx list) (loc: TextLocation) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            // Evaluate all inits in the outer scope (parallel let):
            // none of the bindings are visible during init evaluation.
            let stores =
                specs
                |> List.map (fun (name, initStx) ->
                    let initExpr = expand initStx scope ctx
                    let storage = StorageRef.Local(ExpandCtx.nextLocal ctx)
                    let id = Ident.fresh ()
                    name, id, storage, initExpr)

            // Now register all bindings and extend the scope for the body.
            let innerScope =
                stores
                |> List.fold
                    (fun s (name, id, storage, _) ->
                        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

                        Map.add name (StxBinding.Variable id) s)
                    scope

            ctx.ScopeDepth <- ctx.ScopeDepth + 1
            let bodyExprs, _ = expandSeq body innerScope ctx
            ctx.ScopeDepth <- ctx.ScopeDepth - 1
            let storeExprs = stores |> List.map (fun (_, _, s, v) -> BoundExpr.Store(s, Some v))
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let'"
            BoundExpr.Error

    and expandLetStar (args: Stx list) (loc: TextLocation) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            // Each binding is evaluated in the scope extended by all prior bindings.
            let storeExprs, innerScope =
                specs
                |> List.mapFold
                    (fun currentScope (name, initStx) ->
                        let initExpr = expand initStx currentScope ctx
                        let storage = StorageRef.Local(ExpandCtx.nextLocal ctx)
                        let id = Ident.fresh ()
                        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
                        let nextScope = Map.add name (StxBinding.Variable id) currentScope
                        BoundExpr.Store(storage, Some initExpr), nextScope)
                    scope

            ctx.ScopeDepth <- ctx.ScopeDepth + 1
            let bodyExprs, _ = expandSeq body innerScope ctx
            ctx.ScopeDepth <- ctx.ScopeDepth - 1
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let*'"
            BoundExpr.Error

    and expandLetrec
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        (isStar: bool)
        : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            // Pre-allocate all local slots; extend scope with all names first.
            let storages =
                specs
                |> List.map (fun (name, _) ->
                    let storage = StorageRef.Local(ExpandCtx.nextLocal ctx)
                    let id = Ident.fresh ()
                    ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
                    name, id, storage)

            let innerScope =
                storages
                |> List.fold (fun s (name, id, _) -> Map.add name (StxBinding.Variable id) s) scope

            // uninitIds tracks which Idents are still uninitialised.
            // For letrec, all bindings are uninitialised for all init exprs.
            // For letrec*, each binding becomes available after its own init.
            let mutable uninitIds = storages |> List.map (fun (_, id, _) -> id)

            let storeExprs =
                List.map2
                    (fun (_name, _id, storage) (_, initStx) ->
                        // Check the init Stx BEFORE expanding it, so we still
                        // have source locations from the identifier nodes.
                        if not isStar then
                            checkStxForUninitRefs uninitIds initStx innerScope ctx

                        let initExpr = expand initStx innerScope ctx

                        if isStar then
                            uninitIds <- List.tail uninitIds

                        BoundExpr.Store(storage, Some initExpr))
                    storages
                    specs

            ctx.ScopeDepth <- ctx.ScopeDepth + 1
            let bodyExprs, _ = expandSeq body innerScope ctx
            ctx.ScopeDepth <- ctx.ScopeDepth - 1
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'letrec'"
            BoundExpr.Error

    /// Walk a Stx tree checking for references to uninitialised Idents.
    ///
    /// `uninitIds` is the set of Idents that have not yet been initialised.
    /// Skips lambda bodies since those capture the binding reference but are
    /// only invoked after all initialisers have run.
    and checkStxForUninitRefs (uninitIds: Ident list) (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) =
        let rec walk (scope: StxEnvironment) (stx: Stx) =
            match stx with
            | Stx.Id(name, loc) ->
                // Resolve name → Ident in the current scope, check if uninit.
                match Map.tryFind name scope with
                | Some(StxBinding.Variable id) when List.contains id uninitIds ->
                    name
                    |> sprintf "Reference to uninitialised variable '%s' in letrec binding"
                    |> ctx.Diagnostics.Emit Diag.uninitialisedVar loc
                | _ -> ()
            | Stx.List(head :: _ as items, tail, _) ->
                match resolveHead head scope ctx with
                | Some(StxBinding.Special SpecialFormKind.Lambda)
                | Some(StxBinding.Special SpecialFormKind.Quote) ->
                    // References inside a lambda body or a quote form are safe:
                    // lambda bodies run only after all inits; quote is opaque data.
                    ()
                | _ ->
                    List.iter (walk scope) items
                    tail |> Option.iter (walk scope)
            | Stx.List(items, tail, _) ->
                List.iter (walk scope) items
                tail |> Option.iter (walk scope)
            | Stx.Closure(inner, closedScope, _) -> walk closedScope inner
            | Stx.Vec(items, _) -> List.iter (walk scope) items
            | Stx.Datum _ -> ()
            | Stx.Error _ -> ()

        walk scope stx

    // -- define-syntax / let-syntax -------------------------------------------

    and expandDefineSyntax
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        match args with
        | [ StxId(name, _, _) ; rulesStx ] ->
            // Reserve the Ident first so self-referential macros can see their
            // own name in scope during template transcription (e.g. `my-or`).
            let id, scope' = ExpandCtx.reserveMacro name scope

            match parseSyntaxRulesStx name rulesStx scope' ctx with
            | Some transformer ->
                ExpandCtx.registerMacro ctx id transformer
                BoundExpr.Nop, scope'
            | None -> BoundExpr.Error, scope
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-syntax'"
            BoundExpr.Error, scope

    and expandLetSyntax
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        (isLetrec: bool)
        : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            let bodyScope =
                if isLetrec then
                    // letrec-syntax: pre-allocate all macro Idents into the scope
                    // so that each transformer sees the full mutually-recursive set.
                    // Step 1 — reserve: every name gets an Ident in the shared scope.
                    let reserved, letrecScope =
                        specs
                        |> List.mapFold
                            (fun currentScope (name, _) ->
                                let id, nextScope = ExpandCtx.reserveMacro name currentScope
                                (name, id), nextScope)
                            scope

                    // Step 2 — parse each transformer against the fully-extended scope.
                    // Step 3 — register the transformer under the pre-reserved Ident.
                    List.iter2
                        (fun (name, id) (_, ruleStx) ->
                            match parseSyntaxRulesStx name ruleStx letrecScope ctx with
                            | Some transformer -> ExpandCtx.registerMacro ctx id transformer
                            | None -> ())
                        reserved
                        specs

                    letrecScope
                else
                    // let-syntax: parse all transformers against the outer scope
                    // (no forward references between bindings), then reserve+register
                    // for the body scope.
                    specs
                    |> List.fold
                        (fun currentScope (name, ruleStx) ->
                            match parseSyntaxRulesStx name ruleStx scope ctx with
                            | Some transformer ->
                                let id, nextScope = ExpandCtx.reserveMacro name currentScope
                                ExpandCtx.registerMacro ctx id transformer
                                nextScope
                            | None -> currentScope)
                        scope

            // The body scope is local to this call — macro Idents registered
            // here are in ctx.MacroRegistry but reachable only via scope entries
            // that don't escape this function.  ScopeDepth is bumped so that
            // `define` inside the body produces a Local.
            ctx.ScopeDepth <- ctx.ScopeDepth + 1
            let bodyExprs, _ = expandSeq body bodyScope ctx
            ctx.ScopeDepth <- ctx.ScopeDepth - 1
            BoundExpr.Seq bodyExprs
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let-syntax'"
            BoundExpr.Error

    /// Parse a `(syntax-rules ...)` Stx node into a `SyntaxTransformer`.
    and parseSyntaxRulesStx
        (name: string)
        (stx: Stx)
        (defScope: StxEnvironment)
        (ctx: ExpandCtx)
        : SyntaxTransformer option =
        MacrosNew.makeSyntaxTransformer name stx defScope ctx.Diagnostics stx.Loc


    // -- Macro expansion ------------------------------------------------------

    and expandMacro (id: Ident) (form: Stx) (callScope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match ctx.MacroRegistry.TryFind id with
        | None ->
            ExpandCtx.emitError ctx Diag.expandError form.Loc "macro is not yet registered (forward-reference bug)"
            BoundExpr.Error
        | Some transformer ->
            match transformer form callScope with
            | Result.Ok transcribed -> expand transcribed callScope ctx
            | Result.Error msg ->
                ExpandCtx.emitError ctx Diag.expandError form.Loc msg
                BoundExpr.Error


    // -- Import resolution ----------------------------------------------------

    and private expandImportSets
        (importSets: ImportSet list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        let folder (currentScope: StxEnvironment, exprs: BoundExpr list) importSet =
            match resolveImport ctx.Libraries importSet with
            | Ok library ->
                let newScope =
                    library.Exports
                    |> List.fold
                        (fun s (name, storage) ->
                            match storage with
                            | StorageRef.Macro _ ->
                                // Old-format macro entries not usable by the new expander.
                                s
                            | _ ->
                                let id = Ident.fresh ()

                                ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

                                Map.add name (StxBinding.Variable id) s)
                        currentScope

                let mangledName = library.LibraryName |> String.concat "::"
                (newScope, BoundExpr.Import mangledName :: exprs)
            | Result.Error msg ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc msg
                (currentScope, BoundExpr.Error :: exprs)

        let (finalScope, revExprs) = List.fold folder (scope, []) importSets
        (BoundExpr.Seq(List.rev revExprs), finalScope)

    // -- define-library -------------------------------------------------------

    and expandLibrary
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        match args with
        | nameStx :: declarations ->
            match parseLibraryDefinition nameStx declarations with
            | Result.Error diags ->
                diags |> List.iter ctx.Diagnostics.Add
                BoundExpr.Error, scope
            | Ok(libDef, diags) ->
                diags |> List.iter ctx.Diagnostics.Add
                let name = libDef.LibraryName
                let mangledName = name |> String.concat "::"

                let innerCtx = ExpandCtx.childForLibrary ctx mangledName

                let folder (innerScope, importAcc, bodyAcc, exportsAcc) decl =
                    match decl with
                    | LibraryDeclaration.Export exports ->
                        let newExports =
                            exports
                            |> List.choose (function
                                | ExportSet.Plain n -> Some(n, n)
                                | ExportSet.Renamed r -> Some(r.To, r.From))

                        (innerScope, importAcc, bodyAcc, exportsAcc @ newExports)

                    | LibraryDeclaration.Import importSets ->
                        let expr, newScope = expandImportSets importSets loc innerScope innerCtx
                        (newScope, importAcc @ [ expr ], bodyAcc, exportsAcc)

                    | LibraryDeclaration.Begin stxList ->
                        let exprs, newScope = expandSeq stxList innerScope innerCtx
                        (newScope, importAcc, bodyAcc @ exprs, exportsAcc)

                    | LibraryDeclaration.Error -> (innerScope, importAcc, bodyAcc, exportsAcc)

                // Library inner scope: inherited macros only.
                // Special forms are recognised by name in resolveHead and do not
                // need scope entries.  Variable bindings from the outer scope are
                // intentionally excluded; the library must import everything it uses.
                let initInnerScope =
                    scope
                    |> Map.fold
                        (fun acc k binding ->
                            match binding with
                            | StxBinding.Macro _ -> Map.add k binding acc
                            | _ -> acc)
                        Map.empty

                let (finalInnerScope, importExprs, bodyExprs, exportedPairs) =
                    List.fold folder (initInnerScope, [], [], []) libDef.Declarations

                let exports =
                    exportedPairs
                    |> List.choose (fun (extName, intName) ->
                        match Map.tryFind intName finalInnerScope with
                        | Some(StxBinding.Variable id) ->
                            match Map.tryFind id innerCtx.BindingMap with
                            | Some(Variable(storage, _)) -> Some(extName, storage)
                            | None ->
                                ExpandCtx.emitError
                                    ctx
                                    Diag.illFormedForm
                                    loc
                                    $"exported name '{intName}' is not defined in library"

                                None
                        | Some _ ->
                            ExpandCtx.emitError
                                ctx
                                Diag.illFormedForm
                                loc
                                $"'{intName}' is a keyword and cannot be exported"

                            None
                        | None ->
                            ExpandCtx.emitError
                                ctx
                                Diag.illFormedForm
                                loc
                                $"exported name '{intName}' is not defined in library"

                            None)

                ctx.Libraries <-
                    { LibraryName = name
                      Exports = exports }
                    :: ctx.Libraries

                let body = ExpandCtx.intoBody innerCtx (importExprs @ bodyExprs)
                (BoundExpr.Library(name, mangledName, exports, body), scope)

        | [] ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-library'"
            (BoundExpr.Error, scope)

    /// Expand an `import` form.
    and expandImport
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        let importSets = args |> List.map (parseImport ctx.Diagnostics)
        expandImportSets importSets loc scope ctx

    // -- Sequence expansion ---------------------------------------------------

    /// Check whether a Stx node is a definition form.
    and isBindingForm (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : bool =
        match stx with
        | Stx.List(head :: _, _, _) ->
            match resolveHead head scope ctx with
            | Some(StxBinding.Special SpecialFormKind.Define) -> true
            | Some(StxBinding.Special SpecialFormKind.DefineSyntax) -> true
            | Some(StxBinding.Special SpecialFormKind.Begin) -> true
            | Some(StxBinding.Special SpecialFormKind.Import) -> true
            | Some(StxBinding.Special SpecialFormKind.DefineLibrary) -> true
            | _ -> false
        | Stx.Closure(inner, closedScope, _) -> isBindingForm inner closedScope ctx
        | _ -> false

    /// Try to expand a form as a binding-level definition.
    and tryExpandBinding (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : (BoundExpr * StxEnvironment) option =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            // Recurse into the closure's inner scope, then merge only NEW definitions
            // (those not present in closedScope) back into the call-site scope.
            // This prevents the closure's scope from replacing the outer scope while
            // still allowing macro expansions that produce top-level `define`s to
            // splice those definitions into the surrounding context.
            //
            // FIXME: The real fix here is to have a `collectDefs` which walks a
            // sequence of Stx nodes and partitions them into (name * Stx) list 
            // of defines and `Stx list` of the remaining items.
            //
            // In this case we'd _expand_ the macros in each level when we saw
            // them until no macro remained but wouldn't bind the resulting item
            // until we processed the resulting set of defines.
            tryExpandBinding inner closedScope ctx
            |> Option.map (fun (expr, innerScope) ->
                let mergedScope =
                    innerScope
                    |> Map.fold
                        (fun acc name binding ->
                            if Map.containsKey name closedScope then acc
                            else Map.add name binding acc)
                        scope

                expr, mergedScope)
        | Stx.List(head :: args, _, loc) ->
            match resolveHead head scope ctx with
            | Some(StxBinding.Special SpecialFormKind.Define) -> Some(expandDefine args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.DefineSyntax) -> Some(expandDefineSyntax args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.Begin) ->
                let exprs, scope' = expandSeq args scope ctx
                Some(BoundExpr.Seq exprs, scope')
            | Some(StxBinding.Special SpecialFormKind.Import) -> Some(expandImport args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.DefineLibrary) -> Some(expandLibrary args loc scope ctx)
            | Some(StxBinding.Macro id) ->
                // Macro application in definition context: transcribe and re-process
                // so that expansions producing `(define ...)` etc. are spliced in.
                let form = Stx.List(head :: args, None, loc)

                match ctx.MacroRegistry.TryFind id with
                | None ->
                    ExpandCtx.emitError ctx Diag.expandError loc "macro is not yet registered (forward-reference bug)"
                    None
                | Some transformer ->
                    match transformer form scope with
                    | Result.Error msg ->
                        ExpandCtx.emitError ctx Diag.expandError loc msg
                        None
                    | Result.Ok expanded ->
                        match tryExpandBinding expanded scope ctx with
                        | Some result -> Some result
                        | None -> Some(expand expanded scope ctx, scope)
            | _ -> None
        | _ -> None

    /// Expand a list of forms, threading scope through any definitions.
    and expandSeq (stxs: Stx list) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr list * StxEnvironment =
        match stxs with
        | [] -> [], scope
        | stx :: rest ->
            match tryExpandBinding stx scope ctx with
            | Some(boundExpr, scope') ->
                let restExprs, finalScope = expandSeq rest scope' ctx
                boundExpr :: restExprs, finalScope
            | None ->
                let expr = expand stx scope ctx
                let restExprs, finalScope = expandSeq rest scope ctx
                expr :: restExprs, finalScope

// -- Public API ---------------------------------------------------------------

/// Public API for the new binding and expansion pass.
module Expand =

    /// Seed preloaded variable bindings from a `Map<string, StorageRef>` into
    /// an existing `StxEnvironment`, registering each in `ctx.BindingMap`.
    /// Old-format `StorageRef.Macro` entries are silently skipped — callers
    /// should add macros to the base scope via `ExpandCtx.addMacro` before
    /// calling any of the expand functions.
    let private seedPreloaded
        (ctx: ExpandCtx)
        (scope: StxEnvironment)
        (preloaded: Map<string, StorageRef>)
        : StxEnvironment =
        preloaded
        |> Map.fold
            (fun s name storage ->
                match storage with
                | StorageRef.Macro _ -> s // old-format macro refs — skip
                | _ -> ExpandCtx.registerStorage ctx name storage s)
            scope

    /// Expand a parsed program into a list of `BoundExpr` nodes.
    ///
    /// `initialScope` should extend `StxEnvironment.builtin` with any macro
    /// bindings for the compilation unit (via `ExpandCtx.addMacro`).
    /// `preloaded` is a `Map<string, StorageRef>` of library-level variable
    /// bindings to make visible without an explicit `(import ...)` form (e.g.
    /// for Script / REPL mode); pass `Map.empty` for normal program mode.
    let expandProgram
        (prog: Tree.Program)
        (initialScope: StxEnvironment)
        (preloaded: Map<string, StorageRef>)
        (ctx: ExpandCtx)
        : BoundExpr list =
        let scope = seedPreloaded ctx initialScope preloaded
        let docId = prog.DocId

        let stxs =
            prog.Body
            |> Seq.map (Stx.ofExpr ctx.Registry docId ctx.Diagnostics)
            |> List.ofSeq

        let exprs, _ = Expander.expandSeq stxs scope ctx
        exprs

    /// Expand a list of programs, threading scope across units so that
    /// top-level definitions in one unit are visible in subsequent units.
    /// See `expandProgram` for the meaning of `initialScope` and `preloaded`.
    let expandPrograms
        (progs: Tree.Program list)
        (initialScope: StxEnvironment)
        (preloaded: Map<string, StorageRef>)
        (ctx: ExpandCtx)
        : BoundExpr list =
        let startScope = seedPreloaded ctx initialScope preloaded

        let exprs, _ =
            progs
            |> List.mapFold
                (fun scope prog ->
                    let docId = prog.DocId

                    let stxs =
                        prog.Body
                        |> Seq.map (Stx.ofExpr ctx.Registry docId ctx.Diagnostics)
                        |> List.ofSeq

                    let exprs, scope' = Expander.expandSeq stxs scope ctx
                    exprs, scope')
                startScope

        List.concat exprs

    /// Expand a script program (single optional expression) using the new expander.
    /// See `expandProgram` for the meaning of `initialScope` and `preloaded`.
    let expandScript
        (script: Tree.ScriptProgram)
        (initialScope: StxEnvironment)
        (preloaded: Map<string, StorageRef>)
        (ctx: ExpandCtx)
        : BoundExpr list =
        let scope = seedPreloaded ctx initialScope preloaded
        let docId = script.DocId

        let stxs =
            script.Body
            |> Option.map (Stx.ofExpr ctx.Registry docId ctx.Diagnostics)
            |> Option.toList

        let exprs, _ = Expander.expandSeq stxs scope ctx
        exprs
