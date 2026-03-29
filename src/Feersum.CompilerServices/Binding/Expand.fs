namespace Feersum.CompilerServices.NewBindingTest

open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding

// ─────────────────────────────────────────────────────────────── Diagnostics

module private Diag =
    let expandError =
        DiagnosticKind.Create DiagnosticLevel.Error 50 "Expansion error"

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

// ─────────────────────────────────────────────────────────────── Special forms

/// Well-known built-in transformer kinds.
[<RequireQualifiedAccess>]
type SpecialFormKind =
    | If
    | Lambda
    | Define
    | SetBang
    | Begin
    | Quote
    | Let
    | LetStar
    | Letrec
    | LetrecStar
    | DefineSyntax
    | LetSyntax
    | LetrecSyntax
    | Import
    | DefineLibrary

// ─────────────────────────────────────────────────────────────── Syntax env

/// What a name maps to at syntax time.
type SyntaxBinding =
    | SpecialForm of SpecialFormKind
    | MacroDef of SyntaxTransformer
    | Variable of StorageRef

/// A macro transformer: patterns, templates, and definition-site environment.
and SyntaxTransformer =
    { Patterns: (MacroPattern * MacroTemplate) list
      DefEnv: Map<string, SyntaxBinding>
      DefLoc: TextLocation }

/// Immutable map from raw names to their current syntactic meaning.
type SyntaxEnv = Map<string, SyntaxBinding>

// ─────────────────────────────────────────────────────────────────────── Stx

/// The intermediate hygiene-annotated syntax type.
/// Every node carries a source location; `Closure` nodes carry an explicit env.
[<RequireQualifiedAccess>]
type Stx =
    /// An identifier (raw, unresolved name).
    | Id of name: string * loc: TextLocation
    /// A self-evaluating constant.
    | Const of value: BoundLiteral * loc: TextLocation
    /// A proper list — `(e1 e2 … en)`.
    | List of items: Stx list * loc: TextLocation
    /// An improper list — `(e1 e2 … . tail)`.
    | Dotted of items: Stx list * tail: Stx * loc: TextLocation
    /// A quoted datum — `'datum`.
    | Quoted of inner: Stx * loc: TextLocation
    /// A syntactic closure — expand `inner` in `env` regardless of ambient env.
    | Closure of inner: Stx * env: SyntaxEnv * loc: TextLocation

    /// Extract the location from any node.
    member x.Loc =
        match x with
        | Id(_, l)
        | Const(_, l)
        | List(_, l)
        | Dotted(_, _, l)
        | Quoted(_, l)
        | Closure(_, _, l) -> l

/// Convert an `Expression` (Firethorn CST node) into a `Stx` node.
/// Requires a `SourceRegistry` and `DocId` to resolve source locations.
module Stx =

    let rec ofExpr (reg: SourceRegistry) (docId: DocId) (expr: Expression) : Stx =
        let loc = SourceRegistry.resolveLocation reg docId expr.SyntaxRange

        match expr with
        | SymbolNode s -> Stx.Id(s.CookedValue, loc)
        | ConstantNode c -> Stx.Const(BoundLiteral.FromConstantValue c.Value, loc)
        | FormNode f ->
            let body = f.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq

            match f.DottedTail with
            | None -> Stx.List(body, loc)
            | Some tail ->
                let t =
                    tail.Body
                    |> Option.map (ofExpr reg docId)
                    |> Option.defaultValue (Stx.Const(BoundLiteral.Null, loc))

                Stx.Dotted(body, t, loc)
        | QuotedNode q ->
            let inner =
                q.Inner
                |> Option.map (ofExpr reg docId)
                |> Option.defaultValue (Stx.Const(BoundLiteral.Null, loc))

            Stx.Quoted(inner, loc)
        | VecNode v ->
            // Vector literals: represent as a special form for now.
            let items = v.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq
            // We wrap in a Quoted so downstream quotes handle it.
            Stx.List(Stx.Id("vector", loc) :: items, loc)
        | ByteVecNode _ ->
            // Byte vectors passed through as an opaque constant.
            Stx.Const(BoundLiteral.Null, loc)

// ────────────────────────────────────────────────────────────── Pattern types

/// A captured pattern variable: the matched Stx paired with its captured env.
type PatternCapture =
    | Single of stx: Stx * env: SyntaxEnv
    | Repeated of (Stx * SyntaxEnv) list

/// All captured bindings from a successful pattern match.
type PatternBindings = Map<string, PatternCapture>

// ──────────────────────────────────────────────────────────────── ExpandCtx

/// Mutable per-scope state threaded through the expander.
type ExpandCtx =
    { mutable LocalCount: int
      mutable Captures: StorageRef list
      mutable HasDynEnv: bool
      Diagnostics: DiagnosticBag
      Registry: SourceRegistry
      MangledName: string
      IsGlobal: bool
      Parent: ExpandCtx option }

module ExpandCtx =

    /// Create a root context for global-scope expansion.
    let createGlobal (registry: SourceRegistry) (mangledName: string) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          Diagnostics = DiagnosticBag.Empty
          Registry = registry
          MangledName = mangledName
          IsGlobal = true
          Parent = None }

    /// Create a child context for a lambda body.
    let childForLambda (parent: ExpandCtx) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          Diagnostics = parent.Diagnostics
          Registry = parent.Registry
          MangledName = parent.MangledName
          IsGlobal = false
          Parent = Some parent }

    /// Resolve a location for diagnostic emission.
    let resolveLocation (ctx: ExpandCtx) (loc: TextLocation) = loc

    /// Emit an error diagnostic.
    let emitError (ctx: ExpandCtx) (kind: DiagnosticKind) (loc: TextLocation) (msg: string) =
        ctx.Diagnostics.Emit kind loc msg

    /// Allocate the next local slot index.
    let nextLocal (ctx: ExpandCtx) =
        let idx = ctx.LocalCount
        ctx.LocalCount <- idx + 1
        idx

    /// Mint a new storage reference for a fresh binding.
    let mintStorage (ctx: ExpandCtx) (name: string) : StorageRef =
        if ctx.IsGlobal then
            StorageRef.Global(ctx.MangledName, Field name)
        else
            StorageRef.Local(nextLocal ctx)

    /// Convert a context and list of body expressions into a `BoundBody`.
    let intoBody (ctx: ExpandCtx) (exprs: BoundExpr list) : BoundBody =
        let env = if ctx.HasDynEnv then Some [] else None

        { Body = BoundExpr.Seq exprs
          Locals = ctx.LocalCount
          Captures = ctx.Captures
          EnvMappings = env }

    /// Register a capture of an outer storage reference.
    let capture (ctx: ExpandCtx) (outer: StorageRef) : StorageRef =
        ctx.Captures <- outer :: ctx.Captures
        ctx.HasDynEnv <- true
        StorageRef.Captured outer

// ──────────────────────────────────────────────────────────── Built-in env

module SyntaxEnv =

    /// The initial syntax environment populated with all built-in special form
    /// keywords. Callers extend this with global variable bindings before
    /// calling `Expand.expandProgram`.
    let builtin: SyntaxEnv =
        [ "if", SpecialForm SpecialFormKind.If
          "lambda", SpecialForm SpecialFormKind.Lambda
          "define", SpecialForm SpecialFormKind.Define
          "set!", SpecialForm SpecialFormKind.SetBang
          "begin", SpecialForm SpecialFormKind.Begin
          "quote", SpecialForm SpecialFormKind.Quote
          "let", SpecialForm SpecialFormKind.Let
          "let*", SpecialForm SpecialFormKind.LetStar
          "letrec", SpecialForm SpecialFormKind.Letrec
          "letrec*", SpecialForm SpecialFormKind.LetrecStar
          "define-syntax", SpecialForm SpecialFormKind.DefineSyntax
          "let-syntax", SpecialForm SpecialFormKind.LetSyntax
          "letrec-syntax", SpecialForm SpecialFormKind.LetrecSyntax
          "define-library", SpecialForm SpecialFormKind.DefineLibrary
          "import", SpecialForm SpecialFormKind.Import ]
        |> Map.ofList

// ─────────────────────────────────────────────────────────────── Expander

module private Expander =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Peel any `Stx.Closure` wrappers and look up the head binding in the
    /// appropriate environment.  Returns `None` for non-identifier heads.
    let rec resolveHead (stx: Stx) (env: SyntaxEnv) : SyntaxBinding option =
        match stx with
        | Stx.Id(name, _) -> Map.tryFind name env
        | Stx.Closure(inner, closedEnv, _) -> resolveHead inner closedEnv
        | _ -> None

    /// Convert a Stx node to a BoundDatum (for quoted expressions).
    let rec stxToDatum (stx: Stx) : BoundDatum =
        match stx with
        | Stx.Id(name, _) -> BoundDatum.Ident name
        | Stx.Const(lit, _) -> BoundDatum.SelfEval lit
        | Stx.List(items, _) -> BoundDatum.Compound(items |> List.map stxToDatum)
        | Stx.Dotted(items, tail, _) -> BoundDatum.Pair(items |> List.map stxToDatum, stxToDatum tail)
        | Stx.Quoted(inner, _) -> BoundDatum.Quoted(stxToDatum inner)
        | Stx.Closure(inner, _, _) -> stxToDatum inner

    /// Parse formal parameters from a Stx node into a `BoundFormals` value.
    let rec parseFormals (stx: Stx) (ctx: ExpandCtx) : BoundFormals =
        match stx with
        | Stx.Id(name, _) -> BoundFormals.Simple name
        | Stx.List(items, _) ->
            let names =
                items
                |> List.choose (function
                    | Stx.Id(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"
                        None)

            BoundFormals.List names
        | Stx.Dotted(items, tail, loc) ->
            let names =
                items
                |> List.choose (function
                    | Stx.Id(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"
                        None)

            match tail with
            | Stx.Id(rest, _) -> BoundFormals.DottedList(names, rest)
            | other ->
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
            | Stx.List([ Stx.Id(name, _); init ], _) -> Some(name, init)
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "ill-formed binding specification"
                None

        match stx with
        | Stx.List(items, _) -> items |> List.choose parseOne
        | Stx.Closure(inner, _, _) -> parseBindingSpecs inner ctx
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "expected binding list"
            []

    // ── Variable lookup with capture tracking ─────────────────────────────

    /// Look up a name in the syntax environment, tracking captures for
    /// variables that come from outer lambda scopes.
    let lookupVar (name: string) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match Map.tryFind name env with
        | Some(Variable storage) ->
            // Detect capture: if this storage belongs to a parent lambda scope
            // it must be tracked.  For now we propagate the storage directly;
            // capture analysis is handled by Lower.fs as before.
            BoundExpr.Load storage
        | Some(SpecialForm _) | Some(MacroDef _) ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc $"keyword '{name}' used in value position"
            BoundExpr.Error
        | None ->
            ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
            BoundExpr.Error

    // ── Main recursive expander ───────────────────────────────────────────

    /// Expand a single Stx node in the given environment.
    let rec expand (stx: Stx) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match stx with
        | Stx.Closure(inner, closedEnv, _) ->
            // Syntactic closure: switch to the enclosed environment.
            expand inner closedEnv ctx

        | Stx.Id(name, loc) -> lookupVar name loc env ctx

        | Stx.Const(lit, _) -> BoundExpr.Literal lit

        | Stx.Quoted(inner, _) -> BoundExpr.Quoted(stxToDatum inner)

        | Stx.Dotted(_, _, loc) ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "dotted pair in expression position"
            BoundExpr.Error

        | Stx.List([], loc) -> BoundExpr.Literal BoundLiteral.Null

        | Stx.List(head :: args, loc) -> expandForm head args loc env ctx

    /// Dispatch a compound form based on what the head resolves to.
    and expandForm (head: Stx) (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match resolveHead head env with
        | Some(SpecialForm kind) -> expandSpecialForm kind args loc env ctx
        | Some(MacroDef macro) -> expandMacro macro (Stx.List(head :: args, loc)) env ctx
        | _ ->
            let fn = expand head env ctx
            let actuals = args |> List.map (fun a -> expand a env ctx)
            BoundExpr.Application(fn, actuals)

    /// Expand a special form.
    and expandSpecialForm (kind: SpecialFormKind) (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        let illFormed name =
            ExpandCtx.emitError ctx Diag.illFormedForm loc $"ill-formed '{name}'"
            BoundExpr.Error

        match kind with
        | SpecialFormKind.Quote ->
            match args with
            | [ datum ] -> BoundExpr.Quoted(stxToDatum datum)
            | _ -> illFormed "quote"

        | SpecialFormKind.If ->
            match args with
            | [ test; thenExpr ] ->
                let b = expand >> (fun f -> f env ctx)
                BoundExpr.If(b test, b thenExpr, None)
            | [ test; thenExpr; elseExpr ] ->
                let b = expand >> (fun f -> f env ctx)
                BoundExpr.If(b test, b thenExpr, Some(b elseExpr))
            | _ -> illFormed "if"

        | SpecialFormKind.Begin ->
            let exprs, _ = expandSeq args env ctx
            BoundExpr.Seq exprs

        | SpecialFormKind.Lambda -> expandLambda args loc env ctx

        | SpecialFormKind.Define ->
            // define in expression position is treated as a top-level define;
            // the resulting env extension is discarded.
            let expr, _ = expandDefine args loc env ctx
            expr

        | SpecialFormKind.SetBang ->
            match args with
            | [ Stx.Id(name, idLoc); value ] ->
                match Map.tryFind name env with
                | Some(Variable storage) -> BoundExpr.Store(storage, Some(expand value env ctx))
                | _ ->
                    ExpandCtx.emitError ctx Diag.undefinedSymbol idLoc $"set! to unbound or non-variable identifier '{name}'"
                    BoundExpr.Error
            | _ -> illFormed "set!"

        | SpecialFormKind.Let -> expandLet args loc env ctx
        | SpecialFormKind.LetStar -> expandLetStar args loc env ctx
        | SpecialFormKind.Letrec -> expandLetrec args loc env ctx false
        | SpecialFormKind.LetrecStar -> expandLetrec args loc env ctx true

        | SpecialFormKind.DefineSyntax ->
            let _, _ = expandDefineSyntax args loc env ctx
            BoundExpr.Nop

        | SpecialFormKind.LetSyntax ->
            expandLetSyntax args loc env ctx false

        | SpecialFormKind.LetrecSyntax ->
            expandLetSyntax args loc env ctx true

        | SpecialFormKind.Import | SpecialFormKind.DefineLibrary ->
            // Not yet implemented in isolation; emit a stub.
            ExpandCtx.emitError ctx Diag.illFormedForm loc "import/define-library not yet supported in new expander"
            BoundExpr.Error

    // ── Lambda ───────────────────────────────────────────────────────────

    and expandLambda (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | formals :: body when not (List.isEmpty body) ->
            let bFormals = parseFormals formals ctx
            let names = formalsNames bFormals
            let childCtx = ExpandCtx.childForLambda ctx

            // Extend env with argument bindings (Arg i).
            let envWithFormals =
                names
                |> List.mapi (fun i name -> name, Variable(StorageRef.Arg i))
                |> List.fold (fun e (k, v) -> Map.add k v e) env

            let bodyExprs, _ = expandSeq body envWithFormals childCtx
            let bd = ExpandCtx.intoBody childCtx bodyExprs
            BoundExpr.Lambda(bFormals, bd)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'lambda'"
            BoundExpr.Error

    // ── Define ───────────────────────────────────────────────────────────

    /// Expand a `define` form, returning the bound expression and the extended
    /// `SyntaxEnv` for subsequent sibling forms.
    and expandDefine (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr * SyntaxEnv =
        let illFormed () =
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define'"
            BoundExpr.Error, env

        match args with
        | [ Stx.Id(name, _) ] ->
            // (define id) — uninitialized
            let storage = ExpandCtx.mintStorage ctx name
            let env' = Map.add name (Variable storage) env
            BoundExpr.Store(storage, None), env'

        | [ Stx.Id(name, _); value ] ->
            // (define id value)
            let storage = ExpandCtx.mintStorage ctx name
            // Add name to env before expanding value so self-recursion works.
            let env' = Map.add name (Variable storage) env
            let boundValue = expand value env' ctx
            BoundExpr.Store(storage, Some boundValue), env'

        | Stx.List(Stx.Id(name, _) :: formals, _) :: body ->
            // (define (name formals...) body...)
            let storage = ExpandCtx.mintStorage ctx name
            let env' = Map.add name (Variable storage) env
            let lambdaExpr = expandLambda (Stx.List(formals, loc) :: body) loc env' ctx
            BoundExpr.Store(storage, Some lambdaExpr), env'

        | Stx.Dotted(Stx.Id(name, _) :: formals, tail, _) :: body ->
            // (define (name formals... . rest) body...)
            let storage = ExpandCtx.mintStorage ctx name
            let env' = Map.add name (Variable storage) env
            let lambdaExpr = expandLambda (Stx.Dotted(formals, tail, loc) :: body) loc env' ctx
            BoundExpr.Store(storage, Some lambdaExpr), env'

        | _ -> illFormed ()

    // ── Let forms ────────────────────────────────────────────────────────

    and expandLet (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx
            // Evaluate all inits in the outer environment (parallel let).
            let stores =
                specs
                |> List.map (fun (name, initStx) ->
                    let initExpr = expand initStx env ctx
                    let storage = StorageRef.Local(ExpandCtx.nextLocal ctx)
                    name, storage, initExpr)

            // Extend env with all new bindings.
            let innerEnv =
                stores
                |> List.fold (fun e (name, storage, _) -> Map.add name (Variable storage) e) env

            let bodyExprs, _ = expandSeq body innerEnv ctx
            let storeExprs = stores |> List.map (fun (_, s, v) -> BoundExpr.Store(s, Some v))
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let'"
            BoundExpr.Error

    and expandLetStar (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx
            // Each binding is evaluated in the env extended by all prior bindings.
            let storeExprs, innerEnv =
                specs
                |> List.mapFold
                    (fun currentEnv (name, initStx) ->
                        let initExpr = expand initStx currentEnv ctx
                        let storage = StorageRef.Local(ExpandCtx.nextLocal ctx)
                        let nextEnv = Map.add name (Variable storage) currentEnv
                        BoundExpr.Store(storage, Some initExpr), nextEnv)
                    env

            let bodyExprs, _ = expandSeq body innerEnv ctx
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let*'"
            BoundExpr.Error

    and expandLetrec (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) (isStar: bool) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            // Pre-allocate all local slots; extend env with all names first.
            let storages =
                specs
                |> List.map (fun (name, _) ->
                    name, StorageRef.Local(ExpandCtx.nextLocal ctx))

            let innerEnv =
                storages
                |> List.fold (fun e (name, storage) -> Map.add name (Variable storage) e) env

            // Track uninitialized names for letrec (not letrec*) checking.
            let mutable uninitNames = storages |> List.map fst

            let storeExprs =
                List.map2
                    (fun (name, storage) (_, initStx) ->
                        let initExpr = expand initStx innerEnv ctx

                        if not isStar then
                            // Check for use of still-uninitialized bindings.
                            checkUninitialised uninitNames initExpr loc ctx

                        if isStar then
                            uninitNames <- List.tail uninitNames

                        BoundExpr.Store(storage, Some initExpr))
                    storages
                    specs

            let bodyExprs, _ = expandSeq body innerEnv ctx
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'letrec'"
            BoundExpr.Error

    /// Check a BoundExpr for loads from any uninitialised storage slots.
    and checkUninitialised (uninitNames: string list) (expr: BoundExpr) (loc: TextLocation) (ctx: ExpandCtx) =
        let rec check =
            function
            | BoundExpr.Load s -> warnIfUninit s
            | BoundExpr.Store(_, Some v) -> check v
            | BoundExpr.Application(fn, args) ->
                check fn
                List.iter check args
            | BoundExpr.If(cond, t, f) ->
                check cond
                check t
                Option.iter check f
            | BoundExpr.Seq exprs -> List.iter check exprs
            | BoundExpr.SequencePoint(inner, _) -> check inner
            | BoundExpr.Lambda _ -> ()
            | _ -> ()

        and warnIfUninit storage =
            let idx =
                uninitNames
                |> List.tryFindIndex (fun name ->
                    match storage with
                    | StorageRef.Local _ ->
                        // We'd need to correlate name->storage here; for now accept.
                        false
                    | _ -> false)

            ()

        check expr

    // ── define-syntax / let-syntax ───────────────────────────────────────

    and expandDefineSyntax (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr * SyntaxEnv =
        match args with
        | [ Stx.Id(name, _); rulesStx ] ->
            match parseSyntaxRulesStx name rulesStx env ctx with
            | Some transformer ->
                let env' = Map.add name (MacroDef transformer) env
                BoundExpr.Nop, env'
            | None -> BoundExpr.Error, env
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-syntax'"
            BoundExpr.Error, env

    and expandLetSyntax (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) (_isLetrec: bool) : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            let extendedEnv =
                specs
                |> List.fold
                    (fun currentEnv (name, ruleStx) ->
                        match parseSyntaxRulesStx name ruleStx currentEnv ctx with
                        | Some transformer -> Map.add name (MacroDef transformer) currentEnv
                        | None -> currentEnv)
                    env

            let bodyExprs, _ = expandSeq body extendedEnv ctx
            BoundExpr.Seq bodyExprs
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let-syntax'"
            BoundExpr.Error

    /// Parse a `(syntax-rules ...)` Stx node into a `SyntaxTransformer`.
    /// Returns `None` on error after emitting a diagnostic.
    and parseSyntaxRulesStx (name: string) (stx: Stx) (defEnv: SyntaxEnv) (ctx: ExpandCtx) : SyntaxTransformer option =
        // Delegate to the existing Expression-based parser by re-interpreting
        // the Stx as a synthetic Expression.  This is a temporary bridge until
        // the new Stx-native pattern/template engine is complete.
        //
        // For now, emit a "not yet implemented" diagnostic and return None.
        ExpandCtx.emitError ctx Diag.illFormedForm stx.Loc "define-syntax not yet implemented in new expander"
        None

    // ── Macro expansion ───────────────────────────────────────────────────

    and expandMacro (macro: SyntaxTransformer) (form: Stx) (callEnv: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
        // Match the call form against each rule in order.
        let result =
            macro.Patterns
            |> List.tryPick (fun (pat, tmpl) ->
                matchPattern pat form callEnv |> Option.map (fun b -> tmpl, b))

        match result with
        | None ->
            ExpandCtx.emitError ctx Diag.expandError form.Loc "no macro rule matched the form"
            BoundExpr.Error
        | Some(template, bindings) ->
            // Transcribe the template into a new Stx tree.
            let transcribed = transcribe template bindings macro.DefEnv form.Loc
            // Expand the resulting Stx in the call-site environment;
            // Stx.Closure nodes inside will override it selectively.
            expand transcribed callEnv ctx

    // ── Pattern matching ──────────────────────────────────────────────────

    /// Attempt to match a MacroPattern against a Stx node.
    /// Returns Some PatternBindings on success, None on mismatch.
    and matchPattern (pat: MacroPattern) (stx: Stx) (env: SyntaxEnv) : PatternBindings option =
        match pat, stx with
        | MacroPattern.Underscore, _ -> Some Map.empty

        | MacroPattern.Variable v, s ->
            Some(Map.ofList [ v, Single(s, env) ])

        | MacroPattern.Literal lit, Stx.Id(name, _) ->
            if name = lit then Some Map.empty else None

        | MacroPattern.Literal lit, Stx.Closure(inner, closedEnv, _) ->
            matchPattern (MacroPattern.Literal lit) inner closedEnv

        | MacroPattern.Constant c, Stx.Const(lit, _) ->
            if constantMatchesLiteral c lit then Some Map.empty else None

        | MacroPattern.Form pats, Stx.List(items, _) ->
            matchPatternList pats None items None env

        | MacroPattern.DottedForm(pats, tailPat), Stx.Dotted(items, tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) env

        | MacroPattern.Form pats, Stx.Closure(inner, closedEnv, _) ->
            matchPattern (MacroPattern.Form pats) inner closedEnv

        | MacroPattern.DottedForm(pats, tailPat), Stx.Closure(inner, closedEnv, _) ->
            matchPattern (MacroPattern.DottedForm(pats, tailPat)) inner closedEnv

        | _ -> None

    and matchPatternList
        (pats: MacroPattern list)
        (tailPat: MacroPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (env: SyntaxEnv)
        : PatternBindings option =
        match pats with
        | MacroPattern.Repeat inner :: restPats ->
            matchRepeat inner restPats tailPat items tailItem env []

        | headPat :: restPats ->
            match items with
            | headItem :: restItems ->
                matchPattern headPat headItem env
                |> Option.bind (fun b1 ->
                    matchPatternList restPats tailPat restItems tailItem env
                    |> Option.map (mergeBindings b1))
            | [] -> None

        | [] ->
            match tailPat with
            | Some tp ->
                match tailItem with
                | Some t -> matchPattern tp t env
                | None ->
                    match items with
                    | [ single ] -> matchPattern tp single env
                    | _ -> None
            | None -> if List.isEmpty items && tailItem.IsNone then Some Map.empty else None

    and matchRepeat
        (inner: MacroPattern)
        (restPats: MacroPattern list)
        (tailPat: MacroPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (env: SyntaxEnv)
        (accumulated: PatternBindings list)
        : PatternBindings option =
        // Try to match the tail patterns first (backtracking).
        match matchPatternList restPats tailPat items tailItem env with
        | Some tailBindings ->
            // Merge accumulated repeated captures.
            let repeatedBindings = gatherRepeated accumulated
            Some(mergeBindings tailBindings repeatedBindings)
        | None ->
            match items with
            | head :: rest ->
                matchPattern inner head env
                |> Option.bind (fun b ->
                    matchRepeat inner restPats tailPat rest tailItem env (b :: accumulated))
            | [] -> None

    /// Gather a list of per-repetition PatternBindings into a single
    /// PatternBindings where each entry is a Repeated capture.
    and gatherRepeated (perRep: PatternBindings list) : PatternBindings =
        perRep
        |> List.rev
        |> List.fold
            (fun acc bindings ->
                Map.fold
                    (fun m k v ->
                        let prev =
                            match Map.tryFind k m with
                            | Some(Repeated lst) -> lst
                            | _ -> []

                        let item =
                            match v with
                            | Single(s, e) -> (s, e)
                            | Repeated _ -> failwith "nested ellipsis not supported"

                        Map.add k (Repeated(prev @ [ item ])) m)
                    acc
                    bindings)
            Map.empty

    and mergeBindings (left: PatternBindings) (right: PatternBindings) : PatternBindings =
        Map.fold (fun m k v -> Map.add k v m) left right

    and constantMatchesLiteral (pat: ConstantValue) (lit: BoundLiteral) : bool =
        match pat, lit with
        | NumVal n, BoundLiteral.Number m -> n = m
        | StrVal s, BoundLiteral.Str t -> s = t
        | BoolVal b, BoundLiteral.Boolean c -> b = c
        | CharVal c, BoundLiteral.Character d -> c = Some d
        | _ -> false

    // ── Transcription ─────────────────────────────────────────────────────

    and transcribe
        (template: MacroTemplate)
        (bindings: PatternBindings)
        (defEnv: SyntaxEnv)
        (loc: TextLocation)
        : Stx =
        match template with
        | MacroTemplate.Subst name ->
            match Map.tryFind name bindings with
            | Some(Single(stx, capturedEnv)) -> Stx.Closure(stx, capturedEnv, loc)
            | Some(Repeated _) -> failwith $"ellipsis variable '{name}' used outside a repeat context"
            | None -> failwith $"template variable '{name}' not bound"

        | MacroTemplate.Quoted expr ->
            // Template literal — a raw Expression from the original template.
            // Wrap it as an opaque constant datum rather than re-expanding.
            // A more faithful implementation would convert via Stx.ofExpr.
            Stx.Const(BoundLiteral.Null, loc)

        | MacroTemplate.Form(_, elements) ->
            let children = elements |> List.collect (transcribeElement bindings defEnv loc)
            Stx.List(children, loc)

        | MacroTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defEnv loc)
            Stx.Dotted(children, transcribe tail bindings defEnv loc, loc)

    and transcribeElement
        (bindings: PatternBindings)
        (defEnv: SyntaxEnv)
        (loc: TextLocation)
        (elem: MacroTemplateElement)
        : Stx list =
        match elem with
        | MacroTemplateElement.Template t -> [ transcribe t bindings defEnv loc ]
        | MacroTemplateElement.Repeated t -> transcribeRepeated t bindings defEnv loc

    and transcribeRepeated
        (template: MacroTemplate)
        (bindings: PatternBindings)
        (defEnv: SyntaxEnv)
        (loc: TextLocation)
        : Stx list =
        // Find the first Repeated capture to determine repetition count.
        let repCount =
            bindings
            |> Map.tryPick (fun _ v ->
                match v with
                | Repeated lst -> Some(List.length lst)
                | Single _ -> None)
            |> Option.defaultValue 0

        [ 0 .. repCount - 1 ]
        |> List.map (fun i ->
            // Build a per-repetition binding map substituting the i-th element.
            let perRepBindings =
                Map.map
                    (fun _ v ->
                        match v with
                        | Repeated lst -> Single(lst.[i])
                        | Single _ as s -> s)
                    bindings

            transcribe template perRepBindings defEnv loc)

    // ── Sequence expansion ────────────────────────────────────────────────

    /// Check whether a Stx node is a `define` or `define-syntax` at the head,
    /// possibly wrapped in `Stx.Closure` layers.
    and isBindingForm (stx: Stx) (env: SyntaxEnv) : bool =
        match stx with
        | Stx.List(head :: _, _) ->
            match resolveHead head env with
            | Some(SpecialForm SpecialFormKind.Define) -> true
            | Some(SpecialForm SpecialFormKind.DefineSyntax) -> true
            | Some(SpecialForm SpecialFormKind.Begin) -> true
            | _ -> false
        | Stx.Closure(inner, closedEnv, _) -> isBindingForm inner closedEnv
        | _ -> false

    /// Try to expand a form as a binding-level definition.
    /// Returns `Some(expr, extendedEnv)` for `define` / `define-syntax`,
    /// `None` for all other forms.
    and tryExpandBinding (stx: Stx) (env: SyntaxEnv) (ctx: ExpandCtx) : (BoundExpr * SyntaxEnv) option =
        match stx with
        | Stx.Closure(inner, closedEnv, _) ->
            tryExpandBinding inner closedEnv ctx
        | Stx.List(head :: args, loc) ->
            match resolveHead head env with
            | Some(SpecialForm SpecialFormKind.Define) ->
                Some(expandDefine args loc env ctx)
            | Some(SpecialForm SpecialFormKind.DefineSyntax) ->
                Some(expandDefineSyntax args loc env ctx)
            | Some(SpecialForm SpecialFormKind.Begin) ->
                // Splice begin forms in definition context.
                let exprs, env' = expandSeq args env ctx
                Some(BoundExpr.Seq exprs, env')
            | _ -> None
        | _ -> None

    /// Expand a list of forms, threading env through any definitions.
    /// Returns the list of BoundExprs and the final SyntaxEnv.
    and expandSeq (stxs: Stx list) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr list * SyntaxEnv =
        match stxs with
        | [] -> [], env
        | stx :: rest ->
            match tryExpandBinding stx env ctx with
            | Some(boundExpr, env') ->
                let restExprs, finalEnv = expandSeq rest env' ctx
                boundExpr :: restExprs, finalEnv
            | None ->
                let expr = expand stx env ctx
                let restExprs, finalEnv = expandSeq rest env ctx
                expr :: restExprs, finalEnv

// ─────────────────────────────────────────────────── Public API

/// Public API for the new binding and expansion pass.
module Expand =

    /// Expand a parsed program into a list of `BoundExpr` nodes.
    ///
    /// `initialEnv` should extend `builtinEnv` with global variable and library
    /// bindings for the compilation unit.
    let expandProgram (prog: Tree.Program) (initialEnv: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr list =
        let docId = prog.DocId

        let stxs =
            prog.Body
            |> Seq.map (Stx.ofExpr ctx.Registry docId)
            |> List.ofSeq

        let exprs, _ = Expander.expandSeq stxs initialEnv ctx
        exprs
