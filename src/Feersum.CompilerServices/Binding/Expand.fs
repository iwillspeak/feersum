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

    let invalidMacro =
        DiagnosticKind.Create DiagnosticLevel.Error 56 "Invalid macro definition"

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

// ──────────────────────────────────── BindingId and SyntaxScope

/// A globally unique identity for a single binding introduction.
/// Variables with the same name but introduced at different points get
/// different BindingIds, enabling hygienic expansion without gensyms.
[<Struct>]
type BindingId = private BindingId of int

module BindingId =
    let mutable private counter = 0

    let fresh () =
        let id = counter
        counter <- id + 1
        BindingId id

/// Immutable name → BindingId mapping threaded through all expand functions.
/// Syntactic closures carry a snapshot of the scope at their definition site.
type SyntaxScope = Map<string, BindingId>

// ──────────────────────────────────── Syntax pattern types

/// Pattern element for matching `Stx` syntax in macro rules.
/// `Constant` carries a `BoundLiteral` (the compiled literal value) rather than
/// a raw CST `ConstantValue`, making it independent of the parse-tree layer.
[<RequireQualifiedAccess>]
type StxPattern =
    /// Match a self-evaluating literal exactly.
    | Constant of BoundLiteral
    /// Match anything (`_`).
    | Underscore
    /// Match a literal keyword (must appear in the `syntax-rules` literals list).
    | Literal of string
    /// Capture a single sub-form into a pattern variable.
    | Variable of string
    /// Match zero-or-more repetitions of the sub-pattern (the `...` operator).
    | Repeat of StxPattern
    /// Match a proper list.
    | Form of StxPattern list
    /// Match an improper list.
    | DottedForm of StxPattern list * StxPattern
    /// Match a vector literal `#(...)`.
    | Vec of StxPattern list

// ──────────────────────────────────── Stx (defined before StxTemplate so it can be referenced)

/// The intermediate hygiene-annotated syntax type.
/// Every node carries a source location; `Closure` nodes carry the definition-site scope.
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
    /// A syntactic closure — expand `inner` in `scope` rather than the ambient scope.
    | Closure of inner: Stx * scope: SyntaxScope * loc: TextLocation
    /// A vector literal — `#(e1 e2 … en)`.
    | Vec of items: Stx list * loc: TextLocation
    /// A byte-vector literal — `#u8(b1 b2 … bn)`.
    | ByteVec of bytes: Result<byte, string> list * loc: TextLocation

    member x.Loc =
        match x with
        | Id(_, l)
        | Const(_, l)
        | List(_, l)
        | Dotted(_, _, l)
        | Quoted(_, l)
        | Closure(_, _, l)
        | Vec(_, l)
        | ByteVec(_, l) -> l

// ──────────────────────────────────── Template types (can now reference Stx)

/// A macro template element — either a single sub-template or a repeated one.
[<RequireQualifiedAccess>]
type StxTemplateElement =
    | Template of StxTemplate
    | Repeated of StxTemplate

/// Template for producing new `Stx` syntax in macro rules.
/// `Quoted` carries the literal `Stx` node captured at definition time; on
/// transcription it is wrapped in a `Stx.Closure` with the definition-site scope.
and [<RequireQualifiedAccess>] StxTemplate =
    /// A literal node from the definition site (wrapped in closure for hygiene).
    | Quoted of stx: Stx
    /// Substitute the value bound to a pattern variable.
    | Subst of name: string
    /// Produce a proper list.
    | Form of loc: TextLocation * elements: StxTemplateElement list
    /// Produce an improper list.
    | DottedForm of elements: StxTemplateElement list * tail: StxTemplate
    /// Produce a vector literal.
    | Vec of elements: StxTemplateElement list

// ──────────────────────────────────── Pattern capture types

/// A captured pattern variable: the matched Stx paired with the scope at capture time.
type PatternCapture =
    | Single of stx: Stx * scope: SyntaxScope
    | Repeated of (Stx * SyntaxScope) list

/// All captured bindings from a successful pattern match.
type PatternBindings = Map<string, PatternCapture>

// ──────────────────────────────────── Convert CST Expression to Stx

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
            let items = v.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq
            Stx.Vec(items, loc)
        | ByteVecNode b ->
            let bytes = b.Body |> List.map (fun bv -> bv.Value)
            Stx.ByteVec(bytes, loc)

// ──────────────────────────────────── Transformer and BindingMeaning types

/// A macro transformer: patterns, templates, and definition-site scope.
type SyntaxTransformer =
    { Patterns: (StxPattern * StxTemplate) list
      DefScope: SyntaxScope
      DefLoc: TextLocation }

/// What a BindingId resolves to at compile time.
type BindingMeaning =
    | SpecialForm of SpecialFormKind
    | MacroDef of SyntaxTransformer
    | Variable of StorageRef * lambdaDepth: int

/// BindingId → its compile-time meaning.
type BindingMap = Map<BindingId, BindingMeaning>

// ──────────────────────────────────────────────────────────────── ExpandCtx

/// Mutable per-lambda state threaded through the expander.
type ExpandCtx =
    { mutable LocalCount: int
      mutable Captures: StorageRef list
      mutable HasDynEnv: bool
      /// Nesting depth of lambda frames. 0 = global/library scope.
      /// Incremented in `childForLambda`. Used to detect cross-lambda captures
      /// via comparison with `Variable(_, lambdaDepth)` in BindingMap.
      LambdaDepth: int
      /// Mutable map from BindingId to its compile-time meaning.
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
      Parent: ExpandCtx option }

module ExpandCtx =

    // Forward-declared mutable so SyntaxScope module can populate it after definition.
    let mutable private _builtinBindingMap: BindingMap = Map.empty

    let internal setBuiltinBindingMap (m: BindingMap) =
        _builtinBindingMap <- m

    /// Create a root context for global-scope expansion.
    let createGlobal
        (registry: SourceRegistry)
        (mangledName: string)
        (libraries: LibrarySignature<StorageRef> list)
        =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          LambdaDepth = 0
          BindingMap = _builtinBindingMap
          ScopeDepth = 0
          Diagnostics = DiagnosticBag.Empty
          Registry = registry
          MangledName = mangledName
          IsGlobal = true
          Libraries = libraries
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
    let rec captureAcrossLambdas
        (storage: StorageRef)
        (definedAt: int)
        (ctx: ExpandCtx)
        : StorageRef =
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

    /// Introduce a new variable binding: fresh BindingId, storage allocation,
    /// register in ctx.BindingMap, return (id, storage, extended scope).
    let mintVar
        (ctx: ExpandCtx)
        (name: string)
        (scope: SyntaxScope)
        : BindingId * StorageRef * SyntaxScope =
        let id = BindingId.fresh ()
        let storage = mintStorage ctx name
        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
        id, storage, Map.add name id scope

    /// Register a macro (syntax transformer) in ctx.BindingMap and return the
    /// extended scope with the macro name mapped to its fresh BindingId.
    let addMacro
        (ctx: ExpandCtx)
        (name: string)
        (transformer: SyntaxTransformer)
        (scope: SyntaxScope)
        : SyntaxScope =
        let id = BindingId.fresh ()
        ctx.BindingMap <- Map.add id (MacroDef transformer) ctx.BindingMap
        Map.add name id scope

// ──────────────────────────────────────────────────────────────── Built-in scope

module SyntaxScope =

    let private builtinEntries: (string * SpecialFormKind) list =
        [ "if", SpecialFormKind.If
          "lambda", SpecialFormKind.Lambda
          "define", SpecialFormKind.Define
          "set!", SpecialFormKind.SetBang
          "begin", SpecialFormKind.Begin
          "quote", SpecialFormKind.Quote
          "let", SpecialFormKind.Let
          "let*", SpecialFormKind.LetStar
          "letrec", SpecialFormKind.Letrec
          "letrec*", SpecialFormKind.LetrecStar
          "define-syntax", SpecialFormKind.DefineSyntax
          "let-syntax", SpecialFormKind.LetSyntax
          "letrec-syntax", SpecialFormKind.LetrecSyntax
          "define-library", SpecialFormKind.DefineLibrary
          "import", SpecialFormKind.Import ]

    let private builtinPairs =
        builtinEntries |> List.map (fun (name, kind) -> name, BindingId.fresh (), kind)

    /// The initial scope mapping keyword names to their BindingIds.
    /// Callers extend this with macro and variable bindings before expansion.
    let builtin: SyntaxScope =
        builtinPairs |> List.map (fun (name, id, _) -> name, id) |> Map.ofList

    let private builtinMap: BindingMap =
        builtinPairs |> List.map (fun (_, id, kind) -> id, SpecialForm kind) |> Map.ofList

    // Seed ExpandCtx's forward-declared _builtinBindingMap.
    do ExpandCtx.setBuiltinBindingMap builtinMap

// Backward-compatibility alias so existing call-sites can still write SyntaxEnv.builtin.
module SyntaxEnv =
    let builtin = SyntaxScope.builtin

// ─────────────────────────────────────────────────────────────── MacrosNew

/// Parse `(syntax-rules ...)` bodies from `Stx` nodes into `SyntaxTransformer`
/// values.  This module replaces the old CST-based `Macros` module for the
/// new expander pipeline; only the functions consumed by `Expander` are public.
module MacrosNew =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Collect all bound pattern variable names from a pattern.
    let rec findBound (pat: StxPattern) : string list =
        match pat with
        | StxPattern.Variable name -> [ name ]
        | StxPattern.Form pats -> pats |> List.collect findBound
        | StxPattern.DottedForm(pats, tail) -> (pats |> List.collect findBound) @ findBound tail
        | StxPattern.Vec pats -> pats |> List.collect findBound
        | StxPattern.Repeat inner -> findBound inner
        | _ -> []

    /// Parse a pattern literal list `(lit1 lit2 ...)`.
    let parseLiteralList (stx: Stx) : Result<string list, string> =
        let rec unwrapId s =
            match s with
            | Stx.Id(name, _) -> Result.Ok name
            | Stx.Closure(inner, _, _) -> unwrapId inner
            | _ -> Result.Error "literal list must contain identifiers"

        let rec unwrapList s =
            match s with
            | Stx.List(items, _) ->
                items
                |> List.map unwrapId
                |> List.fold
                    (fun acc r ->
                        match acc, r with
                        | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ x ])
                        | Result.Error e, _ -> Result.Error e
                        | _, Result.Error e -> Result.Error e)
                    (Result.Ok [])
            | Stx.Closure(inner, _, _) -> unwrapList inner
            | _ -> Result.Error "expected list of literals"

        unwrapList stx

    /// Returns true if `stx` is the ellipsis identifier `ell`, peeling any
    /// `Stx.Closure` wrappers.
    let rec private isEllipsisNode (ell: string) (stx: Stx) : bool =
        match stx with
        | Stx.Id(name, _) -> name = ell
        | Stx.Closure(inner, _, _) -> isEllipsisNode ell inner
        | _ -> false

    /// Parse `(pats...)` or `(pats... . tail)` form body into a list of
    /// patterns, detecting ellipsis.
    let rec parseFormBody
        (kw: string)
        (ellipsis: string)
        (lits: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<StxPattern list * StxPattern option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailPat =
                    tail |> Option.map (fun t -> parsePattern kw ellipsis lits t)

                match tailPat with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parsePattern kw ellipsis lits single with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) []
            | current :: (next :: rest) when isEllipsisNode ellipsis next ->
                match parsePattern kw ellipsis lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok inner -> loop (StxPattern.Repeat inner :: acc) rest
            | current :: rest ->
                match parsePattern kw ellipsis lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) rest

        loop [] items

    /// Parse a single `Stx` node as a macro pattern.
    and parsePattern (kw: string) (ellipsis: string) (lits: string list) (stx: Stx) : Result<StxPattern, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parsePattern kw ellipsis lits inner
        | Stx.Const(lit, _) -> Result.Ok(StxPattern.Constant lit)
        | Stx.Id("_", _) -> Result.Ok StxPattern.Underscore
        | Stx.Id(name, _) when name = ellipsis && not (List.contains name lits) ->
            Result.Error "unexpected ellipsis in pattern"
        | Stx.Id(name, _) ->
            if List.contains name lits then
                Result.Ok(StxPattern.Literal name)
            else
                Result.Ok(StxPattern.Variable name)
        | Stx.List(items, _) ->
            match items with
            | Stx.Id(head, _) :: rest when head = kw ->
                match parseFormBody kw ellipsis lits rest None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(StxPattern.Form(StxPattern.Variable kw :: pats))
                | Result.Ok(pats, Some tail) ->
                    Result.Ok(StxPattern.DottedForm(StxPattern.Variable kw :: pats, tail))
            | _ ->
                match parseFormBody kw ellipsis lits items None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(StxPattern.Form pats)
                | Result.Ok(pats, Some tail) -> Result.Ok(StxPattern.DottedForm(pats, tail))
        | Stx.Dotted(items, tail, _) ->
            match parseFormBody kw ellipsis lits items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(pats, tailOpt) ->
                let tailPat = tailOpt |> Option.defaultValue StxPattern.Underscore
                Result.Ok(StxPattern.DottedForm(pats, tailPat))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parsePattern kw ellipsis lits)

            results
            |> List.fold
                (fun acc r ->
                    match acc, r with
                    | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ x ])
                    | Result.Error e, _ -> Result.Error e
                    | _, Result.Error e -> Result.Error e)
                (Result.Ok [])
            |> Result.map StxPattern.Vec
        | _ -> Result.Error $"unrecognised pattern node: {stx}"

    /// Detect ellipsis in a template form body.
    let rec parseTemplateFormBody
        (ellipsis: string)
        (bound: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<StxTemplateElement list * StxTemplate option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailTmpl = tail |> Option.map (fun t -> parseTemplate ellipsis bound t)

                match tailTmpl with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parseTemplate ellipsis bound single with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Template tmpl :: acc) []
            | current :: (next :: rest) when isEllipsisNode ellipsis next ->
                match parseTemplate ellipsis bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Repeated tmpl :: acc) rest
            | current :: rest ->
                match parseTemplate ellipsis bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Template tmpl :: acc) rest

        loop [] items

    /// Parse a single `Stx` node as a macro template.
    and parseTemplate (ellipsis: string) (bound: string list) (stx: Stx) : Result<StxTemplate, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parseTemplate ellipsis bound inner
        | Stx.Id(name, _) ->
            if List.contains name bound then
                Result.Ok(StxTemplate.Subst name)
            else
                Result.Ok(StxTemplate.Quoted stx)
        | Stx.List(items, loc) ->
            match parseTemplateFormBody ellipsis bound items None with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, None) -> Result.Ok(StxTemplate.Form(loc, elems))
            | Result.Ok(elems, Some tail) -> Result.Ok(StxTemplate.DottedForm(elems, tail))
        | Stx.Dotted(items, tail, loc) ->
            match parseTemplateFormBody ellipsis bound items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, tailOpt) ->
                let tailTmpl = tailOpt |> Option.defaultValue (StxTemplate.Quoted stx)
                Result.Ok(StxTemplate.DottedForm(elems, tailTmpl))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parseTemplate ellipsis bound)

            results
            |> List.fold
                (fun acc r ->
                    match acc, r with
                    | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ StxTemplateElement.Template x ])
                    | Result.Error e, _ -> Result.Error e
                    | _, Result.Error e -> Result.Error e)
                (Result.Ok [])
            |> Result.map StxTemplate.Vec
        | other ->
            Result.Ok(StxTemplate.Quoted other)

    /// Parse one `(pattern template)` transformer arm.
    let parseTransformer
        (kw: string)
        (ellipsis: string)
        (lits: string list)
        (stx: Stx)
        : Result<StxPattern * StxTemplate, string> =
        match stx with
        | Stx.List([ pat; tmpl ], _) ->
            match parsePattern kw ellipsis lits pat with
            | Result.Error e -> Result.Error e
            | Result.Ok patParsed ->
                let bound = findBound patParsed

                match parseTemplate ellipsis bound tmpl with
                | Result.Error e -> Result.Error e
                | Result.Ok tmplParsed -> Result.Ok(patParsed, tmplParsed)
        | _ -> Result.Error "each syntax-rules rule must be (pattern template)"

    /// Parse the body of `(syntax-rules (lits...) rule...)`.
    let parseSyntaxRulesBody
        (kw: string)
        (ellipsis: string)
        (litsStx: Stx)
        (rules: Stx list)
        (defScope: SyntaxScope)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxTransformer option =
        match parseLiteralList litsStx with
        | Result.Error e ->
            diag.Emit Diag.invalidMacro loc e
            None
        | Result.Ok lits ->
            let results = rules |> List.map (parseTransformer kw ellipsis lits)

            let patterns =
                results
                |> List.choose (function
                    | Result.Ok pair -> Some pair
                    | Result.Error e ->
                        diag.Emit Diag.invalidMacro loc e
                        None)

            Some
                { Patterns = patterns
                  DefScope = defScope
                  DefLoc = loc }

    let rec private stxIdName (stx: Stx) : string option =
        match stx with
        | Stx.Id(name, _) -> Some name
        | Stx.Closure(inner, _, _) -> stxIdName inner
        | _ -> None

    let rec private stxUnwrapList (stx: Stx) : Stx option =
        match stx with
        | Stx.List _ -> Some stx
        | Stx.Closure(inner, _, _) -> stxUnwrapList inner
        | _ -> None

    /// Public entry point: parse a `(syntax-rules ...)` form into a transformer.
    /// Supports the R7RS extended form `(syntax-rules <ellipsis> (lits...) rules...)`
    /// where an optional identifier before the literal list names the ellipsis.
    let parseSyntaxRulesStx
        (name: string)
        (stx: Stx)
        (defScope: SyntaxScope)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxTransformer option =
        let rec unwrap s =
            match s with
            | Stx.Closure(inner, _, _) -> unwrap inner
            | _ -> s

        match unwrap stx with
        | Stx.List(_ :: second :: rest, _) ->
            match stxIdName second, rest with
            | Some ellipsis, litsStx :: rules when stxUnwrapList litsStx |> Option.isSome ->
                parseSyntaxRulesBody name ellipsis litsStx rules defScope diag loc
            | None, rules when stxUnwrapList second |> Option.isSome ->
                parseSyntaxRulesBody name "..." second rules defScope diag loc
            | _ ->
                diag.Emit Diag.invalidMacro loc "expected (syntax-rules (literals...) rules...)"
                None
        | _ ->
            diag.Emit Diag.invalidMacro loc "expected (syntax-rules (literals...) rules...)"
            None

// ─────────────────────────────────────────────────────────────── Expander

module private Expander =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Peel any `Stx.Closure` wrappers and look up the head binding in the
    /// appropriate scope.  Returns `None` for non-identifier heads.
    let rec resolveHead (stx: Stx) (scope: SyntaxScope) (ctx: ExpandCtx) : BindingMeaning option =
        match stx with
        | Stx.Id(name, _) ->
            Map.tryFind name scope |> Option.bind (fun id -> Map.tryFind id ctx.BindingMap)
        | Stx.Closure(inner, closedScope, _) ->
            match inner with
            | Stx.Id(name, _) when not (Map.containsKey name closedScope) ->
                resolveHead inner scope ctx
            | _ -> resolveHead inner closedScope ctx
        | _ -> None

    /// Active pattern: extract an identifier name+location from a Stx node,
    /// peeling any `Stx.Closure` hygiene wrappers.
    let rec (|StxId|_|) (stx: Stx) : (string * TextLocation) option =
        match stx with
        | Stx.Id(name, loc) -> Some(name, loc)
        | Stx.Closure(inner, _, _) -> (|StxId|_|) inner
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
        | Stx.Vec(items, _) ->
            BoundDatum.SelfEval(BoundLiteral.Vector(items |> List.map stxToDatum))
        | Stx.ByteVec(bytes, _) ->
            let bs =
                bytes |> List.choose (function Ok b -> Some b | Result.Error _ -> None)

            BoundDatum.SelfEval(BoundLiteral.ByteVector bs)

    /// Parse formal parameters from a Stx node into a `BoundFormals` value.
    let rec parseFormals (stx: Stx) (ctx: ExpandCtx) : BoundFormals =
        match stx with
        | Stx.Id(name, _) -> BoundFormals.Simple name
        | Stx.List(items, _) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc
                            "expected identifier in formals"

                        None)

            BoundFormals.List names
        | Stx.Dotted(items, tail, loc) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc
                            "expected identifier in formals"

                        None)

            match tail with
            | StxId(rest, _) -> BoundFormals.DottedList(names, rest)
            | _ ->
                ExpandCtx.emitError ctx Diag.invalidFormals loc
                    "expected identifier after dot in formals"

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
            | Stx.List([ StxId(name, _); init ], _) -> Some(name, init)
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc
                    "ill-formed binding specification"

                None

        match stx with
        | Stx.List(items, _) -> items |> List.choose parseOne
        | Stx.Closure(inner, _, _) -> parseBindingSpecs inner ctx
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "expected binding list"
            []

    // ── Variable lookup with capture tracking ─────────────────────────────

    /// Resolve a variable name to its (potentially capture-wrapped) StorageRef.
    /// Uses BindingIds to distinguish same-named variables from different scopes,
    /// enabling hygienic macro expansion.
    let resolveVar
        (name: string)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : StorageRef option =
        match Map.tryFind name scope with
        | Some id ->
            match Map.tryFind id ctx.BindingMap with
            | Some(Variable(storage, definedAt)) ->
                if definedAt < ctx.LambdaDepth then
                    Some(ExpandCtx.captureAcrossLambdas storage definedAt ctx)
                else
                    Some storage
            | Some(SpecialForm _) | Some(MacroDef _) ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc
                    $"keyword '{name}' used in value position"

                None
            | None ->
                ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
                None
        | None ->
            ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
            None

    /// Look up a name in the scope, tracking captures for variables from
    /// outer lambda scopes.
    let lookupVar
        (name: string)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
        match resolveVar name loc scope ctx with
        | Some storage -> BoundExpr.Load storage
        | None -> BoundExpr.Error

    // ── Main recursive expander ───────────────────────────────────────────

    /// Expand a single Stx node in the given scope.
    let rec expand (stx: Stx) (scope: SyntaxScope) (ctx: ExpandCtx) : BoundExpr =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            // Syntactic closure: expand `inner` in the definition-site scope.
            // The closed scope is the authoritative source of BindingIds for
            // any identifiers present in it, which is the key hygiene mechanism:
            // macro-introduced `temp` (BindingId N) and user's `temp` (BindingId M)
            // are distinguished by their different ids even though they share a name.
            //
            // Exception: a simple identifier NOT in the closed scope falls back
            // to the call-site scope, allowing macro-introduced names to be visible
            // once they are bound at the call site.
            match inner with
            | Stx.Id(name, _) when not (Map.containsKey name closedScope) ->
                expand inner scope ctx
            | _ -> expand inner closedScope ctx

        | Stx.Id(name, loc) -> lookupVar name loc scope ctx

        | Stx.Const(lit, _) -> BoundExpr.Literal lit

        | Stx.Quoted(inner, _) -> BoundExpr.Quoted(stxToDatum inner)

        | Stx.Vec(items, _) ->
            BoundExpr.Literal(BoundLiteral.Vector(items |> List.map stxToDatum))

        | Stx.ByteVec(bytes, loc) ->
            let bs =
                bytes
                |> List.choose (function
                    | Ok b -> Some b
                    | Result.Error msg ->
                        ExpandCtx.emitError ctx Diag.illFormedForm loc msg
                        None)

            BoundExpr.Literal(BoundLiteral.ByteVector bs)

        | Stx.Dotted(_, _, loc) ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "dotted pair in expression position"
            BoundExpr.Error

        | Stx.List([], _) -> BoundExpr.Literal BoundLiteral.Null

        | Stx.List(head :: args, loc) -> expandForm head args loc scope ctx

    /// Dispatch a compound form based on what the head resolves to.
    and expandForm
        (head: Stx)
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
        match resolveHead head scope ctx with
        | Some(SpecialForm kind) -> expandSpecialForm kind args loc scope ctx
        | Some(MacroDef macro) -> expandMacro macro (Stx.List(head :: args, loc)) scope ctx
        | _ ->
            let fn = expand head scope ctx
            let actuals = args |> List.map (fun a -> expand a scope ctx)
            BoundExpr.Application(fn, actuals)

    /// Expand a special form.
    and expandSpecialForm
        (kind: SpecialFormKind)
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
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
            | [ StxId(name, idLoc); value ] ->
                match resolveVar name idLoc scope ctx with
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

        | SpecialFormKind.LetSyntax ->
            expandLetSyntax args loc scope ctx false

        | SpecialFormKind.LetrecSyntax ->
            expandLetSyntax args loc scope ctx true

        | SpecialFormKind.Import ->
            let expr, _ = expandImport args loc scope ctx
            expr

        | SpecialFormKind.DefineLibrary ->
            fst (expandLibrary args loc scope ctx)

    // ── Lambda ───────────────────────────────────────────────────────────

    and expandLambda
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
        match args with
        | formals :: body when not (List.isEmpty body) ->
            let bFormals = parseFormals formals ctx
            let names = formalsNames bFormals
            let childCtx = ExpandCtx.childForLambda ctx

            // Add formals to childCtx's BindingMap and extend the scope.
            let scopeWithFormals =
                names
                |> List.mapi (fun i name ->
                    let id = BindingId.fresh ()

                    childCtx.BindingMap <-
                        Map.add id (Variable(StorageRef.Arg i, childCtx.LambdaDepth)) childCtx.BindingMap

                    name, id)
                |> List.fold (fun s (name, id) -> Map.add name id s) scope

            let bodyExprs, _ = expandSeq body scopeWithFormals childCtx
            let bd = ExpandCtx.intoBody childCtx bodyExprs
            BoundExpr.Lambda(bFormals, bd)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'lambda'"
            BoundExpr.Error

    // ── Define ───────────────────────────────────────────────────────────

    /// Expand a `define` form, returning the bound expression and the extended
    /// `SyntaxScope` for subsequent sibling forms.
    and expandDefine
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr * SyntaxScope =
        let illFormed () =
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define'"
            BoundExpr.Error, scope

        match args with
        | [ StxId(name, _) ] ->
            // (define id) — uninitialized
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            BoundExpr.Store(storage, None), scope'

        | [ StxId(name, _); value ] ->
            // (define id value)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            // Name is in scope' so self-recursion works.
            let boundValue = expand value scope' ctx
            BoundExpr.Store(storage, Some boundValue), scope'

        | Stx.List(StxId(name, _) :: formals, _) :: body ->
            // (define (name formals...) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            let lambdaExpr = expandLambda (Stx.List(formals, loc) :: body) loc scope' ctx
            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | Stx.Dotted(StxId(name, _) :: formals, tail, _) :: body ->
            // (define (name formals... . rest) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            let lambdaExpr = expandLambda (Stx.Dotted(formals, tail, loc) :: body) loc scope' ctx
            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | _ -> illFormed ()

    // ── Let forms ────────────────────────────────────────────────────────

    and expandLet
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
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
                    let id = BindingId.fresh ()
                    name, id, storage, initExpr)

            // Now register all bindings and extend the scope for the body.
            let innerScope =
                stores
                |> List.fold
                    (fun s (name, id, storage, _) ->
                        ctx.BindingMap <-
                            Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

                        Map.add name id s)
                    scope

            ctx.ScopeDepth <- ctx.ScopeDepth + 1
            let bodyExprs, _ = expandSeq body innerScope ctx
            ctx.ScopeDepth <- ctx.ScopeDepth - 1
            let storeExprs = stores |> List.map (fun (_, _, s, v) -> BoundExpr.Store(s, Some v))
            BoundExpr.Seq(storeExprs @ bodyExprs)
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'let'"
            BoundExpr.Error

    and expandLetStar
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
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
                        let id = BindingId.fresh ()
                        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
                        let nextScope = Map.add name id currentScope
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
        (scope: SyntaxScope)
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
                    let id = BindingId.fresh ()
                    ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
                    name, id, storage)

            let innerScope =
                storages
                |> List.fold (fun s (name, id, _) -> Map.add name id s) scope

            let mutable uninitNames = storages |> List.map (fun (n, _, _) -> n)

            let storeExprs =
                List.map2
                    (fun (name, _id, storage) (_, initStx) ->
                        let initExpr = expand initStx innerScope ctx

                        if not isStar then
                            checkUninitialised uninitNames initExpr loc ctx

                        if isStar then
                            uninitNames <- List.tail uninitNames

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

    /// Check a BoundExpr for loads from any uninitialised storage slots.
    and checkUninitialised
        (uninitNames: string list)
        (expr: BoundExpr)
        (loc: TextLocation)
        (ctx: ExpandCtx)
        =
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

        and warnIfUninit _storage = ()

        check expr

    // ── define-syntax / let-syntax ───────────────────────────────────────

    and expandDefineSyntax
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr * SyntaxScope =
        match args with
        | [ StxId(name, _); rulesStx ] ->
            match parseSyntaxRulesStx name rulesStx scope ctx with
            | Some transformer ->
                let scope' = ExpandCtx.addMacro ctx name transformer scope
                BoundExpr.Nop, scope'
            | None -> BoundExpr.Error, scope
        | _ ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-syntax'"
            BoundExpr.Error, scope

    and expandLetSyntax
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        (_isLetrec: bool)
        : BoundExpr =
        match args with
        | bindingStx :: body when not (List.isEmpty body) ->
            let specs = parseBindingSpecs bindingStx ctx

            let bodyScope =
                specs
                |> List.fold
                    (fun currentScope (name, ruleStx) ->
                        match parseSyntaxRulesStx name ruleStx currentScope ctx with
                        | Some transformer ->
                            ExpandCtx.addMacro ctx name transformer currentScope
                        | None -> currentScope)
                    scope

            // The scope is immutable and threaded: `bodyScope` is only visible
            // within this call.  Macro BindingIds added to ctx.BindingMap here
            // are inaccessible outside the body (no scope entry points to them
            // from the outer scope).  No save/restore of any mutable ctx field
            // is needed for that reason; ScopeDepth is still bumped so that
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
        (defScope: SyntaxScope)
        (ctx: ExpandCtx)
        : SyntaxTransformer option =
        MacrosNew.parseSyntaxRulesStx name stx defScope ctx.Diagnostics stx.Loc

    // ── Macro expansion ───────────────────────────────────────────────────

    and expandMacro
        (macro: SyntaxTransformer)
        (form: Stx)
        (callScope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr =
        let result =
            macro.Patterns
            |> List.tryPick (fun (pat, tmpl) ->
                matchPattern pat form callScope |> Option.map (fun b -> tmpl, b))

        match result with
        | None ->
            ExpandCtx.emitError ctx Diag.expandError form.Loc "no macro rule matched the form"
            BoundExpr.Error
        | Some(template, bindings) ->
            let transcribed = transcribe template bindings macro.DefScope form.Loc
            expand transcribed callScope ctx

    // ── Pattern matching ──────────────────────────────────────────────────

    /// Attempt to match a StxPattern against a Stx node.
    /// Returns Some PatternBindings on success, None on mismatch.
    and matchPattern (pat: StxPattern) (stx: Stx) (scope: SyntaxScope) : PatternBindings option =

        match pat, stx with
        | StxPattern.Underscore, _ -> Some Map.empty

        | StxPattern.Variable v, s -> Some(Map.ofList [ v, Single(s, scope) ])

        | StxPattern.Literal lit, Stx.Id(name, _) ->
            if name = lit then Some Map.empty else None

        | StxPattern.Literal lit, Stx.Closure(inner, closedScope, _) ->
            matchPattern (StxPattern.Literal lit) inner closedScope

        | StxPattern.Constant patLit, Stx.Const(stxLit, _) ->
            if patLit = stxLit then Some Map.empty else None

        | StxPattern.Form pats, Stx.List(items, _) ->
            matchPatternList pats None items None scope

        | StxPattern.DottedForm(pats, tailPat), Stx.Dotted(items, tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) scope

        // A proper list can also match a dotted-form pattern.
        | StxPattern.DottedForm(pats, tailPat), Stx.List(items, _) ->
            matchPatternList pats (Some tailPat) items None scope

        | StxPattern.Form pats, Stx.Closure(inner, closedScope, _) ->
            matchPattern (StxPattern.Form pats) inner closedScope

        | StxPattern.DottedForm(pats, tailPat), Stx.Closure(inner, closedScope, _) ->
            matchPattern (StxPattern.DottedForm(pats, tailPat)) inner closedScope

        | StxPattern.Vec pats, Stx.Vec(items, _) ->
            matchPatternList pats None items None scope

        | _ -> None

    and matchPatternList
        (pats: StxPattern list)
        (tailPat: StxPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (scope: SyntaxScope)
        : PatternBindings option =
        match pats with
        | StxPattern.Repeat inner :: restPats ->
            matchRepeat inner restPats tailPat items tailItem scope []

        | headPat :: restPats ->
            match items with
            | headItem :: restItems ->
                matchPattern headPat headItem scope
                |> Option.bind (fun b1 ->
                    matchPatternList restPats tailPat restItems tailItem scope
                    |> Option.map (mergeBindings b1))
            | [] -> None

        | [] ->
            match tailPat with
            | Some tp ->
                match tailItem with
                | Some t -> matchPattern tp t scope
                | None ->
                    match items with
                    | [ single ] -> matchPattern tp single scope
                    | _ when not (List.isEmpty items) ->
                        match tp with
                        | StxPattern.Underscore -> Some Map.empty
                        | StxPattern.Variable v ->
                            Some(
                                Map.ofList
                                    [ v, Single(Stx.List(items, TextLocation.Missing), scope) ]
                            )
                        | _ -> None
                    | _ -> None
            | None -> if List.isEmpty items && tailItem.IsNone then Some Map.empty else None

    and matchRepeat
        (inner: StxPattern)
        (restPats: StxPattern list)
        (tailPat: StxPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (scope: SyntaxScope)
        (accumulated: PatternBindings list)
        : PatternBindings option =
        match matchPatternList restPats tailPat items tailItem scope with
        | Some tailBindings ->
            let repeatedBindings = gatherRepeated accumulated
            Some(mergeBindings tailBindings repeatedBindings)
        | None ->
            match items with
            | head :: rest ->
                matchPattern inner head scope
                |> Option.bind (fun b ->
                    matchRepeat inner restPats tailPat rest tailItem scope (b :: accumulated))
            | [] -> None

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

    // ── Transcription ─────────────────────────────────────────────────────

    and transcribe
        (template: StxTemplate)
        (bindings: PatternBindings)
        (defScope: SyntaxScope)
        (loc: TextLocation)
        : Stx =
        match template with
        | StxTemplate.Subst name ->
            match Map.tryFind name bindings with
            | Some(Single(stx, capturedScope)) -> Stx.Closure(stx, capturedScope, loc)
            | Some(Repeated _) ->
                failwith $"ellipsis variable '{name}' used outside a repeat context"
            | None -> failwith $"template variable '{name}' not bound"

        | StxTemplate.Quoted stx ->
            // Literal from the definition site — wrap in the definition-site scope
            // so that any identifiers inside resolve hygienically.
            Stx.Closure(stx, defScope, loc)

        | StxTemplate.Form(_, elements) ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc)
            Stx.List(children, loc)

        | StxTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defScope loc)
            Stx.Dotted(children, transcribe tail bindings defScope loc, loc)

        | StxTemplate.Vec elements ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc)
            Stx.Vec(children, loc)

    and transcribeElement
        (bindings: PatternBindings)
        (defScope: SyntaxScope)
        (loc: TextLocation)
        (elem: StxTemplateElement)
        : Stx list =
        match elem with
        | StxTemplateElement.Template t -> [ transcribe t bindings defScope loc ]
        | StxTemplateElement.Repeated t -> transcribeRepeated t bindings defScope loc

    and transcribeRepeated
        (template: StxTemplate)
        (bindings: PatternBindings)
        (defScope: SyntaxScope)
        (loc: TextLocation)
        : Stx list =
        let rec templateRefs tmpl =
            match tmpl with
            | StxTemplate.Subst name -> Set.singleton name
            | StxTemplate.Form(_, elems) | StxTemplate.Vec elems ->
                elems |> List.map elemRefs |> Set.unionMany
            | StxTemplate.DottedForm(elems, tail) ->
                Set.union (elems |> List.map elemRefs |> Set.unionMany) (templateRefs tail)
            | StxTemplate.Quoted _ -> Set.empty

        and elemRefs elem =
            match elem with
            | StxTemplateElement.Template t | StxTemplateElement.Repeated t -> templateRefs t

        let referencedNames = templateRefs template

        let repCount =
            bindings
            |> Map.tryPick (fun name v ->
                match v with
                | Repeated lst when Set.contains name referencedNames -> Some(List.length lst)
                | _ -> None)
            |> Option.defaultValue 0

        [ 0 .. repCount - 1 ]
        |> List.map (fun i ->
            let perRepBindings =
                Map.map
                    (fun name v ->
                        match v with
                        | Repeated lst when Set.contains name referencedNames -> Single(lst.[i])
                        | other -> other)
                    bindings

            transcribe template perRepBindings defScope loc)

    // ── Import set parsing  ───────────────────────────────────────────────

    and private stxToImportSet (diags: DiagnosticBag) (stx: Stx) : ImportSet =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | Stx.Closure(Stx.Id(n, _), _, _) -> Some n
            | _ -> None

        let getNames items = items |> List.choose getName

        match stx with
        | Stx.Closure(inner, _, _) -> stxToImportSet diags inner
        | Stx.List(Stx.Id("only", _) :: fromSet :: filters, _) ->
            ImportSet.Only(stxToImportSet diags fromSet, getNames filters)
        | Stx.List(Stx.Id("except", _) :: fromSet :: filters, _) ->
            ImportSet.Except(stxToImportSet diags fromSet, getNames filters)
        | Stx.List([ Stx.Id("prefix", _); fromSet; Stx.Id(prefix, _) ], _) ->
            ImportSet.Prefix(stxToImportSet diags fromSet, prefix)
        | Stx.List(Stx.Id("rename", _) :: fromSet :: renames, _) ->
            let parseRename =
                function
                | Stx.List([ Stx.Id(fr, _); Stx.Id(to_, _) ], _) ->
                    Some { SymbolRename.From = fr; To = to_ }
                | _ -> None

            ImportSet.Renamed(stxToImportSet diags fromSet, renames |> List.choose parseRename)
        | Stx.List(parts, _) ->
            let name = getNames parts
            if List.isEmpty name then ImportSet.Error else ImportSet.Plain name
        | _ -> ImportSet.Error

    // ── Library name parsing ──────────────────────────────────────────────

    and private stxToLibraryName (stx: Stx) (ctx: ExpandCtx) : string list option =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | Stx.Closure(Stx.Id(n, _), _, _) -> Some n
            | Stx.Const(BoundLiteral.Number n, _) -> Some(string (int n))
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedForm other.Loc
                    "expected identifier in library name"

                None

        match stx with
        | Stx.Closure(inner, _, _) -> stxToLibraryName inner ctx
        | Stx.List(parts, _) ->
            let names = List.choose getName parts
            if List.length names = List.length parts then Some names else None
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedForm other.Loc "expected list for library name"
            None

    // ── define-library ────────────────────────────────────────────────────

    and expandLibrary
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr * SyntaxScope =
        match args with
        | nameStx :: declarations ->
            let name = stxToLibraryName nameStx ctx |> Option.defaultValue []
            let mangledName = name |> String.concat "::"

            let innerCtx = ExpandCtx.childForLibrary ctx mangledName

            let folder (innerScope, importAcc, bodyAcc, exportsAcc) decl =
                let decl' =
                    match decl with
                    | Stx.Closure(inner, _, _) -> inner
                    | x -> x

                match decl' with
                | Stx.List(Stx.Id("export", _) :: exports, _) ->
                    let newExports =
                        exports
                        |> List.choose (function
                            | Stx.Id(n, _) -> Some(n, n)
                            | Stx.Closure(Stx.Id(n, _), _, _) -> Some(n, n)
                            | Stx.List(
                                [ Stx.Id("rename", _); Stx.Id(intName, _); Stx.Id(extName, _) ],
                                _) ->
                                Some(extName, intName)
                            | other ->
                                ExpandCtx.emitError innerCtx Diag.illFormedForm other.Loc
                                    "invalid export element"

                                None)

                    (innerScope, importAcc, bodyAcc, exportsAcc @ newExports)

                | Stx.List(Stx.Id("import", _) :: importArgs, declLoc) ->
                    let expr, newScope = expandImport importArgs declLoc innerScope innerCtx
                    (newScope, importAcc @ [ expr ], bodyAcc, exportsAcc)

                | Stx.List(Stx.Id("begin", _) :: beginBody, _) ->
                    let exprs, newScope = expandSeq beginBody innerScope innerCtx
                    (newScope, importAcc, bodyAcc @ exprs, exportsAcc)

                | Stx.List(Stx.Id(kw, _) :: _, declLoc) ->
                    ExpandCtx.emitError ctx Diag.illFormedForm declLoc
                        $"unrecognised library declaration '{kw}'"

                    (innerScope, importAcc, bodyAcc, exportsAcc)

                | other ->
                    ExpandCtx.emitError ctx Diag.illFormedForm other.Loc
                        "ill-formed library declaration"

                    (innerScope, importAcc, bodyAcc, exportsAcc)

            // Library inner scope: builtin special forms + inherited macros.
            // Variable bindings from the outer scope are intentionally excluded;
            // the library must import everything it uses.
            let initInnerScope =
                scope
                |> Map.fold
                    (fun acc k id ->
                        match Map.tryFind id innerCtx.BindingMap with
                        | Some(MacroDef _) -> Map.add k id acc
                        | _ -> acc)
                    SyntaxScope.builtin

            let (finalInnerScope, importExprs, bodyExprs, exportedPairs) =
                List.fold folder (initInnerScope, [], [], []) declarations

            let exports =
                exportedPairs
                |> List.choose (fun (extName, intName) ->
                    match Map.tryFind intName finalInnerScope with
                    | Some id ->
                        match Map.tryFind id innerCtx.BindingMap with
                        | Some(Variable(storage, _)) -> Some(extName, storage)
                        | Some _ ->
                            ExpandCtx.emitError ctx Diag.illFormedForm loc
                                $"'{intName}' is a keyword and cannot be exported"

                            None
                        | None ->
                            ExpandCtx.emitError ctx Diag.illFormedForm loc
                                $"exported name '{intName}' is not defined in library"

                            None
                    | None ->
                        ExpandCtx.emitError ctx Diag.illFormedForm loc
                            $"exported name '{intName}' is not defined in library"

                        None)

            ctx.Libraries <- { LibraryName = name; Exports = exports } :: ctx.Libraries

            let body = ExpandCtx.intoBody innerCtx (importExprs @ bodyExprs)
            (BoundExpr.Library(name, mangledName, exports, body), scope)

        | [] ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-library'"
            (BoundExpr.Error, scope)

    /// Expand an `import` form.
    and expandImport
        (args: Stx list)
        (loc: TextLocation)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr * SyntaxScope =
        let folder (currentScope: SyntaxScope, exprs: BoundExpr list) importStx =
            let importSet = stxToImportSet ctx.Diagnostics importStx

            match Libraries.resolveImport ctx.Libraries importSet with
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
                                let id = BindingId.fresh ()

                                ctx.BindingMap <-
                                    Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

                                Map.add name id s)
                        currentScope

                let mangledName = library.LibraryName |> String.concat "::"
                (newScope, BoundExpr.Import mangledName :: exprs)
            | Result.Error msg ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc msg
                (currentScope, BoundExpr.Error :: exprs)

        let (finalScope, revExprs) = List.fold folder (scope, []) args
        (BoundExpr.Seq(List.rev revExprs), finalScope)

    // ── Sequence expansion ────────────────────────────────────────────────

    /// Check whether a Stx node is a definition form.
    and isBindingForm (stx: Stx) (scope: SyntaxScope) (ctx: ExpandCtx) : bool =
        match stx with
        | Stx.List(head :: _, _) ->
            match resolveHead head scope ctx with
            | Some(SpecialForm SpecialFormKind.Define) -> true
            | Some(SpecialForm SpecialFormKind.DefineSyntax) -> true
            | Some(SpecialForm SpecialFormKind.Begin) -> true
            | Some(SpecialForm SpecialFormKind.Import) -> true
            | Some(SpecialForm SpecialFormKind.DefineLibrary) -> true
            | _ -> false
        | Stx.Closure(inner, closedScope, _) -> isBindingForm inner closedScope ctx
        | _ -> false

    /// Try to expand a form as a binding-level definition.
    and tryExpandBinding
        (stx: Stx)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : (BoundExpr * SyntaxScope) option =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            tryExpandBinding inner closedScope ctx
        | Stx.List(head :: args, loc) ->
            match resolveHead head scope ctx with
            | Some(SpecialForm SpecialFormKind.Define) ->
                Some(expandDefine args loc scope ctx)
            | Some(SpecialForm SpecialFormKind.DefineSyntax) ->
                Some(expandDefineSyntax args loc scope ctx)
            | Some(SpecialForm SpecialFormKind.Begin) ->
                let exprs, scope' = expandSeq args scope ctx
                Some(BoundExpr.Seq exprs, scope')
            | Some(SpecialForm SpecialFormKind.Import) ->
                Some(expandImport args loc scope ctx)
            | Some(SpecialForm SpecialFormKind.DefineLibrary) ->
                Some(expandLibrary args loc scope ctx)
            | Some(MacroDef macro) ->
                // Macro application in definition context: transcribe and re-process
                // so that expansions producing `(define ...)` etc. are spliced in.
                let form = Stx.List(head :: args, loc)

                let transcribed =
                    macro.Patterns
                    |> List.tryPick (fun (pat, tmpl) ->
                        matchPattern pat form scope |> Option.map (fun b -> tmpl, b))
                    |> Option.map (fun (tmpl, bindings) ->
                        transcribe tmpl bindings macro.DefScope loc)

                match transcribed with
                | None -> None
                | Some expanded ->
                    match tryExpandBinding expanded scope ctx with
                    | Some result -> Some result
                    | None -> Some(expand expanded scope ctx, scope)
            | _ -> None
        | _ -> None

    /// Expand a list of forms, threading scope through any definitions.
    and expandSeq
        (stxs: Stx list)
        (scope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr list * SyntaxScope =
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

// ─────────────────────────────────────────────────── Public API

/// Public API for the new binding and expansion pass.
module Expand =

    /// Expand a parsed program into a list of `BoundExpr` nodes.
    ///
    /// `initialScope` should extend `SyntaxScope.builtin` with any additional
    /// macro or variable bindings for the compilation unit.
    /// The ctx's BindingMap must already contain the meanings for all BindingIds
    /// present in `initialScope` (this is handled automatically for the builtin
    /// special forms; callers use `ExpandCtx.addMacro` for additional macros).
    let expandProgram
        (prog: Tree.Program)
        (initialScope: SyntaxScope)
        (ctx: ExpandCtx)
        : BoundExpr list =
        let docId = prog.DocId

        let stxs =
            prog.Body
            |> Seq.map (Stx.ofExpr ctx.Registry docId)
            |> List.ofSeq

        let exprs, _ = Expander.expandSeq stxs initialScope ctx
        exprs
