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


// ──────────────────────────────────── Syntax pattern types

/// Pattern element for matching `Stx` syntax in macro rules.
[<RequireQualifiedAccess>]
type StxPattern =
    /// Match a self-evaluating literal exactly.
    | Constant of StxDatum
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
    | Single of stx: Stx * env: StxEnvironment
    | Repeated of (Stx * StxEnvironment) list

/// All captured bindings from a successful pattern match.
type PatternBindings = Map<string, PatternCapture>


// ──────────────────────────────────── Transformer and BindingMeaning types

/// A macro transformer: patterns, templates, and definition-site scope.
type SyntaxRulesTransformer =
    { Patterns: (StxPattern * StxTemplate) list
      DefScope: StxEnvironment
      DefLoc: TextLocation }

/// What a Ident resolves to at compile time.
type BindingMeaning =
    | Variable of StorageRef * lambdaDepth: int

/// Ident → its compile-time meaning.
type BindingMap = Map<Ident, BindingMeaning>

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
      Parent: ExpandCtx option }

module ExpandCtx =


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
          BindingMap = Map.empty
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

    /// Introduce a new variable binding: fresh Ident, storage allocation,
    /// register in ctx.BindingMap, return (id, storage, extended scope).
    let mintVar
        (ctx: ExpandCtx)
        (name: string)
        (scope: StxEnvironment)
        : Ident * StorageRef * StxEnvironment =
        let id = Ident.fresh ()
        let storage = mintStorage ctx name
        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
        id, storage, Map.add name (StxBinding.Variable id) scope

    /// Register a macro (syntax transformer) in ctx.BindingMap and return the
    /// extended scope with the macro name mapped to its fresh Ident.
    let addMacro
        (ctx: ExpandCtx)
        (name: string)
        (transformer: SyntaxTransformer)
        (scope: StxEnvironment)
        : StxEnvironment =
        // FIXME: Need to get rid fo the dupe `SyntaxTransformer` type, and and
        //        store a delegate here ratehr than a strucutred representation
        //        of the transfomer. This will allow us to decouple the syntax
        //        tree from the macro representation and opens the door to other
        //        macro systems in the future such as procedural macros.
        Map.add name (StxBinding.Macro transformer) scope

    /// Register an existing StorageRef as a visible variable in scope.
    /// Used to seed preloaded library bindings into the initial scope
    /// without allocating new storage (e.g. for Script / REPL mode).
    let registerStorage
        (ctx: ExpandCtx)
        (name: string)
        (storage: StorageRef)
        (scope: StxEnvironment)
        : StxEnvironment =
        let id = Ident.fresh ()
        ctx.BindingMap <- Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap
        Map.add name (StxBinding.Variable id) scope

// ──────────────────────────────────────────────────────────────── Built-in scope

module StxEnvironment =

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
        builtinEntries |> List.map (fun (name, kind) -> name, Ident.fresh (), kind)

    /// The initial scope mapping keyword names to their Idents.
    /// Callers extend this with macro and variable bindings before expansion.
    let builtin: StxEnvironment =
        builtinPairs |> List.map (fun (name, id, kind) -> name, StxBinding.Special kind) |> Map.ofList

// Backward-compatibility alias so existing call-sites can still write SyntaxEnv.builtin.
module SyntaxEnv =
    let builtin = StxEnvironment.builtin

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
            | Stx.List(items, _, _) ->
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
        | Stx.Datum(d, _) -> Result.Ok(StxPattern.Constant d)
        | Stx.Id("_", _) -> Result.Ok StxPattern.Underscore
        | Stx.Id(name, _) when name = ellipsis && not (List.contains name lits) ->
            Result.Error "unexpected ellipsis in pattern"
        | Stx.Id(name, _) ->
            if List.contains name lits then
                Result.Ok(StxPattern.Literal name)
            else
                Result.Ok(StxPattern.Variable name)
        | Stx.List(items, None, _) ->
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
        | Stx.List(items, Some tail, _) ->
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
        | Stx.List(items, None, loc) ->
            match parseTemplateFormBody ellipsis bound items None with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, None) -> Result.Ok(StxTemplate.Form(loc, elems))
            | Result.Ok(elems, Some tail) -> Result.Ok(StxTemplate.DottedForm(elems, tail))
        | Stx.List(items, Some tail, loc) ->
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
        | Stx.List([ pat; tmpl ], _, _) ->
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
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxRulesTransformer option =
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
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxRulesTransformer option =
        let rec unwrap s =
            match s with
            | Stx.Closure(inner, _, _) -> unwrap inner
            | _ -> s

        match unwrap stx with
        | Stx.List(_ :: second :: rest, _, _) ->
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

    // ── Pattern matching ──────────────────────────────────────────────────

    /// Attempt to match a StxPattern against a Stx node.
    /// Returns Some PatternBindings on success, None on mismatch.
    let rec matchPattern (pat: StxPattern) (stx: Stx) (scope: StxEnvironment) : PatternBindings option =
        match pat, stx with
        | StxPattern.Underscore, _ -> Some Map.empty

        | StxPattern.Variable v, s -> Some(Map.ofList [ v, Single(s, scope) ])

        | StxPattern.Literal lit, Stx.Id(name, _) ->
            if name = lit then Some Map.empty else None

        | StxPattern.Literal lit, Stx.Closure(inner, closedScope, _) ->
            matchPattern (StxPattern.Literal lit) inner closedScope

        | StxPattern.Constant patLit, Stx.Datum(stxLit, _) ->
            if patLit = stxLit then Some Map.empty else None

        | StxPattern.Form pats, Stx.List(items, None, _) ->
            matchPatternList pats None items None scope

        | StxPattern.DottedForm(pats, tailPat), Stx.List(items, Some tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) scope

        | StxPattern.DottedForm(pats, tailPat), Stx.List(items, None, _) ->
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
        (scope: StxEnvironment)
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
                                    [ v, Single(Stx.List(items, None, TextLocation.Missing), scope) ]
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
        (scope: StxEnvironment)
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
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
        : Stx =
        match template with
        | StxTemplate.Subst name ->
            match Map.tryFind name bindings with
            | Some(Single(stx, capturedScope)) -> Stx.Closure(stx, capturedScope, loc)
            | Some(Repeated _) ->
                diag.Emit Diag.invalidMacro loc $"ellipsis variable '{name}' used outside a repeat context"
                Stx.List([], None, loc)
            | None ->
                diag.Emit Diag.invalidMacro loc $"template variable '{name}' not bound"
                Stx.List([], None, loc)

        | StxTemplate.Quoted stx ->
            Stx.Closure(stx, defScope, loc)

        | StxTemplate.Form(_, elements) ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.List(children, None, loc)

        | StxTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.List(children, Some(transcribe tail bindings defScope loc diag), loc)

        | StxTemplate.Vec elements ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.Vec(children, loc)

    and transcribeElement
        (bindings: PatternBindings)
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
        (elem: StxTemplateElement)
        : Stx list =
        match elem with
        | StxTemplateElement.Template t -> [ transcribe t bindings defScope loc diag ]
        | StxTemplateElement.Repeated t -> transcribeRepeated t bindings defScope loc diag

    and transcribeRepeated
        (template: StxTemplate)
        (bindings: PatternBindings)
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
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

            transcribe template perRepBindings defScope loc diag)

    /// Public entry point: parse a `(syntax-rules ...)` form into a `SyntaxTransformer`.
    /// Returns a ready-to-call function, or `None` if the form is malformed.
    let makeSyntaxTransformer
        (name: string)
        (stx: Stx)
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxTransformer option =
        parseSyntaxRulesStx name stx defScope diag loc
        |> Option.map (fun rules ->
            fun (form: Stx) (callScope: StxEnvironment) ->
                let result =
                    rules.Patterns
                    |> List.tryPick (fun (pat, tmpl) ->
                        matchPattern pat form callScope |> Option.map (fun b -> tmpl, b))

                match result with
                | None -> Result.Error "no macro rule matched the form"
                | Some(template, bindings) ->
                    let transcribed = transcribe template bindings rules.DefScope form.Loc diag
                    Result.Ok transcribed)

// ─────────────────────────────────────────────────────────────── Expander

module private Expander =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Peel any `Stx.Closure` wrappers and look up the head binding in the
    /// appropriate scope.  Returns `None` for non-identifier heads.
    let rec resolveHead (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : StxBinding option =
        match stx with
        | Stx.Id(name, _) ->
            Map.tryFind name scope
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

    /// Convert a syntactic datum to its bound-tree counterpart.
    let datumToLiteral =
        function
        | StxDatum.Boolean b   -> BoundLiteral.Boolean b
        | StxDatum.Number n    -> BoundLiteral.Number n
        | StxDatum.Character c -> BoundLiteral.Character c
        | StxDatum.Str s       -> BoundLiteral.Str s
        | StxDatum.ByteVector bs -> BoundLiteral.ByteVector bs

    /// Convert a Stx node to a BoundDatum (for quoted expressions).
    let rec stxToDatum (stx: Stx) : BoundDatum =
        match stx with
        | Stx.Id(name, _) -> BoundDatum.Ident name
        | Stx.Datum(d, _) -> BoundDatum.SelfEval(datumToLiteral d)
        | Stx.List(items, None, _) -> BoundDatum.Compound(items |> List.map stxToDatum)
        | Stx.List(items, Some tail, _) -> BoundDatum.Pair(items |> List.map stxToDatum, stxToDatum tail)
        | Stx.Closure(inner, _, _) -> stxToDatum inner
        | Stx.Vec(items, _) ->
            BoundDatum.SelfEval(BoundLiteral.Vector(items |> List.map stxToDatum))

    /// Parse formal parameters from a Stx node into a `BoundFormals` value.
    let rec parseFormals (stx: Stx) (ctx: ExpandCtx) : BoundFormals =
        match stx with
        | Stx.Id(name, _) -> BoundFormals.Simple name
        | Stx.List(items, None, _) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc
                            "expected identifier in formals"

                        None)

            BoundFormals.List names
        | Stx.List(items, Some tail, loc) ->
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
            | Stx.List([ StxId(name, _); init ], _, _) -> Some(name, init)
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc
                    "ill-formed binding specification"

                None

        match stx with
        | Stx.List(items, _, _) -> items |> List.choose parseOne
        | Stx.Closure(inner, _, _) -> parseBindingSpecs inner ctx
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedBinding other.Loc "expected binding list"
            []

    // ── Variable lookup with capture tracking ─────────────────────────────

    /// Resolve a variable name to its (potentially capture-wrapped) StorageRef.
    /// Uses Idents to distinguish same-named variables from different scopes,
    /// enabling hygienic macro expansion.
    let resolveVar
        (name: string)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : StorageRef option =
        match Map.tryFind name scope with
        | Some id ->
            match id with
            | StxBinding.Macro _ | StxBinding.Special _ ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc
                    $"keyword '{name}' used in value position"
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
            ExpandCtx.emitError ctx Diag.undefinedSymbol loc $"unbound identifier '{name}'"
            None

    /// Look up a name in the scope, tracking captures for variables from
    /// outer lambda scopes.
    let lookupVar
        (name: string)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr =
        match resolveVar name loc scope ctx with
        | Some storage -> BoundExpr.Load storage
        | None -> BoundExpr.Error

    // ── Main recursive expander ───────────────────────────────────────────

    /// Expand a single Stx node in the given scope.
    let rec expand (stx: Stx) (scope: StxEnvironment) (ctx: ExpandCtx) : BoundExpr =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            // Syntactic closure: expand `inner` in the definition-site scope.
            // The closed scope is the authoritative source of Idents for
            // any identifiers present in it, which is the key hygiene mechanism:
            // macro-introduced `temp` (Ident N) and user's `temp` (Ident M)
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

        | Stx.Datum(d, _) -> BoundExpr.Literal(datumToLiteral d)

        | Stx.Vec(items, _) ->
            BoundExpr.Literal(BoundLiteral.Vector(items |> List.map stxToDatum))

        | Stx.List(_, Some _, loc) ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "dotted pair in expression position"
            BoundExpr.Error

        | Stx.List([], None, _) -> BoundExpr.Literal BoundLiteral.Null

        | Stx.List(head :: args, None, loc) -> expandForm head args loc scope ctx

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
        | Some(StxBinding.Macro macro) -> expandMacro macro (Stx.List(head :: args, None, loc)) scope ctx
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
        (scope: StxEnvironment)
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

    // ── Define ───────────────────────────────────────────────────────────

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

        | Stx.List(StxId(name, _) :: formals, None, _) :: body ->
            // (define (name formals...) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            let lambdaExpr = expandLambda (Stx.List(formals, None, loc) :: body) loc scope' ctx
            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | Stx.List(StxId(name, _) :: formals, Some tail, _) :: body ->
            // (define (name formals... . rest) body...)
            let _id, storage, scope' = ExpandCtx.mintVar ctx name scope
            let lambdaExpr = expandLambda (Stx.List(formals, Some tail, loc) :: body) loc scope' ctx
            BoundExpr.Store(storage, Some lambdaExpr), scope'

        | _ -> illFormed ()

    // ── Let forms ────────────────────────────────────────────────────────

    and expandLet
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
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
                    let id = Ident.fresh ()
                    name, id, storage, initExpr)

            // Now register all bindings and extend the scope for the body.
            let innerScope =
                stores
                |> List.fold
                    (fun s (name, id, storage, _) ->
                        ctx.BindingMap <-
                            Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

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

    and expandLetStar
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
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
    and checkStxForUninitRefs
        (uninitIds: Ident list)
        (stx: Stx)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        =
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

        walk scope stx

    // ── define-syntax / let-syntax ───────────────────────────────────────

    and expandDefineSyntax
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
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
        (scope: StxEnvironment)
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
            // within this call.  Macro Idents added to ctx.BindingMap here
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
        (defScope: StxEnvironment)
        (ctx: ExpandCtx)
        : SyntaxTransformer option =
        MacrosNew.makeSyntaxTransformer name stx defScope ctx.Diagnostics stx.Loc


    // ── Macro expansion ───────────────────────────────────────────────────

    and expandMacro
        (macro: SyntaxTransformer)
        (form: Stx)
        (callScope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr =
        match macro form callScope with
        | Result.Ok transcribed ->
            expand transcribed callScope ctx
        | Result.Error msg ->
            ExpandCtx.emitError ctx Diag.expandError form.Loc msg
            BoundExpr.Error


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
        | Stx.List(Stx.Id("only", _) :: fromSet :: filters, _, _) ->
            ImportSet.Only(stxToImportSet diags fromSet, getNames filters)
        | Stx.List(Stx.Id("except", _) :: fromSet :: filters, _, _) ->
            ImportSet.Except(stxToImportSet diags fromSet, getNames filters)
        | Stx.List([ Stx.Id("prefix", _); fromSet; Stx.Id(prefix, _) ], _, _) ->
            ImportSet.Prefix(stxToImportSet diags fromSet, prefix)
        | Stx.List(Stx.Id("rename", _) :: fromSet :: renames, _, _) ->
            let parseRename =
                function
                | Stx.List([ Stx.Id(fr, _); Stx.Id(to_, _) ], _, _) ->
                    Some { SymbolRename.From = fr; To = to_ }
                | _ -> None

            ImportSet.Renamed(stxToImportSet diags fromSet, renames |> List.choose parseRename)
        | Stx.List(parts, _, _) ->
            let name = getNames parts
            if List.isEmpty name then ImportSet.Error else ImportSet.Plain name
        | _ -> ImportSet.Error

    // ── Library name parsing ──────────────────────────────────────────────

    and private stxToLibraryName (stx: Stx) (ctx: ExpandCtx) : string list option =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | Stx.Closure(Stx.Id(n, _), _, _) -> Some n
            | Stx.Datum(StxDatum.Number n, _) -> Some(string (int n))
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedForm other.Loc
                    "expected identifier in library name"

                None

        match stx with
        | Stx.Closure(inner, _, _) -> stxToLibraryName inner ctx
        | Stx.List(parts, _, _) ->
            let names = List.choose getName parts
            if List.length names = List.length parts then Some names else None
        | other ->
            ExpandCtx.emitError ctx Diag.illFormedForm other.Loc "expected list for library name"
            None

    // ── define-library ────────────────────────────────────────────────────

    and expandLibrary
        (args: Stx list)
        (loc: TextLocation)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
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
                | Stx.List(Stx.Id("export", _) :: exports, _, _) ->
                    let newExports =
                        exports
                        |> List.choose (function
                            | Stx.Id(n, _) -> Some(n, n)
                            | Stx.Closure(Stx.Id(n, _), _, _) -> Some(n, n)
                            | Stx.List(
                                [ Stx.Id("rename", _); Stx.Id(intName, _); Stx.Id(extName, _) ],
                                _,
                                _) ->
                                Some(extName, intName)
                            | other ->
                                ExpandCtx.emitError innerCtx Diag.illFormedForm other.Loc
                                    "invalid export element"

                                None)

                    (innerScope, importAcc, bodyAcc, exportsAcc @ newExports)

                | Stx.List(Stx.Id("import", _) :: importArgs, _, declLoc) ->
                    let expr, newScope = expandImport importArgs declLoc innerScope innerCtx
                    (newScope, importAcc @ [ expr ], bodyAcc, exportsAcc)

                | Stx.List(Stx.Id("begin", _) :: beginBody, _, _) ->
                    let exprs, newScope = expandSeq beginBody innerScope innerCtx
                    (newScope, importAcc, bodyAcc @ exprs, exportsAcc)

                | Stx.List(Stx.Id(kw, _) :: _, _, declLoc) ->
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
                    (fun acc k binding ->
                        match binding with
                        | StxBinding.Macro _ -> Map.add k binding acc
                        | _ -> acc)
                    StxEnvironment.builtin

            let (finalInnerScope, importExprs, bodyExprs, exportedPairs) =
                List.fold folder (initInnerScope, [], [], []) declarations

            let exports =
                exportedPairs
                |> List.choose (fun (extName, intName) ->
                    match Map.tryFind intName finalInnerScope with
                    | Some(StxBinding.Variable id) ->
                        match Map.tryFind id innerCtx.BindingMap with
                        | Some(Variable(storage, _)) -> Some(extName, storage)
                        | None ->
                            ExpandCtx.emitError ctx Diag.illFormedForm loc
                                $"exported name '{intName}' is not defined in library"

                            None
                    | Some _ ->
                        ExpandCtx.emitError ctx Diag.illFormedForm loc
                            $"'{intName}' is a keyword and cannot be exported"

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
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr * StxEnvironment =
        let folder (currentScope: StxEnvironment, exprs: BoundExpr list) importStx =
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
                                let id = Ident.fresh ()

                                ctx.BindingMap <-
                                    Map.add id (Variable(storage, ctx.LambdaDepth)) ctx.BindingMap

                                Map.add name (StxBinding.Variable id) s)
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
    and tryExpandBinding
        (stx: Stx)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : (BoundExpr * StxEnvironment) option =
        match stx with
        | Stx.Closure(inner, closedScope, _) ->
            tryExpandBinding inner closedScope ctx
        | Stx.List(head :: args, _, loc) ->
            match resolveHead head scope ctx with
            | Some(StxBinding.Special SpecialFormKind.Define) ->
                Some(expandDefine args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.DefineSyntax) ->
                Some(expandDefineSyntax args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.Begin) ->
                let exprs, scope' = expandSeq args scope ctx
                Some(BoundExpr.Seq exprs, scope')
            | Some(StxBinding.Special SpecialFormKind.Import) ->
                Some(expandImport args loc scope ctx)
            | Some(StxBinding.Special SpecialFormKind.DefineLibrary) ->
                Some(expandLibrary args loc scope ctx)
            | Some(StxBinding.Macro transformer) ->
                // Macro application in definition context: transcribe and re-process
                // so that expansions producing `(define ...)` etc. are spliced in.
                let form = Stx.List(head :: args, None, loc)

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
    and expandSeq
        (stxs: Stx list)
        (scope: StxEnvironment)
        (ctx: ExpandCtx)
        : BoundExpr list * StxEnvironment =
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
            |> Seq.map (Stx.ofExpr ctx.Registry docId)
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
                        |> Seq.map (Stx.ofExpr ctx.Registry docId)
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
            |> Option.map (Stx.ofExpr ctx.Registry docId)
            |> Option.toList

        let exprs, _ = Expander.expandSeq stxs scope ctx
        exprs
