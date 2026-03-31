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

// ──────────────────────────────── Syntax env, Stx, and macro pattern/template

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

// Template types — mutually recursive with each other and with
// SyntaxTransformer / SyntaxEnv / SyntaxBinding / Stx.

/// A macro template element — either a single sub-template or a repeated one.
[<RequireQualifiedAccess>]
type StxTemplateElement =
    | Template of StxTemplate
    | Repeated of StxTemplate

/// Template for producing new `Stx` syntax in macro rules.
/// `Quoted` carries the literal `Stx` node captured at definition time; on
/// transcription it is wrapped in a `Stx.Closure` with the definition-site env
/// for hygienic identifier capture.
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

/// What a name maps to at syntax time.
and SyntaxBinding =
    | SpecialForm of SpecialFormKind
    | MacroDef of SyntaxTransformer
    | Variable of StorageRef

/// A macro transformer: patterns, templates, and definition-site environment.
and SyntaxTransformer =
    { Patterns: (StxPattern * StxTemplate) list
      DefEnv: SyntaxEnv
      DefLoc: TextLocation }

/// Immutable map from raw names to their current syntactic meaning.
and SyntaxEnv = Map<string, SyntaxBinding>

// ─────────────────────────────────────────────────────────────────────── Stx

/// The intermediate hygiene-annotated syntax type.
/// Every node carries a source location; `Closure` nodes carry an explicit env.
and [<RequireQualifiedAccess>] Stx =
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
    /// A vector literal — `#(e1 e2 … en)`. Elements are already converted to Stx
    /// and are treated as data (never evaluated as code).
    | Vec of items: Stx list * loc: TextLocation
    /// A byte-vector literal — `#u8(b1 b2 … bn)`. Each element is a pre-parsed
    /// `Result<byte, string>`; errors are deferred until `expand` time so that
    /// diagnostics can be emitted with a `DiagnosticBag`.
    | ByteVec of bytes: Result<byte, string> list * loc: TextLocation

    /// Extract the location from any node.
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

// ────────────────────────────────────────────────────────────── Pattern types

/// A captured pattern variable: the matched Stx paired with its captured env.
and PatternCapture =
    | Single of stx: Stx * env: SyntaxEnv
    | Repeated of (Stx * SyntaxEnv) list

/// All captured bindings from a successful pattern match.
and PatternBindings = Map<string, PatternCapture>

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
            // Vector literals: elements are data, never code.
            let items = v.Body |> Seq.map (ofExpr reg docId) |> List.ofSeq
            Stx.Vec(items, loc)
        | ByteVecNode b ->
            // Byte-vector literal: pre-parse each element; defer error reporting
            // to expand time where a DiagnosticBag is available.
            let bytes = b.Body |> List.map (fun bv -> bv.Value)
            Stx.ByteVec(bytes, loc)

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
      mutable Libraries: LibrarySignature<StorageRef> list
      Parent: ExpandCtx option }

module ExpandCtx =

    /// Create a root context for global-scope expansion.
    let createGlobal (registry: SourceRegistry) (mangledName: string) (libraries: LibrarySignature<StorageRef> list) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
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
          Diagnostics = parent.Diagnostics
          Registry = parent.Registry
          MangledName = parent.MangledName
          IsGlobal = false
          Libraries = parent.Libraries
          Parent = Some parent }

    /// Create a child context for a library body.
    /// The library gets its own global scope with a dedicated mangled name.
    let childForLibrary (parent: ExpandCtx) (mangledName: string) =
        { LocalCount = 0
          Captures = []
          HasDynEnv = false
          Diagnostics = parent.Diagnostics
          Registry = parent.Registry
          MangledName = mangledName
          IsGlobal = true
          Libraries = parent.Libraries
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

// ─────────────────────────────────────────────────────────────── MacrosNew

/// Parse `(syntax-rules ...)` bodies from `Stx` nodes into `SyntaxTransformer`
/// values.  This module replaces the old CST-based `Macros` module for the
/// new expander pipeline; only the functions consumed by `Expander` are public.
module MacrosNew =

    // Local aliases to avoid shadowing by LibraryDeclaration.Result.Error / ImportSet.Result.Error

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
        match stx with
        | Stx.List(items, _) ->
            items
            |> List.map (fun s ->
                match s with
                | Stx.Id(name, _) -> Result.Ok name
                | _ -> Result.Error "literal list must contain identifiers")
            |> List.fold
                (fun acc r ->
                    match acc, r with
                    | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ x ])
                    | Result.Error e, _ -> Result.Error e
                    | _, Result.Error e -> Result.Error e)
                (Result.Ok [])
        | _ -> Result.Error "expected list of literals"

    /// Parse `(pats...)` or `(pats... . tail)` form body into a list of
    /// patterns, detecting ellipsis.  The first element (macro keyword) is
    /// already dropped by the caller.
    let rec parseFormBody
        (kw: string)
        (lits: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<StxPattern list * StxPattern option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailPat =
                    tail |> Option.map (fun t -> parsePattern kw lits t)

                match tailPat with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parsePattern kw lits single with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) []
            | current :: (Stx.Id("...", _) :: rest) ->
                match parsePattern kw lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok inner -> loop (StxPattern.Repeat inner :: acc) rest
            | current :: rest ->
                match parsePattern kw lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) rest

        loop [] items

    /// Parse a single `Stx` node as a macro pattern.
    and parsePattern (kw: string) (lits: string list) (stx: Stx) : Result<StxPattern, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parsePattern kw lits inner
        | Stx.Const(lit, _) -> Result.Ok(StxPattern.Constant lit)
        | Stx.Id("_", _) -> Result.Ok StxPattern.Underscore
        | Stx.Id("...", _) -> Result.Error "unexpected ellipsis in pattern"
        | Stx.Id(name, _) ->
            if List.contains name lits then
                Result.Ok(StxPattern.Literal name)
            else
                Result.Ok(StxPattern.Variable name)
        | Stx.List(items, _) ->
            match items with
            | Stx.Id(head, _) :: rest when head = kw ->
                // The keyword at the head of each rule pattern becomes a Variable
                // binding so that recursive templates like `(or e2 ...)` can
                // refer back to the macro keyword via substitution and re-expand
                // it at the call site.
                match parseFormBody kw lits rest None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(StxPattern.Form(StxPattern.Variable kw :: pats))
                | Result.Ok(pats, Some tail) -> Result.Ok(StxPattern.DottedForm(StxPattern.Variable kw :: pats, tail))
            | _ ->
                match parseFormBody kw lits items None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(StxPattern.Form pats)
                | Result.Ok(pats, Some tail) -> Result.Ok(StxPattern.DottedForm(pats, tail))
        | Stx.Dotted(items, tail, _) ->
            match parseFormBody kw lits items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(pats, tailOpt) ->
                let tailPat = tailOpt |> Option.defaultValue StxPattern.Underscore
                Result.Ok(StxPattern.DottedForm(pats, tailPat))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parsePattern kw lits)

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

    /// Detect ellipsis in a template form body, like `parseFormBody` for patterns.
    let rec parseTemplateFormBody
        (bound: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<StxTemplateElement list * StxTemplate option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailTmpl = tail |> Option.map (fun t -> parseTemplate bound t)

                match tailTmpl with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parseTemplate bound single with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Template tmpl :: acc) []
            | current :: (Stx.Id("...", _) :: rest) ->
                match parseTemplate bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Repeated tmpl :: acc) rest
            | current :: rest ->
                match parseTemplate bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (StxTemplateElement.Template tmpl :: acc) rest

        loop [] items

    /// Parse a single `Stx` node as a macro template.
    and parseTemplate (bound: string list) (stx: Stx) : Result<StxTemplate, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parseTemplate bound inner
        | Stx.Id(name, _) ->
            if List.contains name bound then
                Result.Ok(StxTemplate.Subst name)
            else
                Result.Ok(StxTemplate.Quoted stx)
        | Stx.List(items, loc) ->
            match parseTemplateFormBody bound items None with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, None) -> Result.Ok(StxTemplate.Form(loc, elems))
            | Result.Ok(elems, Some tail) -> Result.Ok(StxTemplate.DottedForm(elems, tail))
        | Stx.Dotted(items, tail, loc) ->
            match parseTemplateFormBody bound items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, tailOpt) ->
                let tailTmpl = tailOpt |> Option.defaultValue (StxTemplate.Quoted stx)
                Result.Ok(StxTemplate.DottedForm(elems, tailTmpl))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parseTemplate bound)

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
            // Literal datum (constant, quoted, etc.) — preserve it with its
            // definition-site syntax object so transcribe can wrap it in a
            // Closure and preserve hygiene.
            Result.Ok(StxTemplate.Quoted other)

    /// Parse one `(pattern template)` transformer arm.
    let parseTransformer
        (kw: string)
        (lits: string list)
        (stx: Stx)
        : Result<StxPattern * StxTemplate, string> =
        match stx with
        | Stx.List([ pat; tmpl ], _) ->
            match parsePattern kw lits pat with
            | Result.Error e -> Result.Error e
            | Result.Ok patParsed ->
                let bound = findBound patParsed
                match parseTemplate bound tmpl with
                | Result.Error e -> Result.Error e
                | Result.Ok tmplParsed -> Result.Ok(patParsed, tmplParsed)
        | _ -> Result.Error "each syntax-rules rule must be (pattern template)"

    /// Parse the body of `(syntax-rules (lits...) rule...)`.
    let parseSyntaxRulesBody
        (kw: string)
        (litsStx: Stx)
        (rules: Stx list)
        (defEnv: SyntaxEnv)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxTransformer option =
        match parseLiteralList litsStx with
        | Result.Error e ->
            diag.Emit Diag.invalidMacro loc e
            None
        | Result.Ok lits ->
            let results = rules |> List.map (parseTransformer kw lits)

            let patterns =
                results
                |> List.choose (function
                    | Result.Ok pair -> Some pair
                    | Result.Error e ->
                        diag.Emit Diag.invalidMacro loc e
                        None)

            Some
                { Patterns = patterns
                  DefEnv = defEnv
                  DefLoc = loc }

    /// Public entry point: parse a `(syntax-rules ...)` form into a transformer.
    let parseSyntaxRulesStx
        (name: string)
        (stx: Stx)
        (defEnv: SyntaxEnv)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : SyntaxTransformer option =
        match stx with
        | Stx.List(Stx.Id("syntax-rules", _) :: litsStx :: rules, _) ->
            parseSyntaxRulesBody name litsStx rules defEnv diag loc
        | _ ->
            diag.Emit Diag.invalidMacro loc "expected (syntax-rules (literals...) rules...)"
            None

// ─────────────────────────────────────────────────────────────── Expander

module private Expander =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Peel any `Stx.Closure` wrappers and look up the head binding in the
    /// appropriate environment.  Returns `None` for non-identifier heads.
    let rec resolveHead (stx: Stx) (env: SyntaxEnv) : SyntaxBinding option =
        match stx with
        | Stx.Id(name, _) -> Map.tryFind name env
        | Stx.Closure(inner, closedEnv, _) ->
            // Mirror the expand Stx.Closure fallback: if the inner identifier
            // is not bound at the definition site, resolve it in the ambient
            // (call-site) env instead.  This is what makes recursive macros
            // like `(my-or e2 ...)` and `(alist z ...)` find their own MacroDef.
            match inner with
            | Stx.Id(name, _) when not (Map.containsKey name closedEnv) ->
                resolveHead inner env
            | _ -> resolveHead inner closedEnv
        | _ -> None

    /// Active pattern: extract an identifier name+location from a Stx node,
    /// peeling any `Stx.Closure` hygiene wrappers.  Use this wherever special
    /// forms need to pull an identifier out of a sub-form that may have come
    /// through macro transcription.
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
            // Silently discard invalid byte values when converting to datum;
            // errors were/will be reported in expression context.
            let bs = bytes |> List.choose (function Ok b -> Some b | Result.Error _ -> None)
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
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"
                        None)

            BoundFormals.List names
        | Stx.Dotted(items, tail, loc) ->
            let names =
                items
                |> List.choose (function
                    | StxId(n, _) -> Some n
                    | other ->
                        ExpandCtx.emitError ctx Diag.invalidFormals other.Loc "expected identifier in formals"
                        None)

            match tail with
            | StxId(rest, _) -> BoundFormals.DottedList(names, rest)
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
            | Stx.List([ StxId(name, _); init ], _) -> Some(name, init)
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
            // Exception: for simple identifiers not found in the def-site env,
            // fall back to the call-site env.  This allows macro-introduced
            // names (e.g. `tmp` in `(let ((tmp x)) ... tmp ...)`  inside a
            // template) to be visible once they are bound at call-site — a
            // necessary trade-off until gensym-based hygiene is implemented.
            match inner with
            | Stx.Id(name, _) when not (Map.containsKey name closedEnv) ->
                expand inner env ctx
            | _ -> expand inner closedEnv ctx

        | Stx.Id(name, loc) -> lookupVar name loc env ctx

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
            | [ StxId(name, idLoc); value ] ->
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

        | SpecialFormKind.Import ->
            // In expression position: import but discard the env extension.
            let expr, _ = expandImport args loc env ctx
            expr

        | SpecialFormKind.DefineLibrary ->
            // In expression position — process but discard the env extension.
            fst (expandLibrary args loc env ctx)

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
        | [ StxId(name, _) ] ->
            // (define id) — uninitialized
            let storage = ExpandCtx.mintStorage ctx name
            let env' = Map.add name (Variable storage) env
            BoundExpr.Store(storage, None), env'

        | [ StxId(name, _); value ] ->
            // (define id value)
            let storage = ExpandCtx.mintStorage ctx name
            // Add name to env before expanding value so self-recursion works.
            let env' = Map.add name (Variable storage) env
            let boundValue = expand value env' ctx
            BoundExpr.Store(storage, Some boundValue), env'

        | Stx.List(StxId(name, _) :: formals, _) :: body ->
            // (define (name formals...) body...)
            let storage = ExpandCtx.mintStorage ctx name
            let env' = Map.add name (Variable storage) env
            let lambdaExpr = expandLambda (Stx.List(formals, loc) :: body) loc env' ctx
            BoundExpr.Store(storage, Some lambdaExpr), env'

        | Stx.Dotted(StxId(name, _) :: formals, tail, _) :: body ->
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
        | [ StxId(name, _); rulesStx ] ->
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
        MacrosNew.parseSyntaxRulesStx name stx defEnv ctx.Diagnostics stx.Loc

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
    and matchPattern (pat: StxPattern) (stx: Stx) (env: SyntaxEnv) : PatternBindings option =

        match pat, stx with
        | StxPattern.Underscore, _ -> Some Map.empty

        | StxPattern.Variable v, s ->
            Some(Map.ofList [ v, Single(s, env) ])

        | StxPattern.Literal lit, Stx.Id(name, _) ->
            if name = lit then Some Map.empty else None

        | StxPattern.Literal lit, Stx.Closure(inner, closedEnv, _) ->
            matchPattern (StxPattern.Literal lit) inner closedEnv

        | StxPattern.Constant patLit, Stx.Const(stxLit, _) ->
            if patLit = stxLit then Some Map.empty else None

        | StxPattern.Form pats, Stx.List(items, _) ->
            matchPatternList pats None items None env

        | StxPattern.DottedForm(pats, tailPat), Stx.Dotted(items, tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) env

        | StxPattern.Form pats, Stx.Closure(inner, closedEnv, _) ->
            matchPattern (StxPattern.Form pats) inner closedEnv

        | StxPattern.DottedForm(pats, tailPat), Stx.Closure(inner, closedEnv, _) ->
            matchPattern (StxPattern.DottedForm(pats, tailPat)) inner closedEnv

        | StxPattern.Vec pats, Stx.Vec(items, _) ->
            matchPatternList pats None items None env

        | _ -> None

    and matchPatternList
        (pats: StxPattern list)
        (tailPat: StxPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (env: SyntaxEnv)
        : PatternBindings option =
        match pats with
        | StxPattern.Repeat inner :: restPats ->
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
        (inner: StxPattern)
        (restPats: StxPattern list)
        (tailPat: StxPattern option)
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

    and constantMatchesLiteral (_pat: ConstantValue) (_lit: BoundLiteral) : bool = false // removed — superseded by StxPattern

    // ── Transcription ─────────────────────────────────────────────────────

    and transcribe
        (template: StxTemplate)
        (bindings: PatternBindings)
        (defEnv: SyntaxEnv)
        (loc: TextLocation)
        : Stx =
        match template with
        | StxTemplate.Subst name ->
            match Map.tryFind name bindings with
            | Some(Single(stx, capturedEnv)) -> Stx.Closure(stx, capturedEnv, loc)
            | Some(Repeated _) -> failwith $"ellipsis variable '{name}' used outside a repeat context"
            | None -> failwith $"template variable '{name}' not bound"

        | StxTemplate.Quoted stx ->
            // Literal from the definition site — wrap it in the definition-site
            // environment so that any identifiers inside resolve hygienically.
            Stx.Closure(stx, defEnv, loc)

        | StxTemplate.Form(_, elements) ->
            let children = elements |> List.collect (transcribeElement bindings defEnv loc)
            Stx.List(children, loc)

        | StxTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defEnv loc)
            Stx.Dotted(children, transcribe tail bindings defEnv loc, loc)

        | StxTemplate.Vec elements ->
            let children = elements |> List.collect (transcribeElement bindings defEnv loc)
            Stx.Vec(children, loc)

    and transcribeElement
        (bindings: PatternBindings)
        (defEnv: SyntaxEnv)
        (loc: TextLocation)
        (elem: StxTemplateElement)
        : Stx list =
        match elem with
        | StxTemplateElement.Template t -> [ transcribe t bindings defEnv loc ]
        | StxTemplateElement.Repeated t -> transcribeRepeated t bindings defEnv loc

    and transcribeRepeated
        (template: StxTemplate)
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

    // ── Import set parsing  ───────────────────────────────────────────────

    /// Parse a Stx node into an `ImportSet` (mirrors Libraries.parseImportDeclaration).
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
                | Stx.List([ Stx.Id(fr, _); Stx.Id(to_, _) ], _) -> Some { SymbolRename.From = fr; To = to_ }
                | _ -> None

            ImportSet.Renamed(stxToImportSet diags fromSet, renames |> List.choose parseRename)
        | Stx.List(parts, _) ->
            let name = getNames parts
            if List.isEmpty name then ImportSet.Error else ImportSet.Plain name
        | _ -> ImportSet.Error

    // ── Library name parsing ──────────────────────────────────────────────

    /// Parse a `(name-part ...)` list Stx node into a library name.
    and private stxToLibraryName (stx: Stx) (ctx: ExpandCtx) : string list option =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | Stx.Closure(Stx.Id(n, _), _, _) -> Some n
            | Stx.Const(BoundLiteral.Number n, _) -> Some(string (int n))
            | other ->
                ExpandCtx.emitError ctx Diag.illFormedForm other.Loc "expected identifier in library name"
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

    /// Expand a top-level `(define-library <name> <decl> ...)` form.
    ///
    /// Returns the `BoundExpr.Library` node and the unchanged outer `SyntaxEnv`
    /// (the library is not imported automatically; callers use `import` later).
    /// Also mutates `ctx.Libraries` so subsequent forms in the same file can
    /// import the newly-defined library.
    and expandLibrary
        (args: Stx list)
        (loc: TextLocation)
        (env: SyntaxEnv)
        (ctx: ExpandCtx)
        : BoundExpr * SyntaxEnv =
        match args with
        | nameStx :: declarations ->
            let name = stxToLibraryName nameStx ctx |> Option.defaultValue []
            let mangledName = name |> String.concat "::"

            // Fresh inner context — own global scope for the library body.
            let innerCtx = ExpandCtx.childForLibrary ctx mangledName

            // Process each library declaration in order, threading the inner env.
            let folder (innerEnv, importAcc, bodyAcc, exportsAcc) decl =
                // Peel any syntactic-closure wrapper.
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
                            | Stx.List([ Stx.Id("rename", _); Stx.Id(intName, _); Stx.Id(extName, _) ], _) ->
                                Some(extName, intName)
                            | other ->
                                ExpandCtx.emitError innerCtx Diag.illFormedForm other.Loc "invalid export element"
                                None)

                    (innerEnv, importAcc, bodyAcc, exportsAcc @ newExports)

                | Stx.List(Stx.Id("import", _) :: importArgs, declLoc) ->
                    let expr, newEnv = expandImport importArgs declLoc innerEnv innerCtx
                    (newEnv, importAcc @ [ expr ], bodyAcc, exportsAcc)

                | Stx.List(Stx.Id("begin", _) :: beginBody, _) ->
                    let exprs, newEnv = expandSeq beginBody innerEnv innerCtx
                    (newEnv, importAcc, bodyAcc @ exprs, exportsAcc)

                | Stx.List(Stx.Id(kw, _) :: _, declLoc) ->
                    ExpandCtx.emitError ctx Diag.illFormedForm declLoc
                        $"unrecognised library declaration '{kw}'"

                    (innerEnv, importAcc, bodyAcc, exportsAcc)

                | other ->
                    ExpandCtx.emitError ctx Diag.illFormedForm other.Loc "ill-formed library declaration"
                    (innerEnv, importAcc, bodyAcc, exportsAcc)

            // Library body starts with just the keyword environment, plus any
            // MacroDef bindings (builtin macros) inherited from the outer scope.
            // Variable bindings from the outer scope are intentionally NOT
            // inherited — the library must import everything it uses.
            let initInnerEnv =
                env
                |> Map.fold
                    (fun acc k v ->
                        match v with
                        | MacroDef _ -> Map.add k v acc
                        | _ -> acc)
                    SyntaxEnv.builtin

            let (finalInnerEnv, importExprs, bodyExprs, exportedPairs) =
                List.fold folder (initInnerEnv, [], [], []) declarations

            // Resolve exported names to their storage references.
            let exports =
                exportedPairs
                |> List.choose (fun (extName, intName) ->
                    match Map.tryFind intName finalInnerEnv with
                    | Some(Variable storage) -> Some(extName, storage)
                    | Some _ ->
                        ExpandCtx.emitError ctx Diag.illFormedForm loc
                            $"'{intName}' is a keyword and cannot be exported"

                        None
                    | None ->
                        ExpandCtx.emitError ctx Diag.illFormedForm loc
                            $"exported name '{intName}' is not defined in library"

                        None)

            // Register the new library so subsequent `(import ...)` can find it.
            ctx.Libraries <- { LibraryName = name; Exports = exports } :: ctx.Libraries

            let body = ExpandCtx.intoBody innerCtx (importExprs @ bodyExprs)
            (BoundExpr.Library(name, mangledName, exports, body), env)

        | [] ->
            ExpandCtx.emitError ctx Diag.illFormedForm loc "ill-formed 'define-library'"
            (BoundExpr.Error, env)

    /// Expand an `import` form. Returns the list of `BoundExpr.Import` nodes
    /// and an extended `SyntaxEnv` with all imported names in scope.
    and expandImport (args: Stx list) (loc: TextLocation) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr * SyntaxEnv =
        let folder (currentEnv: SyntaxEnv, exprs: BoundExpr list) importStx =
            let importSet = stxToImportSet ctx.Diagnostics importStx

            match Libraries.resolveImport ctx.Libraries importSet with
            | Ok library ->
                let newEnv =
                    library.Exports
                    |> List.fold
                        (fun e (name, storage) ->
                            match storage with
                            | StorageRef.Macro _ ->
                                // Old-format macro entries are not meaningful to
                                // the new expander.  Builtins are seeded into the
                                // initial SyntaxEnv as MacroDef; any other Macro
                                // storage is simply ignored here.
                                e
                            | _ -> Map.add name (Variable storage) e)
                        currentEnv

                let mangledName = library.LibraryName |> String.concat "::"
                (newEnv, BoundExpr.Import mangledName :: exprs)
            | Result.Error msg ->
                ExpandCtx.emitError ctx Diag.illFormedForm loc msg
                (currentEnv, BoundExpr.Error :: exprs)

        let (finalEnv, revExprs) = List.fold folder (env, []) args
        (BoundExpr.Seq(List.rev revExprs), finalEnv)

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
            | Some(SpecialForm SpecialFormKind.Import) -> true
            | Some(SpecialForm SpecialFormKind.DefineLibrary) -> true
            | _ -> false
        | Stx.Closure(inner, closedEnv, _) -> isBindingForm inner closedEnv
        | _ -> false

    /// Try to expand a form as a binding-level definition.
    /// Returns `Some(expr, extendedEnv)` for `define` / `define-syntax` / `import`,
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
            | Some(SpecialForm SpecialFormKind.Import) ->
                Some(expandImport args loc env ctx)
            | Some(SpecialForm SpecialFormKind.DefineLibrary) ->
                Some(expandLibrary args loc env ctx)
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
