# Syntactic Closures Expander

This document describes the planned implementation of a hygienic macro
expander for Feersum using the *Syntactic Closures* model (Bawden & Rees,
1988). The implementation lives entirely in
`src/Feersum.CompilerServices/Binding/Expand.fs` and is developed in isolation
alongside the existing binder, with integration deferred until the new pass is
feature-complete.

It draws on the concepts in [Expansion Work Stack](./expansion-workstack/) but
replaces the stamps-based hygiene mechanism and the explicit work stack with a
richer intermediate syntax type (`Stx`) and a set of mutually recursive
functions that thread environments structurally.

## Background

### Limitations of the Current Binder

The current macro expansion in `Binder.fs` / `Macros.fs` is not hygienic.
When `macroExpand` transcribes a template, the resulting `Expression` nodes
are fed back into `bindInContext` with the **caller's** ambient scope.
Macro-introduced identifiers therefore resolve in the call site, not the
definition site, which violates hygiene.

### What Are Syntactic Closures?

A *syntactic closure* is a pair of `(syntax, SyntaxEnv)`. It says "expand
this piece of syntax, but resolve any free identifiers in *this* environment,
not whatever the ambient environment happens to be at the call site."

The critical operations are:

- **Capture** — wrap call-site syntax in a closure over the call-site env so
  it remains anchored to the caller's scope when substituted into a template.
- **Close** — wrap template literals in a closure over the macro's
  definition-site env so they never capture caller bindings.

No marks, stamps, or rename tables are needed. The environment is everything.

### Why a Recursive Approach over an Explicit Work Stack

The [Expansion Work Stack](./expansion-workstack/) design uses an explicit
`Stack<WorkItem>` to make the expander iterative. This is correct but complex:
assembling compound `BoundExpr` nodes (lambda bodies, let forms, etc.) requires
either `Continuation` items carrying closures or a secondary pending stack to
collect child results, making the control flow hard to follow.

Recursive functions express the same traversal more directly: the call stack
*is* the continuation, and compound forms are assembled by natural
`let`-binding of recursive results. F#'s default stack depth is ample for all
realistic Scheme programs. The recursive approach is therefore preferred for
the initial implementation, with a work-stack rewrite deferred if profiling
reveals stack pressure.

## Design

### Overview

```
Red/green tree (Firethorn, unchanged)
    ↓  typed CST (Expression, Tree.fs)
         ↓  Stx.ofExpr (Expand.fs)
       Stx (new hygiene-annotated tree)
         ↓  expand : Stx → SyntaxEnv → BoundExpr  (Expand.fs, recursive)
BoundExpr tree (existing type, unchanged)
    ↓  Lower.fs (unchanged)
    ↓  Emit.fs  (unchanged)
```

The pipeline adds one new intermediate representation, `Stx`, that sits
between `Expression` (the Firethorn-backed CST) and `BoundExpr` (the resolved
output). `Stx` mirrors the syntactic structure of a Scheme form but carries
source location on every node and may wrap any sub-tree in a `SyntaxEnv`
override (a syntactic closure). The expander walks `Stx` recursively,
threading a `SyntaxEnv`, and produces `BoundExpr` in a single fused pass —
no separate name-resolution step is needed afterward.

### `Stx` — The Intermediate Syntax Type

`Stx` has the same shape as the surface syntax but is self-contained: it does
not reference Firethorn internals and can be constructed freely by the macro
transcription step without allocating live red/green nodes.

```fsharp
/// Source location carried on every Stx node.
type StxLoc = { DocId: DocId; Range: SyntaxRange }

/// The core intermediate syntax type.
/// Every node carries a location; syntactic closures carry an explicit env.
[<RequireQualifiedAccess>]
type Stx =
    /// An identifier. The string is the raw, unresolved name.
    | Id      of name: string * loc: StxLoc
    /// A self-evaluating constant.
    | Const   of value: BoundLiteral * loc: StxLoc
    /// A proper list — `(e1 e2 … en)`.
    | List    of items: Stx list * loc: StxLoc
    /// A dotted pair — `(e1 e2 … . tail)`.
    | Dotted  of items: Stx list * tail: Stx * loc: StxLoc
    /// A quoted datum — `'datum`.
    | Quoted  of inner: Stx * loc: StxLoc
    /// A syntactic closure. When the expander encounters this node it switches
    /// to `env` before expanding `inner`, regardless of the ambient env.
    | Closure of inner: Stx * env: SyntaxEnv * loc: StxLoc
```

The `Closure` variant directly represents a syntactic closure: it pairs a
syntax node with the environment in which it should be expanded. The
expander's environment switching is therefore visible in the tree rather than
hidden in work-stack items or tree annotations.

`Stx` is a discriminated union (no inheritance): pattern matching is
exhaustive without any `ice` fallback needed.

#### Converting `Expression` to `Stx`

A thin `Stx.ofExpr : DocId → Expression → Stx` function wraps each
`Expression` variant into the corresponding `Stx` variant, preserving the
node's `SyntaxRange` as the location. This conversion is done once per
expression before expansion begins. No `Stx.Closure` nodes are introduced
here; they appear only during macro transcription.

```fsharp
let rec ofExpr (docId: DocId) (expr: Expression) : Stx =
    let loc = { DocId = docId; Range = expr.SyntaxRange }
    match expr with
    | SymbolNode  s -> Stx.Id(s.CookedValue, loc)
    | ConstantNode c ->
        Stx.Const(BoundLiteral.FromConstantValue c.Value, loc)
    | FormNode f ->
        let body = f.Body |> Seq.map (ofExpr docId) |> List.ofSeq
        match f.DottedTail with
        | None ->
            Stx.List(body, loc)
        | Some tail ->
            let t =
                tail.Body
                |> Option.map (ofExpr docId)
                |> Option.defaultValue (Stx.Const(BoundLiteral.Null, loc))
            Stx.Dotted(body, t, loc)
    | QuotedNode q ->
        let inner =
            q.Inner
            |> Option.map (ofExpr docId)
            |> Option.defaultValue (Stx.Const(BoundLiteral.Null, loc))
        Stx.Quoted(inner, loc)
    | _ ->
        Stx.Const(BoundLiteral.Null, loc)   // error recovery
```

### The Syntax Environment

```fsharp
/// Well-known built-in transformer kinds.
[<RequireQualifiedAccess>]
type SpecialFormKind =
    | If | Lambda | Define | Set | Begin | Quote
    | Let | LetStar | Letrec | LetrecStar
    | DefineSyntax | LetSyntax | LetrecSyntax
    | Import | DefineLibrary

/// What a name maps to at syntax time.
type SyntaxBinding =
    | SpecialForm of SpecialFormKind
    | MacroDef    of MacroTransformer   // user-defined transformer
    | Variable    of StorageRef         // resolved storage location

/// Immutable map from raw names to their current meaning.
type SyntaxEnv = Map<string, SyntaxBinding>
```

`SyntaxEnv` maps directly to `StorageRef` on the `Variable` branch, keeping
the expand + bind fused in one pass: the expander mints a `StorageRef` at
each binding site and records it in `SyntaxEnv`. When a later `Stx.Id`
reference is expanded, the `StorageRef` is already present — no second
resolution phase is required.

Immutable maps mean `SyntaxEnv` values can be snapshotted cheaply for macro
`DefEnv` fields and `Stx.Closure` nodes.

### Macro Transformers

```fsharp
type MacroTransformer =
    { Patterns  : (MacroPattern * MacroTemplate) list
      DefEnv    : SyntaxEnv   // SyntaxEnv captured at define-syntax time
      DefLoc    : StxLoc }
```

`DefEnv` is the full `SyntaxEnv` at the point `define-syntax` was processed.
Template literals are always expanded in this environment; pattern variable
substitutions carry their captured call-site environment as a `Stx.Closure`.

### The Expander

The main entry point is a family of mutually recursive functions. The
top-level driver converts `Expression` values to `Stx` and calls `expand`:

```fsharp
let expandProgram (prog: Tree.Program) (initialEnv: SyntaxEnv) : BoundExpr list =
    let stxs = prog.Body |> Seq.map (Stx.ofExpr prog.DocId) |> List.ofSeq
    let exprs, _ = expandSeq stxs initialEnv
    exprs
```

#### `expand` — Single Form

```fsharp
/// Expand a single Stx node in the given environment.
/// Returns a fully resolved BoundExpr.
let rec expand (stx: Stx) (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
    match stx with

    | Stx.Closure(inner, closedEnv, _) ->
        // Syntactic closure: switch to the closed-over environment.
        expand inner closedEnv ctx

    | Stx.Id(name, loc) ->
        match Map.tryFind name env with
        | Some(Variable storage) -> BoundExpr.Load storage
        | Some(SpecialForm _) | Some(MacroDef _) ->
            ctx.Error loc "keyword in value position"; BoundExpr.Error
        | None ->
            ctx.Error loc $"unbound identifier '{name}'"; BoundExpr.Error

    | Stx.Const(lit, _) ->
        BoundExpr.Literal lit

    | Stx.Quoted(inner, _) ->
        BoundExpr.Quoted(stxToDatum inner)

    | Stx.List([], _) ->
        BoundExpr.Literal BoundLiteral.Null

    | Stx.List(head :: args, loc) ->
        expandForm head args loc env ctx

    | Stx.Dotted _ ->
        // Dotted pairs in expression position are invalid.
        BoundExpr.Error
```

`Stx.Closure` is matched first; because `expand` then recurses on `inner`
with `closedEnv`, any chain of nested `Stx.Closure` wrappers is peeled
naturally.

#### `expandForm` — Head Dispatch

```fsharp
and expandForm (head: Stx) (args: Stx list) (loc: StxLoc)
               (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
    // Resolve the head through any closure layers to see its binding,
    // without fully expanding it.
    match resolveHead head env with
    | Some(SpecialForm kind) ->
        expandSpecialForm kind args loc env ctx
    | Some(MacroDef macro) ->
        expandMacro macro (Stx.List(head :: args, loc)) env ctx
    | _ ->
        // Ordinary application.
        let fn      = expand head env ctx
        let actuals = args |> List.map (fun a -> expand a env ctx)
        BoundExpr.Application(fn, actuals)
```

`resolveHead` peels `Stx.Closure` wrappers and looks up the outermost
`Stx.Id` in its closed-over environment, returning `Some SyntaxBinding` when
the head is an identifier, `None` otherwise.

#### `expandSeq` — Sequencing with Defines

Because `define` and `define-syntax` in a body extend the environment for
subsequent siblings, the sequencing function must thread environment changes
forward:

```fsharp
/// Expand a list of forms, threading env through any defines.
/// Returns the list of BoundExprs and the final SyntaxEnv.
and expandSeq (stxs: Stx list) (env: SyntaxEnv) (ctx: ExpandCtx)
              : BoundExpr list * SyntaxEnv =
    match stxs with
    | [] -> [], env
    | stx :: rest ->
        match tryExpandBinding stx env ctx with
        | Some(boundExpr, env') ->
            // This form was a define/define-syntax; continue in extended env.
            let restExprs, finalEnv = expandSeq rest env' ctx
            boundExpr :: restExprs, finalEnv
        | None ->
            let expr = expand stx env ctx
            let restExprs, finalEnv = expandSeq rest env ctx
            expr :: restExprs, finalEnv
```

`tryExpandBinding` returns `Some(BoundExpr, extendedEnv)` for
`define` / `define-syntax` forms, and `None` for all other forms.

#### `expandSpecialForm` — Special Form Dispatch

Each special form is handled inline or in a small named helper. Example —
`lambda`:

```fsharp
and expandLambda (args: Stx list) (loc: StxLoc)
                 (env: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
    // (lambda formals body...)
    match args with
    | formals :: body when not body.IsEmpty ->
        let paramNames = parseFormals formals ctx loc
        let childCtx   = ExpandCtx.childForLambda ctx
        // Mint a StorageRef.Arg for each formal; build extended env.
        let envWithFormals =
            paramNames
            |> List.mapi (fun i name -> name, Variable(StorageRef.Arg i))
            |> List.fold (fun e (k, v) -> Map.add k v e) env
        let bodyExprs, _ = expandSeq body envWithFormals childCtx
        let bd = ExpandCtx.intoBody childCtx bodyExprs
        BoundExpr.Lambda(toFormals paramNames, bd)
    | _ ->
        ctx.Error loc "ill-formed lambda"; BoundExpr.Error
```

Each special form handler follows the same pattern: parse sub-forms, extend
the environment as appropriate, call `expand` or `expandSeq`, assemble a
`BoundExpr`.

### `ExpandCtx` — Mutable per-Compilation State

The expander needs a small amount of mutable state that cannot be threaded
purely through `SyntaxEnv`:

```fsharp
type ExpandCtx =
    { mutable LocalCount : int
      mutable Captures   : StorageRef list
      Diagnostics        : DiagnosticBag
      Registry           : SourceRegistry
      Parent             : ExpandCtx option }

module ExpandCtx =
    let childForLambda (parent: ExpandCtx) =
        { LocalCount = 0
          Captures   = []
          Diagnostics = parent.Diagnostics   // shared bag
          Registry   = parent.Registry
          Parent     = Some parent }

    let intoBody (ctx: ExpandCtx) (exprs: BoundExpr list) : BoundBody =
        { Body        = BoundExpr.Seq exprs
          Locals      = ctx.LocalCount
          Captures    = ctx.Captures
          EnvMappings = None }              // populated by Lower.fs
```

`ExpandCtx` is structurally similar to the existing `BinderCtx` but stripped
of scope-management concerns (which are now owned by `SyntaxEnv`).

### Macro Expansion

```fsharp
and expandMacro (macro: MacroTransformer) (form: Stx)
                (callEnv: SyntaxEnv) (ctx: ExpandCtx) : BoundExpr =
    // 1. Match against each rule.
    let template, patBindings =
        macro.Patterns
        |> List.tryPick (fun (pat, tmpl) ->
            matchPattern pat form callEnv |> Option.map (fun b -> tmpl, b))
        |> Option.defaultWith (fun () ->
            icef "no matching macro rule for form %A" form)

    // 2. Transcribe template into Stx.
    //    Template literals  → Stx.Closure(_, macro.DefEnv, _)
    //    Pattern variables  → Stx.Closure(capturedStx, capturedEnv, _)
    let transcribed = transcribe template patBindings macro.DefEnv

    // 3. Expand the transcribed Stx in the call-site env.
    //    Stx.Closure nodes override it selectively.
    expand transcribed callEnv ctx
```

#### Pattern Matching

```fsharp
/// A captured pattern variable binding:
/// the matched Stx node paired with the environment it was captured in.
type PatternCapture =
    | Single   of stx: Stx * env: SyntaxEnv
    | Repeated of (stx: Stx * env: SyntaxEnv) list

type PatternBindings = Map<string, PatternCapture>

let rec matchPattern
    (pattern : MacroPattern)
    (stx     : Stx)
    (env     : SyntaxEnv)
    : PatternBindings option = ...
```

Each captured sub-expression records the call-site `SyntaxEnv` at the moment
of capture, so that when it is later placed into a template the correct
environment travels with it as a `Stx.Closure`.

#### Transcription

Transcription walks a `MacroTemplate` and builds a new `Stx` tree. No
Firethorn `Expression` nodes are allocated. The invariant is:

- A pattern variable use (`Subst v`) ⟹
  `Stx.Closure(capturedStx, capturedEnv, loc)` — closed over the call site.
- A template literal (identifier or constant in the template body) ⟹
  `Stx.Closure(Stx.Id(name, loc), macro.DefEnv, loc)` — closed over the
  definition site.
- Structural forms (list, dotted) are reconstructed recursively.

```fsharp
let rec transcribe
    (template : MacroTemplate)
    (bindings : PatternBindings)
    (defEnv   : SyntaxEnv)
    (loc      : StxLoc)
    : Stx =

    match template with
    | MacroTemplate.Subst name ->
        match Map.tryFind name bindings with
        | Some(Single(stx, capturedEnv)) ->
            Stx.Closure(stx, capturedEnv, loc)
        | _ ->
            Stx.Const(BoundLiteral.Null, loc)   // error recovery

    | MacroTemplate.Literal name ->
        // Identifier in the template body — close over definition site.
        Stx.Closure(Stx.Id(name, loc), defEnv, loc)

    | MacroTemplate.Const lit ->
        Stx.Const(lit, loc)

    | MacroTemplate.Form(_, elements) ->
        let children =
            elements |> List.map (transcribeElem bindings defEnv loc)
        Stx.List(children, loc)

    | MacroTemplate.DottedForm(_, elems, tail) ->
        let children =
            elems |> List.map (transcribeElem bindings defEnv loc)
        Stx.Dotted(children, transcribe tail bindings defEnv loc, loc)

and transcribeElem bindings defEnv loc = function
    | MacroTemplateElement.Template t ->
        transcribe t bindings defEnv loc
    | MacroTemplateElement.Repeated t ->
        transcribeRepeated t bindings defEnv loc

and transcribeRepeated template bindings defEnv loc =
    // Find the first Sequence binding to determine repetition count,
    // then emit one Stx subtree per repetition.
    // (Details: see open question 4.)
    Stx.List([], loc)   // placeholder
```

### Hygiene Summary

| Source of syntax | Expanding environment |
|---|---|
| User-written source | Ambient `env` passed by the recursive caller |
| Pattern variable substitution | `capturedEnv` stored in `Stx.Closure` at pattern-match time |
| Template literal (identifier in template body) | `macro.DefEnv` stored in `Stx.Closure` during transcription |
| Macro-introduced binding (e.g. `let` written inside a template) | Fresh `StorageRef` minted when the transcribed binding form is expanded in the definition-site env |

Because `Stx.Closure` nodes carry the environment explicitly, the recursive
`expand` function never needs to distinguish "user syntax" from
"macro-generated syntax" — it simply peels closures and dispatches uniformly.

## Affected Files

| File | Nature of change |
|---|---|
| `src/Feersum.CompilerServices/Binding/Expand.fs` | All new types and logic: `Stx`, `StxLoc`, `SyntaxEnv`, `SyntaxBinding`, `MacroTransformer`, `PatternBindings`, `ExpandCtx`, `expand`, `expandSeq`, `transcribe`, `matchPattern`. No other file is modified during isolation. |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Integration point (future). `bind` calls `Expand.expandProgram` instead of `bindInContext`. |
| `src/Feersum.CompilerServices/Binding/Macros.fs` | Integration point (future). `Macro` type gains `DefEnv : SyntaxEnv`. Existing pattern/template parsing is reused; `macroExpand`/`macroApply` are replaced by `transcribe`/`matchPattern` from `Expand.fs`. |

## Open Questions

1. **`ExpandCtx` vs reusing `BinderCtx`.** The proposed `ExpandCtx` is a
   slimmed-down `BinderCtx` without scope management. An alternative is to
   reuse `BinderCtx` directly and stop using its `Scope` field for
   name-resolution decisions. Decide before implementing lambda expansion.

   **DECISION**: Create a new type. This new expander should be a completey new
   implementation in parallel so we can compare it at the end.

2. **`define` sequencing model.** `expandSeq` as sketched inspects each form
   to check whether it is a `define` before recursing. An alternative is a
   two-pass approach: first pre-scan the body for `define`/`define-syntax`
   to pre-populate the env with forward-declared `StorageRef`s, then expand
   all forms in one pass. The two-pass model is simpler for `letrec*`-style
   mutual recursion but requires knowing the full body upfront. Choose one
   model before implementing `define` in a body context.

   **DECISION**: Two pass define seems simpler in the long run.

3. **`Stx.Closure` chains.** During ellipsis transcription a pattern variable
   substitution may itself be a `Stx.Closure` (if the matched datum was
   already inside a closure). The expander's "match `Stx.Closure` first and
   recurse" rule handles this correctly. The invariant to maintain: `transcribe`
   never nests a freshly-minted `Stx.Closure` directly inside another
   freshly-minted `Stx.Closure`; chains arise only because a captured `Stx`
   node may already be a closure from an outer expansion round.

   **DECISION**: When wrapping in a closure for a substitution if the inner
   item to be wrapped is a closure we just wrap the inner one instead. 

   We could use the `resolve` or similar here. 

4. **Ellipsis unrolling.** `transcribeRepeated` must iterate over `Repeated`
   capture bindings and produce one `Stx` subtree per repetition. The
   repetition count comes from the `Repeated` capture of the first
   ellipsis-bound variable in the template; if multiple ellipsis-bound
   variables appear they must all have the same count. Specify the loop
   structure and mismatch error handling before implementing `syntax-rules`
   with `...`.

   **DECISION**: See the current expander in `Macros.fs` here. The repated part
   of macro bindings will contain a set of nested bindings at each level. If a
   binding doesn't exist because one match was shorter than the other then the
   expansion should fail because no caputre for that pattern variable exists at
   that level.

5. **Integration gate.** Define the list of special forms that must be handled
   before the new expander can replace `bindInContext`:
   `if`, `begin`, `lambda`, `define`, `set!`, `quote`, `let`, `let*`,
   `letrec`, `letrec*`, `define-syntax`, `let-syntax`, `letrec-syntax`,
   `import`, `define-library`.

    **DECISION**: Implement just `if`, `begin`, `lambda`, `define`, `set!`,
    `quote`, `define-syntax` and `let-syntax` for now. Libraries and letrecs
    can be added later. 
