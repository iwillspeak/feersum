# Expansion Work Stack

This document describes a work-stack-driven macro expansion pass for Feersum,
introducing hygienic `syntax-rules` expansion by threading environments through
an explicit work stack rather than embedding hygiene information in the syntax
tree. The design is adapted from the approach outlined in the [Hygienic Macro
Expansion](./scheme-hygenic-workstack/) reference document, mapped onto
Feersum's existing compiler pipeline and data structures.

## Background

### Current Pipeline

Feersum follows a four-pass compilation pipeline:

```
Source text
    ↓  Parse.fs
Red/green tree (Firethorn)  →  typed CST (Expression, Form, Symbol, …)
    ↓  Binder.fs
BoundExpr tree              →  resolved storage references
    ↓  Lower.fs
Lowered BoundExpr           →  capture-converted, compacted
    ↓  Emit.fs / Compiler.fs
CIL bytecode (Mono.Cecil)
```

The red/green tree comes from the Firethorn library and is lossless. The typed
CST layer in `Syntax/Tree.fs` wraps red nodes to provide structured access
(`Form`, `Symbol`, `Constant`, `Quoted`, etc.). The `Expression` base class
carries a `DocId` for provenance but no hygiene annotations.

### Current Macro Handling

Macro expansion happens inside the binder. When `bindInContext` encounters a
form whose head resolves to `StorageRef.Macro m`, it calls
`Macros.macroApply`, which:

1. Matches the call-site `Expression` against the macro's `MacroPattern` list.
2. On match, calls `macroExpand` to produce a new `Expression` tree from the
   `MacroTemplate` and `MacroBindings`.
3. The resulting synthetic `Expression` (created via `Factories.fs` with
   `DocId.Synthetic`) is recursively passed back to `bindInContext`.

This approach has several limitations:

- **No hygiene.** Macro-introduced identifiers resolve in the caller's scope.
  A template that references a helper binding from the macro's definition site
  will instead capture whatever the caller has in scope under that name.
- **Recursive expansion on the call stack.** Deep or nested macro expansion
  risks stack overflow. Each expansion round-trips through the full binder.
- **Intermediate `Expression` trees.** Expansion builds new red/green syntax
  nodes (`Factories.form`, `Factories.symbol`, etc.) only to immediately tear
  them apart in the binder. These transient trees are structurally valid but
  carry no provenance and exist solely as a communication format between
  `Macros.fs` and `Binder.fs`.
- **No provenance chain.** Synthetic nodes get `DocId.Synthetic`, so
  diagnostics from inside an expansion have no path back to the call site or
  the template site.

### Goal

Replace the recursive macro-expand-then-rebind loop with a single fused
expansion + elaboration pass driven by an explicit work stack. Environments
travel on the stack, hygiene is achieved through stamped identifiers, and the
`BoundExpr` output tree is produced directly without intermediate syntax
reconstruction.

## Design

### Overview

```
Red/green tree (Firethorn, unchanged)
    ↓  typed CST (Expression)
         ↓ expandAll (new fused pass)
BoundExpr tree (existing type, mostly unchanged)
    ↓  Lower.fs (unchanged)
    ↓  Emit.fs  (unchanged)
```

The key architectural decision is that **hygiene is a property of the
traversal, not the tree**. No hygiene annotations appear on `Expression`
nodes or in the green tree. Environment information lives exclusively on
the work stack during expansion and is baked into resolved storage
references in the `BoundExpr` tree once expansion is complete.

### Identifiers and Stamps

A `SyntaxIdentifier` is a fully resolved, self-contained binding reference.
Two identifiers are the same binding if and only if their stamps and scopes
both match.

```fsharp
/// A hygienic identifier. Self-contained: no side table needed.
[<Struct>]
type SyntaxIdentifier =
    { Name  : string
      Stamp : int       // fresh per binding introduction; 0 = user-written
      Scope : ScopeId }

and ScopeId = ScopeId of int
```

Stamps are minted at binding introduction sites (lambda parameters, let
bindings, `define`) by the expander. User-written identifiers receive stamp
`0` during initial expansion. Macro-introduced identifiers receive fresh
stamps during transcription. Two identifiers with the same name but different
stamps are different bindings — this is the entire hygiene mechanism.

The existing `StorageRef` type continues to describe *where* a value lives
(`Local`, `Arg`, `Global`, `Environment`, `Captured`). The new
`SyntaxIdentifier` describes *which* binding is referenced, and the syntax
environment maps identifiers to storage.

### Syntax Bindings and the Syntax Environment

```fsharp
/// What a name is bound to in the syntax-time environment.
type SyntaxBinding =
    | SpecialForm of string        // "if", "lambda", "define", etc.
    | MacroDef    of Macro         // existing Macro type from Macros.fs
    | Variable    of StorageRef    // resolved storage

/// The syntax environment: a persistent linked chain of frames.
/// Lookup is by name only; stamps are used to *create* the mapping,
/// not to query it.
type SyntaxEnv =
    { Bindings : Map<string, SyntaxBinding>
      Parent   : SyntaxEnv option }
```

The environment is an immutable linked chain. Snapshotting for capture into a
macro transformer is free — F# records are immutable and parent chains are
shared. This replaces the mutable `Scope<StorageRef>` used by the current
`BinderCtx` for syntax-time decisions, though `BinderCtx` itself remains for
managing local counts, captures, and the diagnostic bag.

```fsharp
module SyntaxEnv =

    let empty = { Bindings = Map.empty; Parent = None }

    let extend (bindings: (string * SyntaxBinding) list) (env: SyntaxEnv) =
        { Bindings =
            bindings |> List.fold (fun m (k, v) -> Map.add k v m) env.Bindings
          Parent = Some env }

    let rec lookup (name: string) (env: SyntaxEnv) =
        match Map.tryFind name env.Bindings with
        | Some b -> Some b
        | None -> env.Parent |> Option.bind (lookup name)
```

### Macro Transformers

The existing `Macro` type gains a definition-site environment snapshot:

```fsharp
type Macro =
    { Name: string
      Transformers: MacroTransformer list
      DefEnv: SyntaxEnv              // environment at definition site
      DefProvenance: DocId }         // where this macro was defined
```

When `define-syntax` or `let-syntax` processes a `syntax-rules` form, the
current ambient `SyntaxEnv` is captured in `DefEnv`. This snapshot is what
makes template-literal identifiers resolve at the definition site rather
than the call site.

### Captured Datums

A `CapturedDatum` pairs a syntax node with the environment in which it was
captured during pattern matching. It exists only during expansion — never in
any tree.

```fsharp
/// A captured pattern variable substitution.
type CapturedDatum =
    { Datum : Expression
      Env   : SyntaxEnv
      DocId : DocId }

type BindingValue =
    | Single   of CapturedDatum
    | Sequence of CapturedDatum list   // for ellipsis bindings

type PatternBindings = Map<string, BindingValue>
```

### Work Items

```fsharp
/// Instructions on the work stack.
type WorkItem =
    /// Expand this expression in this environment and push a Result.
    | Expand    of Expression * SyntaxEnv
    /// Begin collecting child results for a compound expression.
    | BeginList of DocId
    /// Finish collecting; assemble the accumulated results.
    | EndList   of DocId
    /// A fully elaborated expression; pass upward to the nearest collector.
    | Result    of BoundExpr
```

`Expand` is the environment-switching primitive. When transcription produces
an `Expand(datum, callSiteEnv)` for a pattern variable and an
`Expand(node, defSiteEnv)` for a template literal, the expander processes
each in the appropriate environment without any tree annotation.

### The Main Loop

The loop uses a mutable `result` cell as the exit signal. A `Result` item with
no active list collector is the terminal condition — it sets the cell and
exits the loop. This avoids two related bugs in a naïve implementation:

- **Infinite loop:** re-pushing `Result` when the list stack is empty keeps
  `work.Count > 0` forever, so `while work.Count > 0` never exits.
- **Unreachable drain:** if the loop exits only when `work.Count = 0`, any
  post-loop `TryPop` always returns `false` and the result is lost.

Results accumulate into `listStk` frames in prepend order, so `List.rev` in
`EndList` is necessary. Items pushed by `expandMacro`, `expandSpecialForm`, and
`expandApplication` must be pushed in **reverse** execution order since the
stack is LIFO — this invariant applies to all callers.

```fsharp
let expandAll
    (root    : Expression)
    (ambient : SyntaxEnv)
    (ctx     : BinderCtx)
    : BoundExpr =

    let work    = Stack<WorkItem>()
    let listStk = Stack<BoundExpr list * DocId>()
    let mutable result : BoundExpr option = None

    work.Push(Expand(root, ambient))

    while result.IsNone && work.Count > 0 do
        match work.Pop() with

        | Result expr ->
            if listStk.Count > 0 then
                // Accumulate into the innermost open list frame.
                let (exprs, docId) = listStk.Pop()
                listStk.Push(expr :: exprs, docId)
            else
                // Nothing left to collect into — this is the final result.
                result <- Some expr

        | BeginList docId ->
            listStk.Push([], docId)

        | EndList _ ->
            // EndList uses the docId stored in the frame by BeginList;
            // the docId carried by EndList itself is not needed.
            let (exprs, docId) = listStk.Pop()
            let assembled = assembleForm (List.rev exprs) docId ctx
            work.Push(Result assembled)

        | Expand(datum, env) ->
            match datum with
            | SymbolNode s ->
                work.Push(Result(elaborateRef s env ctx))
            | ConstantNode c ->
                work.Push(Result(BoundLiteral.FromConstantValue c.Value
                                 |> BoundExpr.Literal))
            | FormNode f ->
                let body = f.Body |> List.ofSeq
                match body with
                | [] ->
                    work.Push(Result(BoundExpr.Literal BoundLiteral.Null))
                | headExpr :: _ ->
                    match headExpr with
                    | SymbolNode sym ->
                        match SyntaxEnv.lookup sym.CookedValue env with
                        | Some(MacroDef macro) ->
                            expandMacro macro f env work ctx
                        | Some(SpecialForm _) ->
                            expandSpecialForm sym.CookedValue f env work ctx
                        | _ ->
                            expandApplication body env work ctx
                    | _ ->
                        expandApplication body env work ctx
            | QuotedNode q ->
                // Quoted forms bypass expansion
                match q.Inner with
                | Some inner -> work.Push(Result(bindQuoted ctx inner))
                | None -> work.Push(Result BoundExpr.Error)
            | _ ->
                work.Push(Result(bindDatum ctx datum))

    match result with
    | Some expr -> expr
    | None -> failwith "expansion produced no result"
```

### Pattern Matching

The matcher returns `PatternBindings`. Each capture records the call-site
`SyntaxEnv` at the moment of capture, so the environment travels with the
datum.

```fsharp
let rec matchPattern
    (pattern  : MacroPattern)
    (datum    : Expression)
    (env      : SyntaxEnv)
    (bindings : PatternBindings)
    : PatternBindings option =

    match pattern, datum with
    | MacroPattern.Underscore, _ ->
        Some bindings
    | MacroPattern.Variable v, d ->
        let captured =
            { Datum = d; Env = env; DocId = d.DocId }
        Some(Map.add v (Single captured) bindings)
    | MacroPattern.Literal lit, SymbolNode s ->
        if lit = s.CookedValue then Some bindings else None
    | MacroPattern.Constant c, ConstantNode cn ->
        // delegate to existing constantsMatch
        match cn.Value with
        | Some k when constantsMatch c k -> Some bindings
        | _ -> None
    | MacroPattern.Form pats, FormNode f ->
        matchList pats (f.Body |> List.ofSeq) env bindings
    | MacroPattern.Repeat inner, FormNode f ->
        matchEllipsis inner (f.Body |> List.ofSeq) env bindings
    | _ -> None
```

This parallels the existing `Macros.macroMatch` but threads a `SyntaxEnv`
and produces `CapturedDatum` values instead of bare `Expression` nodes.

### Transcription

Transcription interprets a template directly into `WorkItem` values. No
intermediate `Expression` tree is constructed. Environment switching is
encoded entirely in `Expand` items carrying the appropriate `SyntaxEnv`.

```fsharp
let rec transcribe
    (template : MacroTemplate)
    (bindings : PatternBindings)
    (defEnv   : SyntaxEnv)
    (callEnv  : SyntaxEnv)
    : WorkItem list =

    match template with
    | MacroTemplate.Subst sym ->
        match Map.tryFind sym bindings with
        | Some(Single captured) ->
            // Push captured datum in call-site env
            [ Expand(captured.Datum, captured.Env) ]
        | _ ->
            failwith $"unbound template variable {sym}"

    | MacroTemplate.Quoted node ->
        // Template literal — resolve in def-site env
        [ Expand(node, defEnv) ]

    | MacroTemplate.Form(_, elements) ->
        let childItems =
            elements |> List.collect (transcribeElement bindings defEnv callEnv)
        [ BeginList DocId.Synthetic ]
        @ childItems
        @ [ EndList DocId.Synthetic ]

    | MacroTemplate.DottedForm _ ->
        // TODO: implement dotted form transcription
        failwith "Dotted forms in macro transcription not yet supported"

and transcribeElement bindings defEnv callEnv =
    function
    | MacroTemplateElement.Template t ->
        transcribe t bindings defEnv callEnv
    | MacroTemplateElement.Repeated t ->
        transcribeRepeated t bindings defEnv callEnv

and transcribeRepeated template bindings defEnv callEnv =
    // Unroll: for each repetition, substitute the i-th element of each
    // Sequence binding and transcribe one copy of the template.
    // (Implementation detail — same structure as reference doc §9.)
    []
```

The critical property: pattern-variable substitutions push
`Expand(datum, capturedEnv)` and template literals push
`Expand(node, defEnv)`. The expander handles environment switching
naturally without any tree-embedded closure nodes.

### Macro Expansion

```fsharp
let expandMacro
    (macro   : Macro)
    (form    : Form)
    (callEnv : SyntaxEnv)
    (work    : Stack<WorkItem>)
    (ctx     : BinderCtx)
    : unit =

    let syntax = form :> Expression

    // Try each transformer until one matches
    let rule, bindings =
        macro.Transformers
        |> List.tryPick (fun (pat, tmpl) ->
            matchPattern pat syntax callEnv Map.empty
            |> Option.map (fun b -> (pat, tmpl), b))
        |> Option.defaultWith (fun () ->
            failwith $"no matching rule for {macro.Name}")

    let _, template = rule

    // Transcribe directly to work items
    let items = transcribe template bindings macro.DefEnv callEnv

    // Push in reverse so they execute in order
    items |> List.rev |> List.iter work.Push
```

### Special Form Elaboration

Special forms are elaborated directly into `BoundExpr` nodes, just as in
the current binder. The difference is that binding introductions mint
fresh `SyntaxIdentifier` stamps and extend the ambient `SyntaxEnv` before
pushing child `Expand` items.

```fsharp
let expandSpecialForm
    (name    : string)
    (form    : Form)
    (ambient : SyntaxEnv)
    (work    : Stack<WorkItem>)
    (ctx     : BinderCtx)
    : unit =

    let children = form.Body |> List.ofSeq

    match name with
    | "lambda" ->
        // (lambda (p1 p2 …) body…)
        match children with
        | _ :: formals :: body ->
            // Mint fresh identifiers for each parameter, extend env,
            // push body as Expand items in the extended env.
            // Push a collector to assemble the Lambda BoundExpr.
            ...
        | _ -> ()

    | "define" ->
        match children with
        | [ _; SymbolNode nameNode; value ] ->
            let id = nameNode.CookedValue
            let storage = BinderCtx.addBinding ctx id
            let extended =
                SyntaxEnv.extend [ id, Variable storage ] ambient
            work.Push(Expand(value, extended))
            // ... assemble Store
        | _ -> ()

    | "define-syntax" ->
        match children with
        | [ _; SymbolNode nameNode; syntaxRules ] ->
            let id = nameNode.CookedValue
            match Macros.parseSyntaxRules id syntaxRules with
            | Ok macro ->
                // Snapshot the current ambient env as DefEnv
                let macroWithEnv =
                    { macro with DefEnv = ambient }
                let extended =
                    SyntaxEnv.extend [ id, MacroDef macroWithEnv ] ambient
                // Continue expansion in the extended env
                ...
            | Error e ->
                ctx.Diagnostics.Add e
        | _ -> ()

    | "if" | "begin" | "let" | "let*" | "letrec" | "letrec*"
    | "set!" | "quote" | "let-syntax" | "import" | "define-library" ->
        // Each handled analogously to the current bindForm cases,
        // but pushing Expand work items instead of recursive calls.
        ...

    | _ -> ()
```

### Name Resolution

```fsharp
let elaborateRef
    (sym : Symbol)
    (env : SyntaxEnv)
    (ctx : BinderCtx)
    : BoundExpr =
    let id = sym.CookedValue
    match SyntaxEnv.lookup id env with
    | Some(Variable storage) ->
        BoundExpr.Load storage
    | Some(SpecialForm _) ->
        // Special form name in value position — error or identity
        BoundExpr.Error
    | Some(MacroDef _) ->
        // Macro name in value position — error
        BoundExpr.Error
    | None ->
        // Unbound — diagnostic
        ctx.Diagnostics.Emit
            BinderDiagnostics.undefinedSymbol
            (getNodeLocation ctx (sym :> Expression))
            (sprintf "The symbol %s is not defined in the current context" id)
        BoundExpr.Error
```

### Integration with Existing Code

The `expandAll` function replaces the current `bindInContext` as the main
entry point for the bind phase. The `Binder.bind` function changes to
construct an initial `SyntaxEnv` from the library scope and call `expandAll`:

```fsharp
let bind scope libraries registry units =
    let ctx = BinderCtx.createForGlobalScope scope libraries ["LispProgram"] registry

    // Build the initial SyntaxEnv from the scope
    let ambient =
        scope
        |> Map.toList
        |> List.map (fun (name, storage) -> name, Variable storage)
        |> fun bindings -> SyntaxEnv.extend bindings SyntaxEnv.empty

    let bound =
        units
        |> List.collect (fun exprs ->
            exprs |> List.map (fun expr -> expandAll expr ambient ctx))
        |> BoundExpr.Seq
        |> BinderCtx.intoRoot ctx

    { Root = bound
      MangledName = ctx.MangledName
      Diagnostics = ctx.Diagnostics.Take }
```

### What Lives Where

| Component | Contents | Hygiene role |
|-----------|----------|-------------|
| Green tree (Firethorn) | Pure syntax content. Immutable. Structurally shared. | None |
| Red tree (Firethorn) | Position + parent pointers. | None |
| Typed CST (`Expression`) | `Form`, `Symbol`, `Constant`, etc. Carries `DocId`. | None — no stamps, no env |
| `SyntaxEnv` | Threaded as `ambient` through work items. Snapshot in `Macro.DefEnv`. | Lives only on work stack |
| `CapturedDatum` | `(Expression, SyntaxEnv, DocId)` triple inside `PatternBindings`. | Lives only inside `expandMacro` |
| `WorkItem` | The sole carrier of environments during expansion. | Consumed within `expandAll` |
| `BinderCtx` | Local counts, captures, diagnostics, registry. | Unchanged from current design |
| `BoundExpr` | Fully resolved. `StorageRef` references. No `SyntaxEnv`. | Self-contained output |

### The Hygiene Guarantee

- **Macro-introduced identifiers** (template literals) are expanded inside
  `Expand(node, defEnv)` items. They resolve in the definition environment
  and cannot capture caller bindings.

- **Caller-supplied identifiers** (pattern variable substitutions) are
  expanded inside `Expand(datum, captured.Env)` items, where `captured.Env`
  is the call-site environment recorded at pattern-match time. They resolve
  in the caller's scope regardless of what the template does around them.

- **Same-binding test** for downstream passes can compare `StorageRef`
  values directly, which already have identity semantics.

### What Changes Compared to the Current Design

| Current | Proposed |
|---------|----------|
| `Macros.macroExpand` builds new `Expression` trees | Transcription produces `WorkItem` lists directly |
| `Macros.macroApply` returns `Result<Expression, _>` | `expandMacro` pushes items onto the work stack |
| `bindInContext` is recursive | `expandAll` is iterative (explicit work stack) |
| No hygiene — all names resolve in caller scope | Stamped identifiers + env threading give R5RS hygiene |
| `DocId.Synthetic` loses provenance | Can be extended with provenance chain per [Document Service](./document-service/) |
| `Scope<StorageRef>` is mutable, linear | `SyntaxEnv` is immutable, shared, snapshot-safe |
| `StorageRef.Macro` embeds macro in scope | `SyntaxBinding.MacroDef` with `DefEnv` snapshot |

## Affected Files

| File | Change |
|------|--------|
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Replace recursive `bindInContext` with iterative `expandAll`. Extract special-form handlers into `expandSpecialForm`. Build initial `SyntaxEnv` from scope in `Binder.bind`. |
| `src/Feersum.CompilerServices/Binding/Macros.fs` | Add `DefEnv` and `DefProvenance` to `Macro`. Replace `macroExpand`/`macroApply` with `matchPattern` (env-aware) and `transcribe` (produces `WorkItem` lists). Keep pattern/template parsing unchanged. |
| `src/Feersum.CompilerServices/Binding/Lower.fs` | No changes expected. The `BoundExpr` shape remains the same. |
| `src/Feersum.CompilerServices/Syntax/Tree.fs` | No changes. The typed CST layer remains hygiene-free. |
| `src/Feersum.CompilerServices/Syntax/Factories.fs` | Reduce usage — transcription no longer builds intermediate `Expression` trees. Factory helpers may still be needed for error recovery. |
| `src/Feersum.CompilerServices/Scope.fs` | May be retained for `BinderCtx` internal use, but no longer the primary name-resolution structure during expansion. |
| `src/Feersum.CompilerServices/Text.fs` | No changes. `DocId` and `SourceRegistry` remain as-is. |
| `src/Feersum.CompilerServices/Compile/Compiler.fs` | No changes. Continues to call `Binder.bind` → `Lower.lower` → `Emit.emit`. |

## Open Questions

1. **Gradual migration or big-bang?** The fused pass is a large change to the
   binder. One option is to introduce `expandAll` alongside `bindInContext` and
   migrate form-by-form behind a feature flag. The preferred direction is
   gradual migration, keeping both paths active until all special forms are
   ported.

2. **`BinderCtx` lifetime.** The current `BinderCtx` owns mutable state for
   local counts, captures, and the diagnostic bag. The work-stack approach
   still needs this state but never recurses into child `BinderCtx` instances
   during expansion — child contexts are only needed for lambda bodies. Clarify
   whether `BinderCtx` nesting should be modelled on the work stack or remain
   as explicit child-context creation.

3. **Provenance threading.** The [Document Service](./document-service/) design
   proposes an `ExpansionProvenance` table. The work-stack expansion is the
   natural place to populate it, but the current design does not yet depend on
   it. Decide whether to integrate provenance tracking in the initial
   implementation or defer it to a follow-up.

4. **Ellipsis transcription.** The current `Macros.macroExpandElement` handles
   `Repeated` templates by iterating over `MacroBindings.Repeated`. The
   work-stack transcription needs an analogous unrolling strategy for
   `Sequence` bindings. The reference design sketches this but the details of
   nesting depth tracking need to be worked out for Feersum's
   `MacroBindings` shape.

5. **Dotted-form macros.** The current implementation notes dotted forms in
   macro expansion as unimplemented. The work-stack transcription should
   handle `MacroTemplate.DottedForm` — decide whether to include this in the
   initial work or keep it as a separate follow-up.
