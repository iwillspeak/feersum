# Binder Context Design

This document describes the restructured binder context — the state threaded
through each `bindXxx` / `expandXxx` call — as the old `Binder.fs` is
replaced by the new `Expand.fs` implementation.

## Background

### Current State

There are two implementations:

1. **`Binder.fs`** — the legacy binder. It is a stub (`unimpl`) that delegates
   to `Expand.fs` for the real work. Its `BinderCtx` record is essentially
   dead code but illustrates the original design intent: a record with mutable
   `LocalCount`, `Captures`, a `Scope: Map<Ident, StorageRef>`, and a `Parent`
   link for lexical nesting.

2. **`Expand.fs`** — the live syntactic-closures expander. It fuses macro
   expansion and binding resolution into a single recursive walk over `Stx`.
   Its `ExpandCtx` record carries:
   - **Per-lambda mutable counters**: `LocalCount`, `Captures`, `HasDynEnv`.
   - **Cross-lambda metadata**: `LambdaDepth`, `IsGlobal`, `ScopeDepth`, `MangledName`.
   - **Compilation-unit singletons** (shared by reference): `Diagnostics`,
     `MacroRegistry`, `Libraries`.
   - **A mutable `BindingMap: Map<Ident, BindingMeaning>`** — the mapping from
     hygiene `Ident` to `StorageRef` + lambda depth.
   - **A `Parent` link** to enable cross-lambda capture threading.

   In addition, a separate `StxEnvironment = Map<string, StxBinding>` (the
   "scope" or "region") is threaded as an explicit parameter through every
   `expand` call. `StxEnvironment` maps source-level names to `StxBinding`,
   which is either a `Special`, a `Macro` (keyed by `Ident` into
   `MacroRegistry`), or a `Variable` (keyed by `Ident` into
   `BindingMap`).

### Problems with the Current Shape

| Problem | Detail |
|---------|--------|
| Mixed mutability | `ExpandCtx` is a record with half its fields mutable. Some mutations are per-lambda (local count), some are per-unit (libraries, macro registry). Child contexts copy-on-create some fields but share others by reference. |
| Flat mutable `BindingMap` | The `Ident -> StorageRef` map is mutated in place and grows monotonically within a frame. Entries introduced inside a `let` body persist in the map after the `let` returns. This is harmless thanks to `Ident` uniqueness but semantically messy. |
| `lambdaDepth` bookkeeping | Every binding stores the lambda depth at which it was introduced. This is only used to decide whether a cross-lambda capture is needed. Tracking it per-entry adds noise. |
| Registries are special-cased | `MacroRegistry` is a class with internal mutable state; `Libraries` is a mutable list on the context. Global variables and method-level state have no equivalent registry. |
| Output threading | Most `expandXxx` functions return `BoundExpr`, but definition-position forms return `BoundExpr * StxEnvironment`. There is no unified result type, so callers must decide on a case-by-case basis which return convention to use. |
| `ScopeDepth` tracks local nesting | A mutable counter distinguishes "true globals" from `define` inside a `let` body. This is fragile and could be replaced by the context-creation pattern. |

### Two Kinds of Scope

The binder manages two distinct scoping concerns that are always extended
together but have different lifetimes and snapshotting behaviour:

| Concern | What it maps | Snapshotted by closures? | Extended by |
|---------|-------------|--------------------------|------------|
| **Region** (syntax env) | `name -> StxBinding` | Yes — macro closures capture it | `define`, `let`, `import`, `define-syntax` |
| **Resolution** (binding env) | `Ident -> StorageRef` | No — `Ident`s are globally unique | Same forms, in lockstep |

A syntactic closure captures the region (so that macro-introduced names
resolve at the definition site), but the resolution map is not captured.
A closure may switch to a completely unrelated region snapshot while the
resolution environment continues growing in the current frame. On exit from
a closure, new bindings introduced inside it are diff-merged back into the
caller's region, while the resolution entries are simply still there because
they're keyed by unique `Ident`s.

This asymmetry is the reason the two maps must remain separate types.

## Design

### Overview

Split the current `ExpandCtx` into three layers:

1. **`CompilationUnit`** — shared singletons (diagnostics, macro registry,
   libraries, source registry). One per compilation.
2. **`FrameCtx`** — per-frame state (local counter, resolution scope stack).
   One per lambda or global/library body. Distinguished by a `FrameKind` DU.
3. **`StxEnvironment`** — the region. Threaded as an explicit parameter, as
   today.

The key change is in how binding resolution works: instead of a flat mutable
`Map<Ident, BindingMeaning>` we use a **stack of `Map<Ident, StorageRef>`**
on the frame. Entering a `let` body pushes a new map; exiting pops it. This
eliminates both the monotonically-growing flat map and the `lambdaDepth`
bookkeeping. Cross-lambda captures are detected by exhausting the current
frame's stack and crossing into the parent.

### CompilationUnit

A record, created once per compilation unit. Owns the truly shared singletons
that every frame needs a reference to:

```fsharp
type CompilationUnit =
    { Diagnostics: DiagnosticBag
      MacroRegistry: MacroRegistry
      mutable Libraries: LibrarySignature<StorageRef> list
      SourceRegistry: SourceRegistry }
```

### FrameKind and LambdaState

A DU that captures the state specific to each kind of frame. Global/library
frames need a mangled name for emitting globals and never participate in
capture threading. Lambda frames need capture tracking and a parent link.

```fsharp
type LambdaState =
    { Parent: FrameCtx
      mutable Captures: StorageRef list
      mutable HasDynEnv: bool }

/// State specific to the kind of binding frame.
type FrameKind =
    /// A global or library-body frame. `define` at the top level produces
    /// `StorageRef.Global(mangledName, Field name)`.
    | Global of mangledName: string
    /// A lambda-body frame. Tracks captures and links to the enclosing frame
    /// for cross-lambda capture threading.
    | Lambda of LambdaState
```

Captures, `HasDynEnv`, and the parent link live exclusively on the `Lambda`
case — a global frame never has captures and never needs a parent for
capture resolution. `FrameKind` is a natural extension point: a future `Repl`
or `Module` frame kind can carry its own state without polluting the others.

### Resolution Scope Stack

The binding resolution environment is a stack of immutable maps. Each map
holds the `Ident -> StorageRef` bindings introduced at one lexical level:

```fsharp
/// A single level in the resolution scope stack.
type ResolutionScope = Map<Ident, StorageRef>

/// The full resolution environment for a frame: a stack of scopes.
/// The head is the innermost (most recently pushed) scope.
type ResolutionEnv = ResolutionScope list
```

Operations:

```fsharp
module ResolutionEnv =

    /// Start with a single empty scope (the frame's top-level scope).
    let empty: ResolutionEnv = [ Map.empty ]

    /// Push a new empty scope (entering a `let` / `letrec` body).
    let push (env: ResolutionEnv) : ResolutionEnv =
        Map.empty :: env

    /// Pop the innermost scope (leaving a `let` / `letrec` body).
    /// Returns the restored environment.
    let pop (env: ResolutionEnv) : ResolutionEnv =
        match env with
        | _ :: rest when not (List.isEmpty rest) -> rest
        | _ -> ice "cannot pop the frame's root scope"

    /// Add a binding to the current (innermost) scope.
    let addBinding (id: Ident) (storage: StorageRef) (env: ResolutionEnv) : ResolutionEnv =
        match env with
        | top :: rest -> (Map.add id storage top) :: rest
        | [] -> ice "empty resolution env"

    /// Look up an Ident in this frame's scopes, from innermost outward.
    /// Returns None if the Ident is not found in this frame at all.
    let tryFind (id: Ident) (env: ResolutionEnv) : StorageRef option =
        env |> List.tryPick (Map.tryFind id)
```

### FrameCtx

The per-frame record. Shared fields that both global and lambda frames need
sit directly on the record:

```fsharp
type FrameCtx =
    { Unit: CompilationUnit
      Kind: FrameKind
      mutable ResEnv: ResolutionEnv
      mutable LocalCount: int }
```

No `LambdaDepth`. No `ScopeDepth`. No `BindingMap`. The resolution scope
stack handles both jobs — its depth tells us whether we're nested, and
exhausting the stack triggers a cross-frame capture walk.

### Variable Resolution

Resolution is a two-step process, as today: look up the name in the region
(`StxEnvironment`) to get the `Ident`, then resolve the `Ident` through the
frame.

```fsharp
module FrameCtx =

    /// Resolve an Ident to a (potentially capture-wrapped) StorageRef.
    /// Walks the current frame's scope stack; if not found, crosses into the
    /// parent frame (Lambda only) and registers a capture.
    let rec resolveId (ctx: FrameCtx) (id: Ident) : StorageRef option =
        match ResolutionEnv.tryFind id ctx.ResEnv with
        | Some storage -> Some storage
        | None ->
            // Not in this frame's scopes. Try the parent (captures).
            match ctx.Kind with
            | Lambda state ->
                match resolveId state.Parent id with
                | Some outerStorage ->
                    // Register capture on this lambda frame.
                    match outerStorage with
                    | Captured _ | Arg _ | Local _ | Environment _ ->
                        state.Captures <- outerStorage :: state.Captures
                        state.HasDynEnv <- true
                        Some(StorageRef.Captured outerStorage)
                    | _ -> Some outerStorage
                | None -> None
            | Global _ ->
                // Global frames have no parent to search.
                None
```

The caller in the expander then looks like:

```fsharp
let lookupVar (name: string) (loc: TextLocation)
              (region: StxEnvironment) (ctx: FrameCtx) : BoundExpr =
    match Map.tryFind name region with
    | Some(StxBinding.Variable id) ->
        match FrameCtx.resolveId ctx id with
        | Some storage -> BoundExpr.Load storage
        | None ->
            emitError ctx ... $"unbound identifier '{name}'"
            BoundExpr.Error
    | Some(StxBinding.Special _ | StxBinding.Macro _) ->
        emitError ctx ... $"keyword '{name}' used in value position"
        BoundExpr.Error
    | None ->
        match Environments.tryResolveSpecial name with
        | Some _ ->
            emitError ctx ... $"keyword '{name}' used in value position"
            BoundExpr.Error
        | None ->
            emitError ctx ... $"unbound identifier '{name}'"
            BoundExpr.Error
```

### Scope Push/Pop for Let Forms

Entering a `let` body pushes both a new resolution scope and (implicitly)
extends the region. Exiting restores the resolution scope. The region doesn't
need explicit restore because it travels as a value parameter.

```fsharp
    /// Push a new resolution scope on the frame.
    let pushScope (ctx: FrameCtx) : unit =
        ctx.ResEnv <- ResolutionEnv.push ctx.ResEnv

    /// Pop the innermost resolution scope from the frame.
    let popScope (ctx: FrameCtx) : unit =
        ctx.ResEnv <- ResolutionEnv.pop ctx.ResEnv
```

A `let` expansion becomes:

```fsharp
and expandLet args loc region ctx =
    match args with
    | bindingStx :: body when not (List.isEmpty body) ->
        let specs = parseBindingSpecs bindingStx ctx

        // Evaluate inits in the outer region (parallel let).
        let stores =
            specs |> List.map (fun (name, initStx) ->
                let initExpr = expand initStx region ctx
                let storage = StorageRef.Local(FrameCtx.nextLocal ctx)
                let id = Ident.fresh ()
                name, id, storage, initExpr)

        // Push a new resolution scope and extend the region for the body.
        FrameCtx.pushScope ctx
        let innerRegion =
            stores |> List.fold (fun rgn (name, id, storage, _) ->
                ctx.ResEnv <- ResolutionEnv.addBinding id storage ctx.ResEnv
                Map.add name (StxBinding.Variable id) rgn
            ) region

        let bodyExprs, _ = expandSeqWithSPs body innerRegion ctx
        FrameCtx.popScope ctx

        let storeExprs = stores |> List.map (fun (_, _, s, v) -> BoundExpr.Store(s, Some v))
        BoundExpr.Seq(storeExprs @ bodyExprs)
    | _ -> ...
```

The resolution scope and the region are extended in lockstep. On return, the
pop discards all the resolution bindings; the region was passed as a value so
the caller's `region` is already unchanged.

### Convenience: Enter/Exit Helpers

Because the two maps are almost always extended together, a pair of helpers
keeps call sites concise:

```fsharp
    /// Introduce a variable: mint Ident, allocate StorageRef, register in
    /// the frame's resolution env, and extend the given region.
    let mintVar (ctx: FrameCtx) (name: string) (region: StxEnvironment)
               : Ident * StorageRef * StxEnvironment =
        let id = Ident.fresh ()
        let storage = mintStorage ctx name
        ctx.ResEnv <- ResolutionEnv.addBinding id storage ctx.ResEnv
        id, storage, Map.add name (StxBinding.Variable id) region
```

### mintStorage

The `IsGlobal` / `ScopeDepth` check becomes a match on `Kind` and stack
depth:

```fsharp
    let mintStorage (ctx: FrameCtx) (name: string) : StorageRef =
        match ctx.Kind with
        | Global mangledName when List.length ctx.ResEnv <= 1 ->
            StorageRef.Global(mangledName, Field name)
        | _ ->
            StorageRef.Local(nextLocal ctx)
```

When the resolution stack has only one level (the frame's root scope) and
the frame is `Global`, we're at the true top level and emit a global field.
Any pushed scope (from `let` etc.) makes `List.length > 1`, forcing a local.
This replaces the `ScopeDepth` counter.

### Child Frame Creation

```fsharp
    let childForLambda (parent: FrameCtx) : FrameCtx =
        { Unit = parent.Unit
          Kind = Lambda { Parent = parent
                          Captures = []
                          HasDynEnv = false }
          ResEnv = ResolutionEnv.empty
          LocalCount = 0 }

    let childForLibrary (parent: FrameCtx) (mangledName: string) : FrameCtx =
        { Unit = parent.Unit
          Kind = Global mangledName
          ResEnv = ResolutionEnv.empty
          LocalCount = 0 }
```

A child frame starts with an empty resolution stack. Lambda formals are added
to its root scope via `ResolutionEnv.addBinding`. The parent's bindings are
reachable only through `resolveId`'s parent-chain walk, which automatically
triggers capture registration.

### Seeding Preloaded Bindings

Library imports and REPL preloads add bindings to the frame's root scope
without allocating new storage:

```fsharp
    let registerStorage (ctx: FrameCtx) (name: string) (storage: StorageRef)
                        (region: StxEnvironment) : StxEnvironment =
        let id = Ident.fresh ()
        ctx.ResEnv <- ResolutionEnv.addBinding id storage ctx.ResEnv
        Map.add name (StxBinding.Variable id) region
```

### Syntactic Closures Interaction

Closures only snapshot the region (`StxEnvironment`). They do not snapshot
the resolution environment. This is correct because:

1. `Ident`s are globally unique — once `Ident 42 -> StorageRef.Local 3` is
   registered on a frame, it's findable from anywhere that has access to that
   frame via the parent chain.
2. `resolveId` walks the resolution stack and then the parent chain, so a
   closure that introduces names referencing bindings from an outer frame
   will still resolve them correctly through capture.

The definition-context closure merge (when a macro expansion produces a
`define`) works exactly as today: diff the region before/after, splice new
region entries into the caller's region. Resolution entries for the new
`Ident`s are already on the frame because the expansion ran against the same
`FrameCtx`.

### `intoBody`

Producing the final `BoundBody` is the same as today, reading mutable state
from the frame:

```fsharp
    let intoBody (ctx: FrameCtx) (exprs: BoundExpr list) : BoundBody =
        let env =
            match ctx.Kind with
            | Lambda state when state.HasDynEnv -> Some []
            | _ -> None
        { Body = BoundExpr.Seq exprs
          Locals = ctx.LocalCount
          Captures =
              match ctx.Kind with
              | Lambda state -> state.Captures
              | Global _ -> []
          EnvMappings = env }
```

## Open Questions

1. **Should `ResolutionEnv` be a list or an array?** A list gives O(1) push
   and pop and O(n) lookup where n is the number of stacked scopes (not the
   number of bindings). Realistic Scheme nesting depths are small (~5-10
   scopes), so list walkover is negligible. An array would give O(1) indexed
   access but O(n) push/pop. List is preferred.

2. **Output threading: `BoundExpr` vs tuple.** Definition-position forms still
   need to return an extended region. The simplest approach is to keep the
   split: `expand` returns `BoundExpr`, `expandDefine` returns
   `BoundExpr * StxEnvironment`. A unified `BindResult` type is a future
   option but out of scope for this change.

3. **letrec uninit checking.** The current code tracks a list of uninitialised
   `Ident`s for `letrec` and walks the Stx tree checking for references. This
   still works — the `Ident`s are registered in the resolution env early (with
   their storage) and the check happens at the Stx level before expansion.
   No change needed.

4. **Stack depth for `mintStorage`.** Using `List.length ctx.ResEnv <= 1` is
   O(n). Since pushes and pops are balanced, a simple `int` depth counter
   alongside the stack would give O(1). Worth adding if profiling shows this
   path is hot, but the stack is typically 1-3 levels deep so it's unlikely
   to matter.

## Affected Files

| File | Change |
|------|--------|
| `src/Feersum.CompilerServices/Binding/Expand.fs` | Core refactor target. `ExpandCtx` splits into `CompilationUnit` + `FrameCtx` with `FrameKind` DU and `ResolutionEnv` scope stack. |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Thin shim that creates contexts and calls into `Expand`. Adopt the new types. |
| `src/Feersum.CompilerServices/Binding/Environments.fs` | `fromLibraries` and `builtin` updated to work with the new context types. |
| `src/Feersum.CompilerServices/Binding/Stx.fs` | `StxEnvironment` alias unchanged. `StxBinding` definition unchanged. |
| `src/Feersum.CompilerServices/Binding/Libraries.fs` | Unchanged. |
| `test/Feersum.Tests/` | No spec changes expected; only internal refactoring. |

## Future Work

The following ideas were discussed during the design of this change and are
explicitly deferred:

- **Unified `Scope` type** — Merge `StxEnvironment` and resolution env into a
  single `Scope` value with convenience methods (`Scope.addVariable`, etc.).
  Blocked on the syntactic-closures snapshotting asymmetry: closures capture
  the region but not the resolution env. Bundling makes the snapshot boundary
  unclear.
- **`BindResult` output type** — A composable result type for definition-context
  forms. Deferred until the tuple return proves unwieldy in practice.
- **Unified `Registry`** — Replace `MacroRegistry`, `Libraries` list, and
  resolution env with a single `Registry` keyed by `Ident`. Deferred until
  there is a concrete need for more compile-time meaning kinds (record types,
  inline hints, etc.).
