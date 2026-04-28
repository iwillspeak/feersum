# Workspace and Symbol API

This document proposes a unified implementation plan that addresses both
[issue #101 (Symbol API)](https://github.com/iwillspeak/feersum/issues/101)
and [issue #111 (Workspace API)](https://github.com/iwillspeak/feersum/issues/111).
Both issues describe two facets of the same underlying problem: the compiler
passes too much raw state across its boundaries and has no on-demand, layered
model suited to incremental or interactive use.

## Background

### Current State

The compiler today is a linear pipeline driven by `Compilation.compile` in
`Compile/Compiler.fs`. Each call:

1. Resolves builtin signatures by walking loaded assemblies.
2. Calls `Environments.fromLibraries` to produce a flat `Scope` shim
   (`Map<string, StorageRef>`).
3. Splits that shim with `Environments.intoParts` into a `StxEnvironment` and
   a `Map<Ident, StorageRef>`, then threads both through `Binder.bindProgram`
   or `Binder.bindScript`.
4. Passes the resulting `BoundSyntaxTree` through `Lower.lower` then
   `Emit.emit`.

This architecture has several weaknesses:

- **Scope is a shim.** `Scope = Map<string, StorageRef>` in `Environments.fs`
  is explicitly marked as temporary. It merges syntax and storage information
  into a single flat map, losing the hygiene-relevant identity separation that
  `Ident` was introduced to provide. The `intoParts` / `fromParts` round-trip
  is lossy (non-variable `StxBinding` entries are silently dropped).

- **No public symbol vocabulary.** There is no type for "what does this name
  mean?" that can be exposed to IDE tooling. Internal types like `StorageRef`
  and `StxBinding` leak across layer boundaries.

- **Document and library state is re-computed on every call.** The `SourceRegistry`
  is created fresh per compilation. `allLibs` is rebuilt by walking assembly
  metadata on each invocation. There is no caching layer.

- **Binder carries mutable document state.** `BinderCtx.CurrentDocument` (now
  removed, but see `Libraries.fs` `doc` parameter) means the binder cannot
  safely process multiple documents without resetting external state between
  calls.

- **No incremental or derived compilation model.** The REPL (`Repl.fs`) works
  around this by re-compiling everything from scratch, discarding all scope
  information between inputs.

### Relationship Between the Two Issues

The [Workspace API design](https://github.com/iwillspeak/feersum/issues/111)
defines the _structural_ layers of the compiler (what owns text, what owns
syntax trees, what owns semantic state). The [Symbol/Scope API
design](https://github.com/iwillspeak/feersum/issues/101) defines what the
semantic layer looks like internally (the `Scope.t` opaque type, `Symbol` as
the public vocabulary, `LibraryRegistry` for lazy import resolution, and
pickling for cross-assembly hygiene).

Both need to land together because `Workspace` is only useful if `Compilation`
has somewhere sensible to put its semantic state, and `Scope.t` is only useful
if there is a stable owner for it across calls.

## Design

### Overview: Three Layers

The unified model has three layers, each with a single owner:

```
+----------------------------------------------------------+
|  Compilation  (semantic pipeline: expand, lower, emit)   |
|  - owns Scope.t (opaque scope state)                     |
|  - owns BoundSyntaxTree / emitted assembly               |
|  - chainable: Compilation.derive for REPL continuations  |
+----------------------------------------------------------+
                          ^
                          |
+----------------------------------------------------------+
|  Workspace  (documents + parsed syntax)                  |
|  - owns SourceRegistry (DocId -> TextDocument)           |
|  - owns lazy parse cache (DocId -> RedNode)              |
|  - provides Workspace.getSyntaxTree : DocId -> RedNode   |
+----------------------------------------------------------+
                          ^
                          |
+----------------------------------------------------------+
|  Input  (source text, paths, synthetic nodes)            |
+----------------------------------------------------------+
```

---

### Layer 1: Workspace

`Workspace` is a thin wrapper around the existing `SourceRegistry` (from
`Text.fs`) that adds a lazy parse cache. The `SourceRegistry` implementation
that already exists is preserved unchanged; `Workspace` composes it.

```fsharp
/// Workspace — owns documents and their syntax trees.
type Workspace =
    { Registry   : SourceRegistry
      mutable ParseCache : Map<DocId, RedNode> }

module Workspace =

    /// Create an empty workspace with no documents.
    let empty () : Workspace =
        { Registry   = SourceRegistry.empty ()
          ParseCache = Map.empty }

    /// Register a source document and return its stable DocId.
    /// Does not parse — parsing is deferred until getSyntaxTree is called.
    let addDocument (path: string) (text: string) (ws: Workspace) : DocId * Workspace =
        let id = SourceRegistry.register ws.Registry path text
        id, ws

    /// Update an existing document's text, invalidating the cached parse.
    let updateDocument (id: DocId) (path: string) (text: string) (ws: Workspace) : Workspace =
        SourceRegistry.update ws.Registry id path text
        { ws with ParseCache = Map.remove id ws.ParseCache }

    /// Parse a document on demand, caching the result.
    let getSyntaxTree (id: DocId) (ws: Workspace) : RedNode * Workspace =
        match Map.tryFind id ws.ParseCache with
        | Some tree -> tree, ws
        | None ->
            let doc = SourceRegistry.tryLookup ws.Registry id |> Option.get
            let tree = Parse.parseText doc.Path (SourceRegistry.getText ws.Registry id)
            let ws' = { ws with ParseCache = Map.add id tree ws.ParseCache }
            tree, ws'

    /// Add a synthetic (compiler-generated) red node — for tests and macro expansion.
    /// Uses DocId.Synthetic; no source document is registered.
    let addSyntheticSyntax (tree: RedNode) (ws: Workspace) : DocId * Workspace =
        DocId.Synthetic, ws
```

The `Workspace` type is intentionally value-like (a record). Mutation of the
`SourceRegistry`'s internal list (which uses `Generic.List`) and the
`ParseCache` are the only mutable parts, both of which are implementation
details. The LSP host may hold a single `Workspace` and update it incrementally
via `updateDocument`.

---

### Layer 2: Scope.t and Symbol

`Scope.t` is the opaque type that replaces the shim `Scope = Map<string,
StorageRef>` in `Environments.fs`. It encapsulates two internal maps — a
`SyntaxEnv` and a `BindingEnv` — and exposes only a `Symbol`-valued query
API. Neither internal map is ever exposed to callers.

```fsharp
/// A single binding in the syntax environment.
/// (internal to Binding/Scope.fs — not exposed)
type private SyntaxEntry =
    | Builtin  of SpecialFormKind
    | Macro    of Ident
    | Var      of Ident

type private SyntaxEnv  = Map<string, SyntaxEntry>  // forked / scoped per lambda/let
type private BindingEnv = Map<Ident,  StorageRef>    // append-only accumulator

/// Opaque scope type — the single type external callers interact with.
[<NoComparison; NoEquality>]
type Scope = private { Syntax: SyntaxEnv; Bindings: BindingEnv }

/// The public vocabulary for what a name means.
/// Produced by Scope.lookup and handed to IDE tooling.
type Symbol =
    | BuiltinSymbol  of name: string * kind: SpecialFormKind
    | MacroSymbol    of name: string * ident: Ident * transformer: SyntaxTransformer
    | GlobalSymbol   of name: string * ident: Ident * storage: StorageRef
    | LocalSymbol    of name: string * ident: Ident * storage: StorageRef
    | ArgumentSymbol of name: string * ident: Ident * index: int
    | CapturedSymbol of name: string * ident: Ident * index: int

module Scope =

    // -- Construction ---------------------------------------------------------

    let empty : Scope = { Syntax = Map.empty; Bindings = Map.empty }

    /// Extend the scope with a named symbol.
    val extend : string -> Symbol -> Scope -> Scope

    // -- Query ----------------------------------------------------------------

    /// Look up a plain name; returns the symbol if in scope.
    val lookup : string -> Scope -> Symbol option

    /// Enumerate all visible name/symbol pairs — for REPL completion, debug.
    val bindings : Scope -> (string * Symbol) list

    // -- Interop with existing types ------------------------------------------

    /// Produce a Scope from an existing list of library signatures.
    /// Used during the transition while LibraryRegistry is not yet wired in.
    val ofLibraries : LibrarySignature<StorageRef> list -> Scope

    /// Extract the StxEnvironment and Map<Ident, StorageRef> pair from a Scope
    /// for use inside the binder. This is the only place the internal maps are
    /// accessed from outside Scope.fs.
    val internal toParts : Scope -> StxEnvironment * Map<Ident, StorageRef>

    /// Reconstruct a Scope from its constituent parts.
    /// Used by the binder to return an updated scope after binding.
    val internal fromParts : StxEnvironment * Map<Ident, StorageRef> -> Scope

    // -- Import ---------------------------------------------------------------

    /// Restore a compiled scope from assembly metadata and merge it in.
    val import : CompiledScope -> Scope -> Scope
```

The `Scope.toParts` / `Scope.fromParts` pair is intentionally `internal`; it
bridges the existing `Binder.fs` implementation without exposing the maps
publicly. As the binder is refactored in later phases these calls can be
removed.

### LibraryRegistry

The `LibraryRegistry` replaces the ad-hoc `allLibs: LibrarySignature<StorageRef>
list` that is currently passed through every call to `Binder.bindProgram`.

```fsharp
/// A fully loaded library — either compiled or restored from an assembly.
type LibraryScope =
    { Name    : string list
      Exports : CompiledScope }

/// Registry of libraries that are available to be imported.
/// Libraries in the registry are not yet visible in any Scope; they are
/// pulled in lazily when the expander processes an (import ...) form.
type LibraryRegistry

module LibraryRegistry =
    val empty  : LibraryRegistry
    val add    : LibraryScope -> LibraryRegistry -> LibraryRegistry
    val find   : string list  -> LibraryRegistry -> LibraryScope option

module Scope =
    // (additions for registry-backed scopes)

    /// Start from an empty visible scope but wire up a registry for
    /// (import ...) to resolve against. Use for fresh file compilations.
    val withRegistry : LibraryRegistry -> Scope

    /// Pre-open a list of libraries directly into the visible scope.
    /// Use for REPL sessions and pre-opened base environments.
    val merge : LibraryScope list -> Scope

    /// Attach a registry to an already-merged scope.
    val attachRegistry : LibraryRegistry -> Scope -> Scope

    /// Called by the expander when it encounters (import <name>).
    /// Returns Error if the library is not in the registry.
    val importLibrary : string list -> Scope -> Result<Scope, string>
```

---

### Layer 3: Compilation

`Compilation` owns the semantic state for a given expansion pass and is the
primary public entry point into the compiler pipeline.

```fsharp
/// Compilation — owns the semantic pipeline for a set of documents.
type Compilation =
    { Workspace  : Workspace
      Scope      : Scope
      Previous   : Compilation option }

module Compilation =

    /// Create a new compilation from a workspace and initial scope.
    val create : Workspace -> Scope -> Compilation

    /// Derive a new compilation from an existing one.
    /// The derived compilation shares the previous compilation's scope as its
    /// starting point — this is the natural model for REPL continuations.
    val derive : Workspace -> Compilation -> Compilation
```

The `Expand` module provides the two entry points into the expansion/binding
pass:

```fsharp
module Expand =

    /// Expand and bind all documents in the compilation.
    /// Returns the bound tree and the updated scope (for chaining into derive).
    val run : Compilation -> BoundSyntaxTree * Scope

    /// Expand and bind, also producing per-node metadata.
    /// The extra map associates every syntax node with the scope visible at
    /// that point and the symbol it resolved to. This is the path for LSP
    /// hover, go-to-definition, and completion.
    val runRich : Compilation -> BoundSyntaxTree * Scope * Map<NodeId, NodeInfo>

/// Per-node information produced by Expand.runRich.
type NodeInfo =
    { ScopeAt : Scope
      Symbol  : Symbol option }
```

`run` and `runRich` share the same internal recursive walk. `runRich` threads a
`NodeInfo` accumulator alongside the existing expansion logic — there is no
separate code path to maintain.

The updated REPL loop then reads:

```fsharp
let repl () =
    let mutable comp = Compilation.create (Workspace.empty ()) baseScope
    while true do
        let line   = Console.ReadLine()
        let ws     = Workspace.empty () |> Workspace.addDocument "repl" line |> snd
        let tree, scope = Expand.run comp
        comp <- Compilation.derive ws { comp with Scope = scope }
        eval tree
```

---

### Library Pickling

When a library is compiled to a .NET assembly its scope must be serialised into
the assembly metadata so importing compilations can restore it — with full
hygiene — without re-compiling the source.

```fsharp
/// Stable cross-assembly identity for a definition.
type StableIdent =
    { ModulePath : string list
      Name       : string }

/// A syntax entry expressed in stable idents — safe to embed in metadata.
type StableSyntaxEntry =
    | StableBuiltin of SpecialFormKind
    | StableMacro   of PickledMacro
    | StableVar     of StableIdent

/// The serialisable form of a macro.
type PickledMacro =
    { /// Reference to the compiled transformer delegate in IL.
      TransformerRef  : StableIdent
      /// Snapshot of the syntax environment at definition time.
      /// May include non-exported names required for hygiene.
      DefSiteSyntax   : Map<string, StableSyntaxEntry> }

/// What gets embedded as metadata in the compiled .NET assembly.
type CompiledScope =
    { /// Only the exported names (listed in (export ...)).
      PublicSyntax  : Map<string, StableSyntaxEntry>
      /// Full definition-site syntax for every exported macro.
      MacroClosures : Map<string, PickledMacro> }
```

`Scope.import` (shown above) is the sole consumer of `CompiledScope` from
external callers. The restoration logic (`restoreMacro`) is private to
`Scope.fs`.

---

### Bootstrap

The existing `Builtins.fs` bootstrap sequence (loading core signatures from
runtime reflection) is replaced by a proper layered bootstrap:

```fsharp
module Compiler =

    /// Build the base LibraryRegistry for a compilation.
    /// Loads the primitive scope from hardwired special forms, then compiles
    /// the builtin macro library (and, or, when, cond, ...) against it.
    let bootstrap (target: TargetInfo) : LibraryRegistry =
        let primitiveScope = Scope.fromPrimitives SpecialFormKind.all
        let primitiveRegistry =
            LibraryRegistry.empty
            |> LibraryRegistry.add { Name = ["compiler"; "primitives"]
                                     Exports = Scope.toCompiledScope primitiveScope }
        // Load builtin Scheme macros from the embedded .sld resource
        let builtinSource = EmbeddedResources.load "builtins.sld"
        let builtinLib    =
            builtinSource
            |> Parse.parse
            |> compileLibrary (Scope.withRegistry primitiveRegistry)
        primitiveRegistry |> LibraryRegistry.add builtinLib

    /// Scope for a normal user file: pre-opens primitives + builtins,
    /// registry-backed for (import ...) of other libraries.
    let scopeForFile (registry: LibraryRegistry) : Scope =
        Scope.merge [
            LibraryRegistry.find ["compiler"; "primitives"] registry |> Option.get
            LibraryRegistry.find ["compiler"; "builtins"]   registry |> Option.get
        ]
        |> Scope.attachRegistry registry
```

---

## Implementation Phases

The changes are large enough to require phased delivery. Each phase is
independently releasable and keeps the test suite green.

### Phase 1 — Proper Scope.t and Symbol

- Add `Binding/Scope.fs` with `Scope` (opaque), `Symbol`, and `module Scope`.
- Implement `Scope.toParts` / `Scope.fromParts` to bridge the existing binder.
- Replace the shim `type Scope = Map<string, StorageRef>` in `Environments.fs`
  with delegating wrappers that call the new `Scope` module.
- Update `Compiler.fs` to construct and thread a `Scope` value instead of
  manually splitting `(StxEnvironment, Map<Ident, StorageRef>)`.
- No external API changes; all tests should pass unchanged.

### Phase 2 — Workspace

- Add `Workspace.fs` wrapping `SourceRegistry` + parse cache.
- Change `CompileInput` in `Compiler.fs` to accept a `Workspace` instead of a
  raw `SourceRegistry`.
- Update `Repl.fs`, `ParseRepl.fs`, and `compileFiles` to construct a
  `Workspace`.
- Remove the `SourceRegistry` construction from `Compiler.compile` callers.

### Phase 3 — Compilation and Expand entry points

- Add `Compilation.fs` with `Compilation` type and `Expand.run` / `Expand.runRich`.
- Migrate `Compiler.compile` to delegate to `Expand.run` internally.
- Update `Eval.fs` to use `Compilation.create` + `Expand.run`.
- Update `Repl.fs` to use `Compilation.derive` for stateful REPL continuations.

### Phase 4 — LibraryRegistry

- Add `LibraryRegistry` to `Binding/Scope.fs`.
- Replace the `allLibs: LibrarySignature<StorageRef> list` argument threading
  in `Binder.bindProgram` / `Binder.bindScript` with `Scope.importLibrary` calls
  driven by the registry.
- Move bootstrap assembly loading from `Builtins.fs` reflection into
  `LibraryRegistry.add` calls in `Compiler.bootstrap`.

### Phase 5 — Library Pickling

- Add `PickledMacro`, `StableSyntaxEntry`, and `CompiledScope` to
  `Binding/Scope.fs`.
- Implement `Scope.import` + `restoreMacro` (private).
- Update `Emit.fs` to embed a `CompiledScope` as assembly metadata when
  `OutputType = Lib`.
- Update `Builtins.loadReferencedSignatures` (in `Builtins.fs`) to call
  `Scope.import` when restoring foreign libraries.

### Phase 6 — Expand.runRich and LSP plumbing (stretch goal)

- Thread the `Map<NodeId, NodeInfo>` accumulator through the binder.
- Wire `runRich` output into the LSP server for hover and go-to-definition.

---

## Affected Files

| File | Change |
|------|--------|
| `src/Feersum.CompilerServices/Binding/Scope.fs` | **New.** `Scope` (opaque), `Symbol`, `LibraryRegistry`, `CompiledScope`, `PickledMacro`. |
| `src/Feersum.CompilerServices/Binding/Environments.fs` | Remove shim `Scope = Map<...>`; update `fromLibraries`, `intoParts`, `fromParts` to delegate to new `Scope` module. Eventually deleted. |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Accept `Scope` instead of the manual `(StxEnvironment, Map<Ident, StorageRef>)` pair; use `Scope.toParts` internally during transition. |
| `src/Feersum.CompilerServices/Workspace.fs` | **New.** `Workspace` type wrapping `SourceRegistry` + parse cache. |
| `src/Feersum.CompilerServices/Compilation.fs` | **New.** `Compilation` type; `Expand.run` / `Expand.runRich`; `NodeInfo`. |
| `src/Feersum.CompilerServices/Compile/Compiler.fs` | Simplify: delegate to `Compilation` + `Expand.run`; remove manual scope splitting; add `Compiler.bootstrap`. |
| `src/Feersum.CompilerServices/Compile/Builtins.fs` | Migrate reflection-based loading to produce `LibraryScope` values; remove `loadCoreSignatures` / `loadBuiltinMacroEnv` after Phase 4. |
| `src/Feersum.CompilerServices/Eval.fs` | Use `Compilation.create` + `Expand.run` instead of `Compilation.compile` directly. |
| `src/Feersum/Repl.fs` | Use `Workspace` + `Compilation.derive` for stateful REPL continuations. |
| `src/Feersum/ParseRepl.fs` | Construct `Workspace` rather than raw `SourceRegistry`. |
| `src/Feersum.CompilerServices/Compile/Emit.fs` | Embed `CompiledScope` metadata into `Lib` output assemblies (Phase 5). |

## Open Questions

1. **Scope mutability model.** The workspace doc proposes immutable value types
   ("derive a new workspace") while the existing `SourceRegistry` uses a mutable
   `Generic.List`. Should `Workspace` expose an immutable-value interface with
   copy-on-write semantics, or keep the mutable-interior model and document it?
   The mutable model is simpler for the LSP (which updates documents in place);
   the immutable model is safer for concurrent analysis. Preferred direction:
   keep mutable interior for now, expose a functional-looking API surface.

2. **Arena-based spans.** The workspace design doc proposes `SpanArena` and
   `AstArena` for compact, stable ID-based storage of semantic information.
   This is more ambitious than the `NodeInfo` map approach from the scope design.
   Should Phase 6 adopt the full arena model or start with the simpler
   `Map<NodeId, NodeInfo>`? Preferred direction: start with `Map<NodeId,
   NodeInfo>` — arenas can be introduced later if profiling shows allocation
   pressure.

3. **`CompiledScope` serialisation format.** The design leaves the on-disk format
   (custom attribute bytes, embedded resource, custom metadata table) unspecified.
   Preferred direction: custom assembly attribute, mirroring how Roslyn embeds
   embedded source and PDB data.

4. **`Scope.fromPrimitives` source of truth.** The bootstrap description says
   "hardwired into the expander — if, lambda, define, quote, set! ...". Today
   these are recognised by name in `Binder.fs:tryResolveSpecial`. Should
   `SpecialFormKind.all` be an explicit list, or derived from the DU cases via
   reflection? Preferred direction: explicit list — keeps the primitive set
   intentional and auditable.

5. **Backwards compatibility of `Compilation.compile`.** Several test helpers
   and the Feersum CLI call `Compilation.compile` directly. Should it be kept
   as a convenience wrapper over the new `Expand.run` indefinitely, or
   deprecated after Phase 3? Preferred direction: keep as a thin shim for one
   release cycle, then deprecate.
