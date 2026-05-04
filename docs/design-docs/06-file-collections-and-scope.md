# File Collections, Scope, and Symbol API

This document revises and supersedes the compilation-model portions of
[Workspace and Symbol API](./workspace-and-symbol-api/) in light of
implementation changes made since that document was written. The symbol API
design in the earlier document remains the authoritative source for `Scope.t`,
`Symbol`, and library pickling; this document updates only the structural
architecture and refines the bound-tree model.

## Background

### What Changed

[Design doc 05](./workspace-and-symbol-api/) was written before three
significant implementation changes landed on `feature/workspaces`:

1. **`SourceRegistry` removed.** The mutable registry that assigned `DocId`
   values and stored raw text has been deleted from `Text.fs`. There is no
   longer a central store of source text inside the compiler.

2. **`DocId` removed from the CST.** Individual green/red tree nodes no longer
   carry a document identity. Source provenance is now held by the
   `SyntaxRoot<'a>` wrapper that the parser returns, not by the tree itself.
   This means a syntax tree is a fully portable, self-contained value — it can
   be passed around, stored, cloned, and processed without any reference to a
   registry.

3. **`StxPos` is now compact.** Syntax positions store a `TextDocument` and a
   raw `Firethorn.TextRange` rather than pre-resolved line/column pairs.
   Resolving to human-readable `TextLocation` is deferred until a diagnostic is
   actually emitted.

The net effect of these three changes is that **syntax trees are isolated
items**. A `SyntaxRoot<Program>` knows its source document and its tree; it
needs nothing else to be a valid, complete input to the binder.

The compiler-level `Workspace` proposed in doc 05 — a thin wrapper around
`SourceRegistry` with a lazy parse cache — is therefore no longer needed or
appropriate at the compiler layer. The `Workspace` concept now lives exclusively
in the LSP layer, where it already exists as `WorkspaceAgent` in
`Feersum.LanguageServer`.

### Remaining Problems

The architecture of `Scope` and `BoundSyntaxTree` is still largely as described
in doc 05's "Current State" section:

- `Scope = Map<string, StorageRef>` in `Environments.fs` is explicitly labelled
  a shim and conflates syntax identity with storage location.
- `BoundSyntaxTree` carries no symbol table: there is no record of which
  identifiers are definitions, which are references, or what is exported.
- The binder entry points (`bindProgram`, `bindScript`) accept `allLibs` as a
  raw list threaded through every call, and return a `BoundSyntaxTree` with no
  outward-facing scope. Chaining compilations for the REPL requires
  re-compiling from scratch.
- `Compilation.compile` in `Compiler.fs` is a monolithic function that
  rebuilds all library signatures on every invocation.

---

## Design

### Revised Architecture: Two Layers

The three-layer model from doc 05 collapses to two layers now that the compiler
no longer owns source text:

```
+------------------------------------------------------+
|  Compilation  (bind -> lower -> emit)                |
|  - accepts Scope (input environment)                  |
|  - returns BoundSyntaxTree + Scope (output env)       |
|  - input is a FileCollection or single SyntaxRoot     |
+------------------------------------------------------+
                        ^
                        |  SyntaxRoot<'a>
                        |
+------------------------------------------------------+
|  Parse  (text -> SyntaxRoot)                          |
|  - Parse.readProgram / readScript / readExpr1         |
|  - returns SyntaxRoot<Program> / SyntaxRoot<Script>   |
|  - no global state; caller owns the result            |
+------------------------------------------------------+
```

The LSP layer sits **beside** this pipeline, not inside it:

```
+------------------------------------------+
|  WorkspaceAgent  (LSP actor)             |
|  - stores document text + versions       |
|  - calls Parse.readProgram on demand     |
|  - feeds SyntaxRoot values to binder     |
+------------------------------------------+
```

`WorkspaceAgent` (already in `Feersum.LanguageServer/Workspace.fs`) is the
correct and complete home for LSP document tracking. No workspace type belongs
in `Feersum.CompilerServices`.

---

### Layer 1: File Collections

A **file collection** is the natural input type for multi-file compilations. It
is simply a list of already-parsed `SyntaxRoot<Program>` values. No wrapper
type is required; the list itself is the collection.

```fsharp
/// A parsed program file ready for the binder.
type FileCollection = SyntaxRoot<Program> list
```

Helper functions in the `Parse` module or a thin `FileCollection` module can
construct these from paths on disk:

```fsharp
module FileCollection =

    /// Parse each path, accumulating diagnostics. Returns a ParseResult whose
    /// Root is the list of successfully parsed roots.
    val ofPaths : string list -> ParseResult<FileCollection>

    /// Build a collection directly from already-parsed roots (tests, LSP).
    val ofRoots : SyntaxRoot<Program> list -> FileCollection
```

`compileFiles` in `Compiler.fs` is then reduced to:

```fsharp
let compileFiles options output paths =
    let result = FileCollection.ofPaths paths
    if hasErrors result.Diagnostics then result.Diagnostics
    else compile options output (CompileInput.Program result.Root)
```

---

### Layer 2: Scope as the Environment Type

`Scope` replaces `Map<string, StorageRef>` (the shim in `Environments.fs`) as
the canonical environment flowing into and out of the binder.

The `Scope` type and `Symbol` vocabulary are defined in detail in
[Workspace and Symbol API](./workspace-and-symbol-api/). The key addition here
is making `Scope` the **explicit output** of every binder entry point, not just
an internal detail.

#### Binder Entry Points

```fsharp
module Binder =

    /// Bind a script. Returns the bound tree and the updated scope containing
    /// any top-level definitions made during binding. The output scope is the
    /// correct input scope for the next REPL step.
    val bindScript :
        scope: Scope
        -> input: SyntaxRoot<ScriptProgram>
        -> BoundSyntaxTree * Scope

    /// Bind a program (one or more library files). Returns the bound tree and
    /// the scope of exported names. The output scope can be fed into
    /// LibraryRegistry.add to make the library importable.
    val bindProgram :
        scope: Scope
        -> inputs: FileCollection
        -> BoundSyntaxTree * Scope
```

The `allLibs: LibrarySignature<StorageRef> list` argument disappears: import
resolution is driven by the `LibraryRegistry` embedded in the `Scope` (as
described in doc 05). The binder calls `Scope.importLibrary` when it encounters
an `(import ...)` form.

#### REPL Chaining

With `Scope` as an explicit output the REPL loop becomes a straightforward fold:

```fsharp
let runRepl () =
    let mutable scope = Compiler.baseScope ()  // builtins pre-opened

    while true do
        let input = readLine ()                     // SyntaxRoot<ScriptProgram>
        let tree, scope' = Binder.bindScript scope input
        eval tree                                   // lower -> emit -> invoke
        scope <- scope'
```

No `Compilation.derive`, no re-seeding from scratch. The scope accumulates
incrementally, carrying all top-level definitions forward between inputs.

#### Export / Import Model

For library compilations the output `Scope` from `bindProgram` contains all
names defined within the library. The caller applies export filtering before
registering the library:

```fsharp
// Inside the compiler pipeline, after bindProgram:
let exportedScope = Scope.filterToExports exportSet outputScope
let libScope = LibraryScope.ofScope libName exportedScope
let registry' = LibraryRegistry.add libScope registry
```

This separates the binder (which sees every definition) from the library
registry (which sees only the public API).

---

### Layer 3: Bound Syntax Tree Symbol Model

The `BoundSyntaxTree` currently carries only what is needed to drive code
emission (`BoundBody`, `MangledName`, `Diagnostics`). For IDE features — hover,
go-to-definition, find-all-references, rename — the bound tree must also carry
a **symbol table** that records every definition and reference in the file.

#### Core Types

```fsharp
/// Stable identity for a single binding introduction.
/// Two definitions of the same name at different points get different ids.
/// This is distinct from `Ident`, which lives in the syntax layer; a
/// `DefinitionId` lives in the semantic layer.
[<Struct>]
type DefinitionId = private DefinitionId of int

/// The kind of a definition.
type DefinitionKind =
    | TopLevel     // (define name ...)  at program / library scope
    | Local        // let / letrec / letrec* binding
    | Formal       // lambda formal parameter
    | Imported     // name introduced by (import ...)
    | SyntaxDef    // (define-syntax ...)

/// A single name introduction in the bound tree.
type Definition =
    { Id       : DefinitionId
      Name     : string
      Kind     : DefinitionKind
      Storage  : StorageRef
      Location : TextLocation }

/// A use of a previously introduced name.
type Reference =
    { Target   : DefinitionId
      Location : TextLocation }

/// The symbol table for a single bound compilation unit.
type SymbolTable =
    { /// All definitions introduced by this compilation unit,
      /// in source order.
      Definitions : Definition list

      /// All resolved name references within this compilation unit.
      References  : Reference list

      /// Names exported by this unit (for library compilations).
      /// Maps the exported string name to the definition that backs it.
      Exports     : Map<string, DefinitionId>

      /// Names imported into this unit, keyed by the local string name.
      /// The StorageRef matches the one in the originating library scope.
      Imports     : Map<string, StorageRef> }
```

#### Updated BoundSyntaxTree

```fsharp
type BoundSyntaxTree =
    { Root        : BoundBody
      MangledName : string
      Diagnostics : Diagnostic list
      /// Present only when the binder ran in 'rich' mode (e.g. for the LSP).
      /// Absent in fast-compile mode to avoid the allocation overhead.
      Symbols     : SymbolTable option }
```

The `Symbols` field is `None` by default. A separate entry point (or a flag on
the scope/options) selects rich mode:

```fsharp
module Binder =

    /// Fast path — no symbol table. Used by the CLI compiler and REPL eval.
    val bindScript : Scope -> SyntaxRoot<ScriptProgram> -> BoundSyntaxTree * Scope

    /// Rich path — symbol table populated. Used by the LSP for hover,
    /// go-to-definition, and find-references.
    val bindScriptRich : Scope -> SyntaxRoot<ScriptProgram> -> BoundSyntaxTree * Scope
```

The rich and fast paths share the same internal recursive traversal; the rich
path threads a mutable `SymbolTableBuilder` alongside the binder state and
freezes it into `SymbolTable` before returning.

#### Interaction with Scope

The `SymbolTable` and `Scope` are complementary:

- `Scope` is the **live environment** — what names are visible right now and
  where they live in storage. It flows between compilation steps.
- `SymbolTable` is the **historical record** — what was defined and referenced
  in a given file, and at what location. It is attached to the `BoundSyntaxTree`
  and can be queried by the LSP after binding is complete.

A definition recorded in `SymbolTable.Definitions` will have the same
`StorageRef` as the binding in the output `Scope`. This allows the LSP to
cross-reference: given a `Reference.Target : DefinitionId` and the
`SymbolTable`, find the `StorageRef`; then look up the `StorageRef` in the
`Scope` to determine if the definition is still live (relevant for REPL
incremental sessions).

---

### LSP Workspace Integration

`WorkspaceAgent` in `Feersum.LanguageServer` already correctly models the LSP
layer: it tracks document text and versions, accepts open/change/close messages,
and provides snapshots on demand. No changes to its structure are required.

The `CompilationManager` in `Feersum.LanguageServer` should evolve to:

1. Receive document snapshots from `WorkspaceAgent`.
2. Call `Parse.readProgram` to obtain `SyntaxRoot<Program>` values.
3. Call `Binder.bindScriptRich` (or `bindProgramRich`) with the current scope.
4. Store the resulting `BoundSyntaxTree` (with its `SymbolTable`) for LSP
   query handlers (hover, go-to-definition, semantic tokens).
5. Forward updated `Scope` values for the next incremental compilation.

The `CompilationManager` is the correct place to cache `SyntaxRoot` values
between document versions (to avoid re-parsing unchanged files). This parse
cache belongs at the LSP layer, not in the compiler core.

---

## Affected Files

| File | Change |
|------|--------|
| `src/Feersum.CompilerServices/Binding/Scope.fs` | **New.** `Scope` (opaque), `Symbol`, `LibraryRegistry`. See doc 05 for detail. |
| `src/Feersum.CompilerServices/Binding/Environments.fs` | Remove shim `type Scope = Map<...>`; eventually deleted. |
| `src/Feersum.CompilerServices/Binding/BoundTree.fs` | Add `DefinitionId`, `DefinitionKind`, `Definition`, `Reference`, `SymbolTable`. Add `Symbols` field to `BoundSyntaxTree`. |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Change entry points to accept and return `Scope`. Add `bindScriptRich` / `bindProgramRich` variants. Remove `allLibs` argument (drives from `Scope`). |
| `src/Feersum.CompilerServices/Compile/Compiler.fs` | Simplify `compile` to use `Scope`-threaded binder. Add `FileCollection` helpers. Remove `Workspace` references from doc 05 plan. |
| `src/Feersum.CompilerServices/Compile/Builtins.fs` | Migrate to produce `LibraryScope` values for registry; remove `loadCoreSignatures` after registry wired in. |
| `src/Feersum/Repl.fs` | Use `Scope` chaining instead of recompiling from scratch. |
| `src/Feersum.LanguageServer/CompilationManager.fs` | Evolve to use `bindScriptRich`, store `SymbolTable`, provide LSP query API. |
| `src/Feersum.LanguageServer/Workspace.fs` | No structural change; this is already the correct LSP actor model. |

Files from doc 05's table that are **no longer needed**:

| File | Reason |
|------|--------|
| `src/Feersum.CompilerServices/Workspace.fs` | Not created; `SourceRegistry` gone, parse cache belongs in LSP layer. |
| `src/Feersum.CompilerServices/Compilation.fs` | Not created; `Compilation` type superseded by plain `Scope` threading. |

---

## Open Questions

1. **`SymbolTableBuilder` concurrency model.** The rich binder path must
   accumulate definition and reference records while performing a recursive
   descent. A mutable `ResizeArray`-backed builder is simplest and avoids
   repeated list consing; an immutable accumulator is safer. Preferred
   direction: mutable builder, frozen to an immutable `SymbolTable` before the
   binder returns.

2. **`DefinitionId` lifetime.** Should `DefinitionId` values be stable across
   re-parses of the same file (useful for the LSP to correlate a reference
   across incremental compilations)? Content-addressing (e.g. a hash of name +
   location) would give stability at the cost of collision risk. Preferred
   direction: fresh monotonic integer per binding pass; the LSP re-queries on
   each re-bind. Stable IDs can be introduced later if needed.

3. **Scope output from `bindScript` for libraries.** `bindProgram` has a clear
   distinction between the internal scope (all definitions) and the exported
   scope (public API). For scripts this distinction does not exist — all
   top-level defines are implicitly public. Should `bindScript` return the full
   scope or should there be an explicit export step? Preferred direction: return
   the full scope; the REPL and any script-level tooling can filter as needed.

4. **`FileCollection` as a type alias vs. a new module.** Naming the list
   `FileCollection = SyntaxRoot<Program> list` and giving it a small module
   adds a convenient parse-from-paths helper but introduces an extra concept.
   Preferred direction: introduce the module to keep `compileFiles` clean, but
   keep the underlying representation as a plain list so callers never have to
   unwrap it.

5. **Interaction with library pickling.** When a library's `BoundSyntaxTree` is
   emitted to a `.dll`, the `SymbolTable.Exports` should feed the `CompiledScope`
   serialisation described in doc 05. The precise mapping (`DefinitionId` ->
   `StableIdent`) is deferred to the pickling phase. Preferred direction:
   `Emit.fs` walks `SymbolTable.Exports` when `OutputType = Lib` to build the
   `CompiledScope`; no other file needs to know about pickling.
