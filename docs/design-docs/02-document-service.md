# Document Service

This document outlines an improved approach to source locations, provenance,
and text document handling. The aim is to introduce a `DocumentService` which
manages interactions with source documents. It provides a means to load both
real and synthetic documents and parse them into syntax trees.

## Background

Currently, source locations are concrete values that embed a file path, line,
and column directly. This makes it impossible to represent locations in
synthetic (in-memory) documents, complicates macro-expansion provenance, and
couples diagnostics to the resolved `TextPoint` / `TextLocation` representation
rather than to a stable identity.

### The ProvenanceId Conflation

`ProvenanceId` in `Text.fs` currently encodes two orthogonal concerns in one
import:

 * **Positive** IDs are indices into a `ProvenanceTable` of `ProvenanceEntry`
   values. Today the only variant is `SourceText of TextDocument`, so a
   positive `ProvenanceId` is effectively an alias for "which source file".
 * **Negative** IDs (produced by a global `Interlocked.Decrement` counter) mark
   compiler-generated syntax that has no source origin. This is used by the
   synthetic factory helpers in `Factories.fs` and by macro expansion in
   `Macros.fs`.

Macro expansion provenance — knowing *which template site* produced a given
node, and *which call site* triggered the expansion — is not tracked at all.

A second problem follows: despite each `AstItem` carrying a `ProvenanceId`,
the binder **does not use it for location resolution**. Instead, `BinderCtx`
carries `mutable CurrentDocument: TextDocument option` which is set once per
compilation unit at the `bind` entry point. `Libraries.fs` mirrors this by
passing a `doc: TextDocument option` argument through its helpers. This means
the `ProvenanceId` stored on every AST node is today unused after parsing.

The new design resolves both problems by pulling document identity and
expansion history apart into separate, purpose-built structures.

## Design

### Source Registry and Expansion Provenance

Each source document is represented by two pieces of information:

 * The document trivia (name, line-ending positions, virtual vs. real, etc.)
 * The **syntax tree** (`Tree.Program` or similar)

We propose introducing a `SourceRegistry` and an `ExpansionProvenance` table.
The former allows documents to be loaded into the compilation workspace before
parsing and error reporting. The latter records the relationship between source
documents and the expansion fragments produced by macros, enabling provenance
chains in diagnostics.


```fsharp
/// Opaque ID for the document. This is the index of the document into the
/// source registry that issued the ID.
type DocId = Doc of int

/// The file registry — owns source identity.
/// Populated at load time, before parsing.
type SourceRegistry =
    { mutable Files : Generic.List<TextDocument> }

and TextDocument =
    { Id       : DocId
      // ... existing fields (Name, LineStarts, etc.)
    }

/// The expansion provenance table — owns expansion history.
/// Populated during macro expansion, after parsing.
type ExpansionProvenance =
    { mutable Entries : Generic.List<ExpansionEntry> }

and ExpansionEntry =
    | Expansion   of {| CallSite  : SourceLocation
                        DefSite   : SourceLocation
                        MacroName : string |}
    | Transcribed of {| TemplateSite : SourceLocation
                        Expansion    : ExpansionRef |}
    | Substituted of {| Original     : SourceLocation
                        Expansion    : ExpansionRef |}

/// A location in the source registry — stable, never changes.
and SourceLocation =
    { Doc  : DocId
      Range : TextRange }

/// A reference into the expansion provenance table.
and ExpansionRef = ExpansionRef of int
```

With this structure, the binder can track `SourceLocation` values throughout
binding. Diagnostics can then refer to error locations using `SourceLocation`
rather than the current resolved `TextPoint` / `TextLocation` pairs. To render
a diagnostic as a human-readable message, the compiler walks the provenance
chain and uses the resulting `SourceLocation`s to look up the corresponding
`TextDocument` by `DocId`.

Each node in the CST `Tree` then has an additional `DocId` property:

```fsharp
[<AbstractClass>]
type AstItem internal (red: NodeOrToken<SyntaxNode, SyntaxToken>, doc: DocId) =

    /// Get the source document ID of this node
    member public _.DocId = doc
```

Compiler-generated (synthetic) AST nodes — those created by `Factories.fs`
helpers for macro expansion templates or test data — have no source document.
Rather than a global counter of negative IDs, add a `Synthetic` union case to
`DocId`:

```fsharp
type DocId =
    | Doc      of int   // Index into SourceRegistry
    | Synthetic         // Compiler-generated; no source origin
```

This keeps `DocId` self-describing and removes the global mutable counter in
`ProvenanceId.makeSynthetic`.

Location references in the **bound tree** then become:

```fsharp
type TreeNodeRef =
    | Source of SourceLocation                        // Direct source
    | Expanded of SourceLocation * ExpansionRef       // Macro expansion (records the location and the macro it came from)
    | Missing                                         // Synthetic nodes: error recovery, compiler-generated forms, etc.
```

### Eliminating `CurrentDocument` from the binder

Because each `AstItem` now carries a `DocId` that can be resolved directly
against the `SourceRegistry`, the binder no longer needs to carry
`CurrentDocument` as mutable state. Replace `getNodeLocation ctx expr` with
a function that looks up `expr.DocId` in the `SourceRegistry`:

```fsharp
let private getNodeLocation (registry: SourceRegistry) (expr: Expression) =
    match expr.DocId with
    | DocId.Synthetic -> TextLocation.Missing
    | DocId.Doc _ ->
        let loc = { Doc = expr.DocId; Range = expr.SyntaxRange }
        SourceRegistry.resolveLocation registry loc
```

The `SourceRegistry.resolveLocation` function walks `SourceLocation → TextDocument
→ TextLocation` in one place. This removes the `CurrentDocument` field from
`BinderCtx`, the mutable assignment at each `bind` call site, and the
propagation of `doc: TextDocument option` through `Libraries.fs`.

### Test API Continuity

The convenience wrappers `Parse.readProgramSimple` and `Parse.readExpr1Simple`
create an internal `ProvenanceTable` today. After the refactoring they can be
re-implemented on top of an internal `DocumentService.create()`. Their
signatures remain unchanged, so tests that call them require no edits.

`Factories.fs` currently calls `ProvenanceId.makeSynthetic()` for every
synthetic node. Replace all uses with `DocId.Synthetic`.

Diagnostic sanitisation helpers in `test/Feersum.Tests/SyntaxUtils.fs`
operating on `TextLocation` / `TextPoint` values are unaffected — those types
stay as the rendered, human-facing form.

### Document Service Layer

The `DocumentService` provides a convenience layer over the underlying
`SourceRegistry`. It handles loading, lazy parsing, and cache invalidation:


```fsharp
type DocumentService =
    { Registry  : SourceRegistry
      mutable ParseCache : Map<DocId, RedNode> }

module DocumentService =

    let create () =
        { Registry   = { Files = new Generic.List<TextDocument> }
          ParseCache = Map.empty }

    /// Load source text. Returns a stable DocId.
    /// Does not parse — parsing is lazy / on demand.
    let load
        (svc  : DocumentService)
        (path : string option)
        (text : string)
        : DocId =
        let id = DocId svc.Registry.Count
        let file = TextDocument.fromParts path text
        svc.Registry.Add(file)
        id

    /// Parse a loaded file. Result is cached; re-parse on text change.
    let parse
        (svc : DocumentService)
        (id  : DocId)
        : RedNode =
        match Map.tryFind id svc.ParseCache with
        | Some tree -> tree
        | None ->
            let file = svc.Registry.Files[let (DocId i) = id in i]
            let tree = Parser.parse file.Text id   // DocId goes into SourceLocation on each node
            svc.ParseCache <- Map.add id tree svc.ParseCache
            tree

    /// Notify of a text change. Invalidates the parse cache entry.
    /// In a salsa-style system this would be the input mutation point.
    let update
        (svc  : DocumentService)
        (id   : DocId)
        (text : string)
        : unit =
        let i = let (DocId i) = id in i
        svc.Registry.Files[i] <-
            { svc.Registry.Files[i] with
                Text    = text
                LineMap = buildLineMap text }
        svc.ParseCache <- Map.remove id svc.ParseCache
```

## Affected Files

| File | Change |
|------|--------|
| `src/Feersum.CompilerServices/Text.fs` | Add `DocId` (with `Synthetic` case), `SourceLocation`, `SourceRegistry`; remove `ProvenanceId`, `ProvenanceEntry`, `ProvenanceTable`; add `Id: DocId` field to `TextDocument`; add `SourceRegistry.resolveLocation` |
| `src/Feersum.CompilerServices/Syntax/Tree.fs` | Change `AstItem` constructor from `ProvenanceId` to `DocId`; expose `DocId` member; add `TreeNodeRef` type |
| `src/Feersum.CompilerServices/Syntax/Parse.fs` | Replace `ProvenanceTable` parameter with `SourceRegistry`; assign `DocId` from registry to parsed nodes; keep `readProgramSimple` / `readExpr1Simple` wrappers |
| `src/Feersum.CompilerServices/Syntax/Factories.fs` | Replace all `ProvenanceId.makeSynthetic()` calls with `DocId.Synthetic` |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Remove `CurrentDocument` from `BinderCtx`; thread `SourceRegistry` instead; rewrite `getNodeLocation` to resolve `DocId` |
| `src/Feersum.CompilerServices/Binding/Libraries.fs` | Remove `doc: TextDocument option` parameter from helpers; use `SourceRegistry` for location resolution |
| `src/Feersum.CompilerServices/Binding/Macros.fs` | Replace `ProvenanceId.makeSynthetic()` with `DocId.Synthetic` in expansion helpers |
| `src/Feersum.CompilerServices/Compile/Compiler.fs` | Replace bare `ProvenanceTable.empty()` + `readProgram` calls with `DocumentService.load` + `DocumentService.parse` |
| `src/Feersum/Repl.fs` | Replace `ProvenanceTable` with `DocumentService`; call `DocumentService.update` on each new REPL input |
| `src/Feersum/ParseRepl.fs` | Same as `Repl.fs` |
| `src/Feersum.CompilerServices/DocumentService.fs` *(new)* | New file: `DocumentService` type and module; `SourceRegistry`; `ExpansionProvenance`; `ExpansionEntry`; `ExpansionRef` |
| `test/Feersum.Tests/SyntaxUtils.fs` | No changes needed |
| `test/Feersum.Tests/TextTests.fs` | Update `TextDocument.fromParts` call sites to supply a `DocId` |

## Open Questions

1. [x] **`ExpansionProvenance` ownership** — Should `ExpansionProvenance` live inside
       `DocumentService`, or be a separate object threaded through macro expansion?
       Keeping it inside `DocumentService` is simpler; a separate object allows
       the macro expander to be tested without a full `DocumentService`.
       *Preferred: separate, passed alongside the `SourceRegistry` into the binder.*

        **Decision**: We should treat provenance as an expansion / binding phase concern. Passed
        separately rather than owned by the document service.

2. [x] **`Diagnostic.Location` type** — Should `Diagnostic.Location` change from
        `TextLocation` to `TreeNodeRef` (deferring resolution until display time),
        or stay as `TextLocation` (resolved eagerly at emit time)? A lazy
        `TreeNodeRef` would enable richer error messages (e.g. showing the macro
        call site) without changing the `Diagnostic` API visible to callers.
        *Preferred: keep `TextLocation` for now; add a separate provenance-chain
        field when macro error reporting is implemented.*


        **Decision**: Diagnostics shoudl probably have a list of additional notes. This will more
        closely mimic the LSP protocol.

3. [x] **`SourceRegistry` mutability** — The design uses a mutable `Generic.List`
        for both the registry and the provenance table. An immutable persistent
        vector would allow cheap snapshots (e.g. for incremental recompilation).
        *Preferred: keep mutable for the initial implementation; revisit when
        adding incremental / LSP support.*

        **Decision**: Leave mutable for now.

4. [x] **Snapshot test stability** — `SourceLocation` values (pairs of `DocId *
        TextRange`) are not human-readable. Snapshot tests that currently serialise
        `TextLocation` strings will need a rendering helper that resolves
        `SourceLocation` → `TextPoint` for comparison. Confirm whether existing
        `.ast` snapshot format is affected before landing this change.


        **Decision**: Source tree locations in the AST are 'mapped' already. We can
        ensure that the appropriate mapping / sanitisation takes place so that the
        existing snapshots don't need to change.
