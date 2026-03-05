# Hygienic Macro Expansion

This document describes the design for introducing hygienic macro expansion to
Feersum, and a related set of improvements to source location tracking and
symbol provenance throughout the compiler pipeline.

## Background: The Problem

The current macro expander in `Feersum.CompilerServices.Binding.Macros` is
_unhygienic_. When a macro template introduces a new identifier (for example, a
`let`-bound temporary), that identifier is emitted as a plain symbol and
resolved by the binder in the call site's lexical scope. This means a user
program that happens to have a variable with the same name will silently capture
the macro's binding — or vice versa.

The `or` builtin macro in `Compile/Builtins.fs` works around this today with a
UUID as the temporary name:

```scheme
(syntax-rules ()
    ((or test1 test2 ...)
        (let ((|90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288| test1))
            (if |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288|
                |90a3b246-0d7b-4f47-8e1e-0a9f0e7e3288|
                (or test2 ...)))))
```

The comment above it says: `TODO: re-write this when proper hygene is supported.`

The same problem blocks the `=>` form of `cond`, which is also marked with a
TODO pending hygiene.

Beyond capture, the macro system has a broader source-information problem:
expanded nodes carry no location data. Every synthetic node produced by
`Macros.macroExpand` has `TextLocation.Missing`, so binder errors on
macro-expanded code produce messages with no file/line, debug sequence points
inside expanded code step to the wrong location, and closure/method names
generated from macro-introduced `let` bindings are meaningless in the IL.

---

## The Marks Algorithm (KFFD)

The algorithm is due to Kohlbecker, Friedman, Felleisen, and Duba (1986), as
refined by Dybvig's "Syntactic Abstraction in Scheme". The core idea is simple:

A **mark** is a unique opaque integer generated once per macro _application_.

1. When a macro is applied, stamp a fresh mark `M` onto all syntax from the
   _call site_ (the entire input form, including all arguments).
2. During template expansion, stamp the same mark `M` onto all _introduced_
   syntax — everything that comes from `MacroTemplate.Quoted` nodes, i.e. the
   macro's own template text.  Do not re-stamp pattern variable substitutions.
3. Two symbol occurrences refer to the same binding if and only if they have the
   same name _and_ the same mark set. Marks cancel pairwise (XOR/toggle
   semantics): a symbol that gains mark `M` twice ends up with the same mark set
   as one that never gained it.

The result is that:
- A template-introduced `tmp` has marks `{M}`, distinct from the user's `tmp`
  which has marks `{}` (or marks from some other expansion `{M'}`).
- A substituted user identifier `test1` gains `M` at call-site stamping; the
  cancellation rule means it does not pick up an extra mark during expansion,
  leaving it transparent and resolving in the call-site scope.

This is sufficient for _syntactic_ hygiene. The harder second half — ensuring
that free identifiers in a macro template (e.g. `cons`, `list`) resolve in the
_definition_ scope of the `define-syntax` rather than the call site — requires
snapshotting the binder's scope at `define-syntax` time. That is left as a
follow-on.

---

## The Core Data Structures

### Why Not Modify the Firethorn Tree?

Feersum's concrete syntax tree is backed by Firethorn's red/green nodes.
Firethorn red nodes are **freshly allocated on every traversal call** — they are
not cached. Reference equality on red nodes is always `false` across traversal
passes, and structural equality (`=`) is only reliable within a single traversal
call because it checks `parent * offset * GreenNode_reference`. This makes red
nodes unsuitable as dictionary keys, particularly after macro expansion produces
new synthetic trees.

Additionally, Firethorn green nodes contain no position information, and the
red tree's `TextRange` is an absolute byte-offset pair into the _source file_ —
there is nothing meaningful there for synthetic factory nodes, which all get
`Offset = 0`.

The design therefore represents all hygiene and source metadata in two _side
tables_ that run parallel to the tree, rather than embedding new fields in the
Firethorn-backed types.

### `DocId` and `NodeKey`

```fsharp
/// An opaque identifier for a "document" — a source file, a macro template
/// parse, or a single macro application's synthetic tree.
[<Struct>] type DocId = DocId of int

module DocId =
    let fresh () : DocId = ...  // atomically incrementing counter

/// A globally unique, traversal-stable key for any syntax node.
[<Struct>]
type NodeKey = { Doc: DocId; Offset: int }

module NodeKey =
    let ofNode (doc: DocId) (expr: Expression) : NodeKey =
        { Doc = doc; Offset = expr.SyntaxRange.Start }
```

`SyntaxRange.Start` is the absolute byte offset already computed by the red
tree. Within any single parsed document, all *sibling* expressions at the same
nestinglevel have distinct start positions. However, a parent `Form` node and
its first child share the same start offset — so `(DocId, offset)` alone is not
quite sufficient as a key for parsed source trees. For parsed source nodes this
is handled by always keying on the *leaf* (symbol, constant) nodes when doing
scope lookups; we never need to look up a `Form` node in `SyntaxContext` by
location. The constraint that matters is: within one `DocId`, no two leaf nodes
that the binder resolves share a start offset.

Synthetic nodes from `Factories.fs` all have `SyntaxRange.Start = 0`. They are
given a dedicated `DocId` for their macro application context, and within that
context `macroExpand` assigns sequential offsets `0, 1, 2, …` to introduced
nodes as it recurses, so they are unique within the application `DocId`.

### The `DocTable`

The `DocTable` is a single registry that maps `DocId` values to richer metadata
about the document they represent.

```fsharp
type DocumentKind =
    /// A source file parsed from disk or a REPL input.
    | SourceFile of TextDocument
    /// The static parse of a macro template body (from `syntax-rules`).
    | MacroTemplate of path: string * definedAt: TextLocation
    /// A single macro application — synthetic nodes created during template
    /// instantiation.  One fresh DocId is assigned per `macroApply` call.
    | MacroApplication of macroName: string * callSiteKey: NodeKey * mark: Mark

type DocTable = Map<DocId, DocumentKind>
```

The `DocTable` serves several purposes that are currently scattered across
different parts of the compiler:

| Problem today | Solution via `DocTable` |
|---|---|
| `BinderCtx.CurrentDocument: TextDocument option` is threaded through the binder to convert `SyntaxRange` to `TextLocation` | Look up `SourceFile(doc)` in `DocTable` to convert on demand |
| Macro expansion trace is entirely lost — errors show `Missing` locations | Follow `MacroApplication.callSiteKey.Doc` chain in `DocTable.describeOrigin` |
| `EmitCtx` keeps a `Dictionary<string, Document>` of debug documents keyed by path string | Iterate `SourceFile` entries in `DocTable` instead |

Location resolution and chain description become straightforward operations on
the table:

```fsharp
module DocTable =
    /// Resolve a NodeKey to a TextLocation for diagnostics / sequence points.
    let tryResolveLocation (table: DocTable) (key: NodeKey) : TextLocation =
        match Map.tryFind key.Doc table with
        | Some(SourceFile doc)         -> TextDocument.rangeToLocation doc { Start = key.Offset; End = key.Offset }
        | Some(MacroTemplate(path, _)) -> (* best-effort from template offset *)
        | _                            -> TextLocation.Missing

    /// Build a human-readable expansion chain for use in diagnostics.
    /// Returns lines like ["expanded from 'or' at bar.scm:11:1"; ...]
    let rec describeOrigin (table: DocTable) (doc: DocId) : string list =
        match Map.tryFind doc table with
        | Some(SourceFile d)                        -> [ d.Path ]
        | Some(MacroTemplate(path, loc))            -> [ sprintf "%s (macro template at %A)" path loc ]
        | Some(MacroApplication(name, callKey, _))  ->
            // Resolve the call-site location so the chain message includes the position.
            let loc = tryResolveLocation table callKey
            sprintf "expanded from macro '%s' at %A" name loc
            :: describeOrigin table callKey.Doc
        | None -> []
```

### `MacroBindings` — change required

`macroMatch` currently produces `MacroBinding = (string * Expression)`. For
the marks system, pattern variable substitutions must carry their `NodeKey` so
that the expander can look up and update their marks in the `SyntaxContext`.
Change the type to:

```fsharp
type MacroBinding = string * NodeKey   // was: string * Expression
```

The matched `Expression` can still be retrieved via `DocTable` / `SyntaxContext`
when the substitution is emitted into the expansion output. `macroMatch` must
therefore also receive the current `SyntaxContext` so it can record `NodeKey`s
for matched sub-expressions.

### `SyntaxContext`

The `SyntaxContext` holds the hygiene marks and debug naming metadata for each
node, keyed by `NodeKey`.

```fsharp
type SyntaxInfo =
    { /// Active hygiene marks — XOR/toggle semantics per the marks algorithm.
      Marks: Set<Mark>
      /// For macro-introduced symbols: the gensym name used in scope lookups.
      IntroducedAs: string option
      /// Human-readable source name for debug info and closure/method naming.
      /// E.g. "result" even when IntroducedAs = "|introduced-5-result|".
      DisplayName: string option
      /// The original Expression node (needed to recover syntax after binding
      /// pattern variables in MacroBindings).
      Syntax: Expression }

type SyntaxContext = Map<NodeKey, SyntaxInfo>
```

`TextLocation` is _not_ stored here — it is derived on demand via `DocTable`.
This keeps `SyntaxContext` purely about hygiene and naming.

### `Mark`

```fsharp
[<Struct>] type Mark = Mark of int

module Mark =
    let fresh () : Mark = ...  // atomically incrementing counter
    /// XOR-toggle a mark in or out of a set.
    let toggle (m: Mark) (marks: Set<Mark>) =
        if Set.contains m marks then Set.remove m marks
        else Set.add m marks
```

---

## Changes to `DocId` Assignment Points

A fresh `DocId` is created at exactly three sites:

| Site | DocId kind |
|---|---|
| `Parse.readProgram path source` | `SourceFile(TextDocument.fromParts path source)` |
| `Macros.parseSyntaxRules` (each `syntax-rules` parse in `parseBuiltinMacro` and `define-syntax` binding) | `MacroTemplate(path, definitionLocation)` |
| Each call to `macroApply` | `MacroApplication(macroName, callSiteKey, freshMark)` |

The `DocTable` starts empty and is returned alongside the `ParseResult` by the
parser. It is extended at each macro expansion site and threaded through the
rest of the pipeline.

---

## Changes to Macro Expansion

`macroApply` signature becomes:

```fsharp
val macroApply :
    macro: Macro ->
    syntax: Expression ->
    callSiteKey: NodeKey ->
    ctx: SyntaxContext ->
    docs: DocTable ->
    Result<Expression * SyntaxContext * DocTable, Diagnostic>
```

Per application:

1. `mark = Mark.fresh()`, `appDocId = DocId.fresh()`
2. Register `MacroApplication(macro.Name, callSiteKey, mark)` in `docs`.
3. **Stamp call-site input**: for every node in the input subtree, XOR `mark`
   into its `Marks` in `ctx`. This produces a *new* `SyntaxContext`; the old
   one must be preserved so that backtracking across transformer arms is
   possible (see Open Question 1).
4. **Expand template**: recurse through `MacroTemplate`, threading a sequential
   offset counter `n` (starting at 0) for synthetic nodes within `appDocId`:
   - `Quoted q` (introduced): create fresh synthetic node via `Factories.*`,
     assign `NodeKey { Doc = appDocId; Offset = n++ }`,
     populate `SyntaxInfo` with `Marks = {mark}`, `IntroducedAs = Some gensym`,
     `DisplayName = Some originalTemplateName`, `Syntax = syntheticNode`.
   - `Subst v` (substituted): look up `v` in `MacroBindings` to get its
     `NodeKey`; XOR `mark` into its `Marks` in `ctx` (this cancels the
     call-site stamp applied in step 3, making the substituted identifier
     transparent and resolvable at the call-site scope).
5. Return the expanded `Expression` with the updated `ctx` and `docs`.

The UUID in `macroOr` becomes just `result`. The macro template for `or`
becomes:

```scheme
(syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
        (let ((result test1))
            (if result result (or test2 ...)))))
```

Hygiene marks make accidental capture impossible.

---

## Changes to the Binder

`BinderCtx` drops `CurrentDocument: TextDocument option` and gains:

```fsharp
type private BinderCtx =
    { ...
      SyntaxCtx:  SyntaxContext
      Docs:       DocTable
      (* CurrentDocument removed *) }
```

**`tryFindBinding`**: `ScopeEntry` keys become `(string * Set<Mark>)` instead of
bare `string`. Two symbols refer to the same binding iff their name after mark
resolution matches and their mark sets agree.

**Error messages**: replace bare `getNodeLocation ctx node` with
`DocTable.tryResolveLocation ctx.Docs (NodeKey.ofNode doc expr)`, and append the
expansion chain from `DocTable.describeOrigin`. A missing-symbol error becomes:

```
undefined identifier 'foo' at bar.scm:12:3 (expanded from macro 'or' at bar.scm:11:1)
```

instead of:

```
undefined identifier 'foo'
```

**`bindWithSequencePoint`**: look up location in `DocTable` first; fall back to
the existing `SyntaxRange`-based computation for non-expanded nodes. This
restores correct debugger step positions inside macro-expanded code.

---

## Changes to Closure Lowering and Code Generation

By the time `Lower.fs` sees `BoundExpr`, identifiers have become
`StorageRef.Local n` (integer slot indices) — the original source symbol is
gone. To recover `DisplayName` for closure naming, the binder must record the
mapping at binding time:

```fsharp
// Added to BinderCtx:
mutable LocalNames: Map<int, string>  // Local slot → display name
```

When `addBinding` allocates a `Local(n)`, it reads `SyntaxInfo.DisplayName`
from `SyntaxContext` for the binding symbol's `NodeKey` and stores it in
`LocalNames`. `Lower.fs` receives this map (or it is embedded in `BoundFormals`)
so it can name converted closures and lambda methods.

This is surfaced to `Compiler.fs` via an optional display name field added to
`BoundFormals` or the `Lambda` `BoundExpr` variant, enabling meaningful names
in the generated IL (`<result>` instead of `<>4__this`).

`Compiler.fs` replaces the `Dictionary<string, Document>` that caches CIL
debug document objects with a lookup over `DocTable` `SourceFile` entries.

---

## Affected Files

| File | Change |
|---|---|
| New `Syntax/DocId.fs` | `DocId`, `NodeKey` types and counters |
| New `Syntax/DocTable.fs` | `DocumentKind`, `DocTable`, location resolution, chain description |
| New `Syntax/SyntaxContext.fs` | `Mark`, `SyntaxInfo`, `SyntaxContext` (keyed by `NodeKey`) |
| `Syntax/Parse.fs` | Assign fresh `DocId` per parse; return `(ParseResult * DocTable * SyntaxContext)` |
| `Syntax/Factories.fs` | Counters for synthetic offsets per expansion context |
| `Binding/Macros.fs` | `macroApply`/`macroExpand` take and return `SyntaxContext` + `DocTable`; implement mark stamping; remove all `TextLocation.Missing` synthetic forms |
| `Binding/Binder.fs` | `BinderCtx` drops `CurrentDocument`; gains `SyntaxCtx` + `Docs`; `tryFindBinding` is mark-aware; errors use `DocTable.describeOrigin` |
| `Binding/Lower.fs` | Thread `SyntaxContext`; extract `DisplayName` for closure naming |
| `Compile/Compiler.fs` | CIL debug documents from `DocTable`; `DisplayName` for method names |
| `Compile/Builtins.fs` | Replace UUID in `macroOr` with `result`; remove `or`/`cond` TODO comments |
| `test/Feersum.Tests/MacroTests.fs` | Add capture-avoidance tests; add expansion-chain diagnostic tests |

---

## Open Questions

1. **`SyntaxContext` copy-on-branch for backtracking**: `macroTryApply` tries
   each transformer arm in order. If arm 1 stamps marks into `ctx` and then
   fails to match, arm 2 must start from the *original* unstamped `ctx`, not
   the partially-modified one — this is a correctness issue, not just
   performance. The current `macroMatch` returns `Result<_, unit>` before any
   stamping occurs, so stamping can (and should) only happen *after* a
   successful match. This ordering must be preserved in the implementation.
   `Map` is cheap to "restore" (just keep the pre-stamp value), but care is
   needed to not stamp before match confirmation.
   
   Separately, `Map` has `O(log n)` updates. A `Dictionary` with snapshot
   semantics could improve performance in macro-heavy code — profile before
   deciding.

2. **Synthetic offset counter threading**: the design uses a sequential counter
   `n` threaded through the `macroExpand` recursion. This must be passed
   explicitly (not a mutable thread-local) so that the expansion of each
   `Repeated` element gets a distinct block of offsets. Decide whether to
   return the updated counter from each recursive call, or pre-count the
   template's synthetic nodes before expansion begins.

3. **`DocTable` in `EmitCtx`**: the CIL emit phase currently constructs a
   `Document` per unique `TextLocation.Source` string. Replacing that with a
   `DocTable` lookup requires threading `DocTable` into `EmitCtx`. Consider
   whether this translation is better done at the binder→lower boundary instead,
   producing a dedicated `DebugDocuments` map that the emitter already
   understands.

4. **Definition-scope for free template identifiers**: the harder half of
   hygiene. An introduced `cons` in a macro template should resolve in the
   _definition_ scope of `define-syntax`, not the call site. Requires
   snapshotting `BinderCtx.Scope` at `define-syntax` time and threading it into
   `macroApply`. The `MacroApplication` `DocTable` entry is a natural carrier
   for this snapshot. Left as a follow-on once the marks mechanism is in place.

5. **`DottedForm` in `MacroTemplate`**: currently `unimpl "Dotted forms in macro
   expansion are not yet supported"`. Implement in the same work item as marks,
   since the stamping logic for `DottedForm` is identical to `Form`.
