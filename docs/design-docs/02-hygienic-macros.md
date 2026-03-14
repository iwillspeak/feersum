# Hygienic Macro Expansion

This document describes the design for introducing hygienic macro expansion to
Feersum, and a related set of improvements to source location tracking and
symbol provenance throughout the compiler pipeline.

The approach is based on the _Syntactic Closures_ system described by Bawden
and Rees in their [1988 paper][sc-paper]. A reference implementation of the
core algorithm for a toy language is available at
[iwillspeak/Mutton][mutton-repo].

[sc-paper]: https://dspace.mit.edu/handle/1721.1/6036
[mutton-repo]: https://github.com/iwillspeak/Mutton

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

## Syntactic Closures

Runtime closures allow a lambda body to remember the _execution environment_ in
which it was created, so that free variables resolve correctly regardless of
where the closure is later called. Syntactic closures apply the same idea to the
_syntax environment_: a piece of macro-introduced syntax remembers the
environment in which the macro was **defined**, preventing names from
accidentally resolving in the **call-site** scope.

The approach avoids the mark-set bookkeeping of the KFFD algorithm
(Kohlbecker _et al._ 1986) and directly addresses the "definition-scope"
problem for free template identifiers — something that KFFD marks alone leave as
a separate concern.

Three concepts underpin the design:

1. **Identifiers with stamps.** Every name in the program is paired with an
   opaque integer _stamp_. The parser assigns stamp `0` to all names. Each
   binding site (e.g. `lambda`, `let`) renames its parameter to a fresh stamp,
   and extends the syntax environment so that references within the body resolve
   to the renamed identifier. Two identifiers refer to the same binding if and
   only if they have the same `(name, stamp)` pair.

2. **Syntax environments.** A `StxEnv` is a `Map<string, StxBinding>` that
   records the syntactic meaning of each name at a given point in the program.
   A name may be bound to a variable identifier, a special-form marker (e.g.
   `Lam`), or a macro transformer. When the expander encounters a name, it
   resolves it via the current `StxEnv`.

3. **Syntactic closures.** When a macro transformer is _defined_ the current
   `StxEnv` is captured alongside the template. When the macro is later
   _applied_, the template syntax is wrapped in a `Closure(stx, defEnv)` node.
   When `expand` encounters a closure it expands the inner syntax using the
   _captured_ environment rather than the call-site environment. The two
   environments never mix, and the same identifier `x` introduced by the
   template can only ever resolve to what `x` meant where the macro was written.

Returning to the motivating example: when `(or 100 200)` is expanded the
template `(let (temp 100) (if temp temp (or 200)))` is wrapped in a closure over
the definition-time environment. When that closure is later expanded, the `x` in
the template resolves to a freshly stamped identifier created during `expandLam`
— one that is entirely distinct from any `x` the caller might have in scope. No
GUIDs required.

## The Core Data Structures

There are two main data structures involved in this new approach to macro expansion: the `Stx` tree, and the `StxEnv`. The tree serves as a standardised input and output format from expansion. The environment is flowed down the stack and allows identifiers to be resolved to their matching syntax items.

These two come together at the `Stx.Closure` variant. In this a node in the `Stx` tree is substituted for another with a captured environment. This happens at two places: when a macro invocation replaces a form with fabricated syntax, and where a substitution is made into the template containing syntax from the call site.

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
there is nothing meaningful there for synthetic factory nodes, which all start
at `Offset = 0` but have no corresponding text document.

The design therefore introduces a new _syntax tree_ (`Stx`) that sits between
the Firethorn-backed CST and the binder. This tree carries identifier stamps,
syntax environments, and closures directly in its structure, rather than
relying on side tables keyed by node identity.

### Position and Provenance

Within the current Firethorn `Expression` tree each node has rich position information. All items in the tree, both nodes and tokens, have a `TextRange`. A pair of byte-offsets into their corresponding `TextDocument`. For binding we need access to this information to allow rich diagnostics. Each node in the `Stx` tree will have a `TextRange`. For nodes with an underlying syntax representation this will be lifted directly from the `Expression` tree. For syntax closures this will defer to the inner syntax.

In addition to knowing the range of a node we also need to know the document that a given node should be associated with. This is referred to as provenance. To avoid bloating the tree we rely on the fact that syntax provenance can only change at two locations:

 * From the outer document to the macro _template_ document at the call site
 * From the macro template to the outer document at a substitution site
 
Both of these locations are covered by a syntax closure. Where the main nodes carry position information the closure will carry provenance information. Each closure will have either `MacroAp of string * DocId` or `MacroSubst of string DocId`. This allows us to construct a chain of `Provenance` which represents the current expansion stack. The innermost document can be used for debug and diagnostic information, and the full stack allows for detailed "additional context" to compiler messages.

### The `Stx` Tree

The `Stx` tree is produced by _illuminating_ the Firethorn `Expression` tree.
It mirrors the structure of the existing `SyntaxNode` / `NodeKind` types in
`Binding/BinderNode.fs`, but adds first-class support for identifiers with
stamps and syntactic closures:

```fsharp
/// An identifier is a name paired with an opaque stamp. Two identifiers
/// refer to the same binding iff (Name, Stamp) are equal.
type Ident = { Name: string; Stamp: int }

module Ident =
    let private counter = ref 0
    let fresh (name: string) : Ident =
        let s = System.Threading.Interlocked.Increment(counter)
        { Name = name; Stamp = s }

type Stx =
    | Literal of SyntaxConstant * 
    | Ident of Ident
    /// A compound form — an application, special form, or macro call.
    /// Contains the main body and an optional dotted tail.
    | Form of Stx list * Stx option
    | Closure of Stx * StxEnv

and StxEnv = Map<string, StxBinding>

and StxBinding =
    | Var of Ident
    | SpecialForm of SpecialFormKind
    | Macro of Transformer

and SpecialFormKind = Lam | Let | If | Define | DefSyn | Quote (* … *)

and Transformer = Stx -> StxEnv -> Stx
```

The `illum` function walks the `Expression` tree and produces the
initial `Stx`, assigning stamp `0` to every symbol:

```fsharp
let rec illum (expr: Expression) : Stx =
    match expr with
    | SymbolNode s ->
        Stx.Ident { Name = s.CookedValue; Stamp = 0 }
	// .....
```

### Syntax Environment Helpers

Two helper functions form the core of the environment machinery:

```fsharp
/// Wrap a piece of syntax in a syntactic closure, capturing `defEnv`.
let close (stx: Stx) (defEnv: StxEnv) : Stx =
    Stx.Closure(stx, defEnv)

/// Resolve an identifier in the syntax environment.  If no binding
/// exists, special-form names get their built-in meaning; everything
/// else is treated as a free variable.
let resolve (id: Ident) (stxEnv: StxEnv) : StxBinding =
    match Map.tryFind id.Name stxEnv with
    | Some binding -> binding
    | None ->
        match id.Name with
        | "lambda" -> SpecialForm Lam
        | "let"    -> SpecialForm Let
        | "if"     -> SpecialForm If
        | "define" -> SpecialForm Define
        | "define-syntax" -> SpecialForm DefSyn
        | "quote"  -> SpecialForm Quote
        | _ -> Var id
```

### Renaming at Binding Sites

When expanding a binding form such as `lambda` or `let`, the parameter is
renamed with a fresh stamp. The syntax environment is extended so that all
references to the same _name_ within the body resolve to the renamed identifier:

```fsharp
let rename (stxEnv: StxEnv) (id: Ident) : StxEnv * Ident =
    let fresh = Ident.fresh id.Name
    let env' = Map.add id.Name (Var fresh) stxEnv
    (env', fresh)
```

This is what prevents accidental capture at binding sites. Thinking back to the
nested lambda example `((lambda x (+ x ((lambda x (+ x x)) x))) 10)`, the two
`x` binders end up as `x.1` and `x.2` rather than competing for the same name.

### The `DocTable`

The `DocTable` is a single registry that maps `DocId` values to richer metadata
about the document they represent.

```fsharp
type SourceFile =
	| Source of string * TextDocument

type DocTable = Map<DocId, SourceFile>
```

The `DocTable` serves several purposes that are currently scattered across
different parts of the compiler:

| Problem today | Solution via `DocTable` |
|---|---|
| `BinderCtx.CurrentDocument: TextDocument option` is threaded through the binder to convert `SyntaxRange` to `TextLocation` | Look up `SourceFile(doc)` in `DocTable` to convert on demand |
| Macro expansion trace is entirely lost — errors show `Missing` locations | Follow `MacroApplication.callSiteKey.Doc` chain in `Provenance.describeOrigin` |
| `EmitCtx` keeps a `Dictionary<string, Document>` of debug documents keyed by path string | Iterate `SourceFile` entries in `DocTable` instead |

Location resolution and chain description become straightforward operations on
the table:

```fsharp
module DocTable =
    /// Resolve a NodeKey to a TextLocation for diagnostics / sequence points.
    let tryResolveLocation (table: DocTable) (key: NodeKey) : TextLocation =
        match Map.tryFind key.Doc table with
        | Some(SourceFile.Source doc)         -> TextDocument.rangeToLocation doc { Start = key.Offset; End = key.Offset }
        | _                            -> TextLocation.Missing
```

## Changes to Macro Expansion

### The Expand Loop

The `expand` function takes a `Stx` node and a `StxEnv` and produces an
expanded `Stx` and an updated environment. Non-empty forms dispatch on the
resolved head identifier:

- **Macro**: invoke the transformer, which returns already-closed syntax.
- **Lam / Let**: rename binders with fresh stamps before expanding the body.
- **DefSyn**: parse rules, build a transformer capturing the current `StxEnv`,
  and add the macro binding to the environment. Returns `None` for the syntax
  (the definition has no runtime representation).
- **Anything else**: recurse into sub-forms.

```fsharp
let rec expand (stx: Stx) (stxEnv: StxEnv) : Stx option * StxEnv =
    match stx with
    | Stx.Literal _ -> (Some stx, stxEnv)
    | Stx.Ident id ->
        match resolve id stxEnv with
        | Var resolved -> (Some(Stx.Ident resolved), stxEnv)
        | _ -> failwith "syntax item in value position"
    | Stx.Form([], _) ->
        (Some stx, stxEnv)  // Empty form
    | Stx.Form(head :: args, dotted) ->
        match head with
        | Stx.Ident id ->
            match resolve id stxEnv with
            | Macro transformer ->
                let expanded = transformer stx stxEnv
                (Some expanded, stxEnv)
            | SpecialForm Lam ->
                (Some(expandLam head args dotted stxEnv), stxEnv)
            | SpecialForm DefSyn ->
                expandDefSyn args dotted stxEnv
            | _ ->
                let expandedArgs = List.map (expandOne stxEnv) (head :: args)
                let expandedDotted = Option.map (expandOne stxEnv) dotted
                (Some(Stx.Form(expandedArgs, expandedDotted)), stxEnv)
        | _ ->
            let expandedArgs = List.map (expandOne stxEnv) (head :: args)
            let expandedDotted = Option.map (expandOne stxEnv) dotted
            (Some(Stx.Form(expandedArgs, expandedDotted)), stxEnv)
    | Stx.Closure(inner, env) ->
        let (result, _) = expand inner env
        (result, stxEnv)
```

The final case — `Stx.Closure` — is the heart of the hygiene mechanism. When we
hit a syntactic closure we expand its inner syntax using the _captured_
environment, then return the result into the _outer_ environment unchanged.

### Building the Transformer

`makeSynTransformer` captures the definition-time environment. When later
invoked, it matches against the call-site arguments in `useEnv`, substitutes
pattern variables into the template, and wraps the result in a
`Stx.Closure(substituted, defEnv)`:

```fsharp
let makeSynTransformer
    (rules: MacroRule list) (defEnv: StxEnv)
    (macroName: string) : Transformer =
  fun (callStx: Stx) (useEnv: StxEnv) ->
    match callStx with
    | Stx.Form(head :: callArgs, dotted) ->
      let tryRule (rule: MacroRule) =
        match matchPatternArgs rule callArgs dotted useEnv with
        | Some bindings ->
          let substituted = applyTemplate rule.Template bindings
          Some(Stx.Closure(substituted, defEnv))
        | None -> None
      match List.tryPick tryRule rules with
      | Some closed ->
        match expand closed useEnv with
        | Some result, _ -> result
        | None, _ -> failwith $"def-syn in template of '{macroName}'"
      | None -> failwith $"No matching rule for macro '{macroName}'"
    | _ -> failwith $"Invalid call form for '{macroName}'"
```

The key detail: `defEnv` is captured when the transformer is _created_ and
becomes the environment in the `Stx.Closure`. `useEnv` is only used for
pattern matching the call-site arguments. The two environments are kept
strictly separate by the closure mechanism.

### The `or` Template

With syntactic closures the UUID workaround is no longer needed. The macro
template for `or` becomes:

```scheme
(syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
        (let ((result test1))
            (if result result (or test2 ...)))))
```

The template-introduced `result` will be expanded inside a closure that carries
the definition-time environment. When the `let` binding is expanded,
`expandLam` renames `result` with a fresh stamp, making it entirely distinct
from any `result` in the caller's scope.

---

## Changes to the Binder

`BinderCtx` drops `CurrentDocument: TextDocument option` and gains:

```fsharp
type private BinderCtx =
    { ...
      Docs: DocTable
      (* CurrentDocument removed *) }
```

Because identifiers in the `Stx` tree already carry stamps, the binder no
longer needs mark-set–aware scope lookups. Two identifiers refer to the same
binding if and only if they have the same `(Name, Stamp)` pair. The existing
`tryFindBinding` can continue to use string keys, but the key is now the
stamped identifier string (e.g. `"x.3"`) rather than the bare name.

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

When `addBinding` allocates a `Local(n)`, it reads the `Ident.Name` from the
`Stx` tree for the binding symbol and stores it in `LocalNames`. `Lower.fs`
receives this map (or it is embedded in `BoundFormals`) so it can name converted
closures and lambda methods.

This is surfaced to `Compiler.fs` via an optional display name field added to
`BoundFormals` or the `Lambda` `BoundExpr` variant, enabling meaningful names
in the generated IL (`<result>` instead of `<>4__this`).

`Compiler.fs` replaces the `Dictionary<string, Document>` that caches CIL
debug document objects with a lookup over `DocTable` `SourceFile` entries.

---

## Affected Files

| File | Change |
|---|---|
| New `Syntax/Stx.fs` | `Ident`, `Stx`, `StxEnv`, `StxBinding` types; `illum`, `close`, `resolve`, `rename` |
| New `Syntax/DocId.fs` | `DocId`, `NodeKey` types and counters |
| New `Syntax/DocTable.fs` | `DocumentKind`, `DocTable`, location resolution, chain description |
| `Syntax/Parse.fs` | Assign fresh `DocId` per parse; return `(ParseResult * DocTable)` |
| `Syntax/Factories.fs` | Counters for synthetic offsets per expansion context |
| `Binding/Macros.fs` | `macroApply`/`macroExpand` operate on `Stx` tree; build transformers with `makeSynTransformer`; wrap templates in `Stx.Closure` |
| `Binding/Binder.fs` | `BinderCtx` drops `CurrentDocument`; gains `Docs`; binding lookups use stamped identifier keys; errors use `DocTable.describeOrigin` |
| `Binding/Lower.fs` | Extract `Ident.Name` for closure display naming |
| `Compile/Compiler.fs` | CIL debug documents from `DocTable`; `DisplayName` for method names |
| `Compile/Builtins.fs` | Replace UUID in `macroOr` with `result`; remove `or`/`cond` TODO comments |
| `test/Feersum.Tests/MacroTests.fs` | Add capture-avoidance tests; add expansion-chain diagnostic tests |

---

## Open Questions

1. ~~**Illumination boundary**: the `illum` function converts the Firethorn
   `Expression` tree into a `Stx` tree. Decide whether illumination happens
   eagerly for the entire program up front, or lazily as each form is
   encountered during expansion. Eager illumination is simpler; lazy
   illumination avoids creating `Stx` nodes for code that is never reached
   (e.g. dead branches in `cond`).~~ Eagerly

3. ~~**`DocTable` in `EmitCtx`**: the CIL emit phase currently constructs a
   `Document` per unique `TextLocation.Source` string. Replacing that with a
   `DocTable` lookup requires threading `DocTable` into `EmitCtx`. Consider
   whether this translation is better done at the binder→lower boundary instead,
   producing a dedicated `DebugDocuments` map that the emitter already
   understands.~~ Having the Binder produce debug documents sounds like a good idea.

4. ~~**Interaction with `syntax-rules` ellipsis**: the current `Repeated` template
   node interacts with pattern matching and substitution. Ensure that repeated
   elements are expanded within a closure that carries the correct
   definition-time environment, and that each expansion iteration receives
   correctly-stamped binder names.~~ This _should_ all flow out of the macro match. Hopefully the arguments we bind to the parameters are modelled correctly still as `Stx` rather than `Expression`. Source provenance for _all_ substitutions should actually be the same, just the template parameter name that varies.
