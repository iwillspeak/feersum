# Syntactic Closures Expansion and Binding

This document records the consolidated macro and binding design that Feersum now
implements. It supersedes the earlier split documents that separately described
work-stack expansion and binder-context restructuring.

## Background

Feersum originally expanded macros by transcribing CST nodes and re-binding them
in the caller environment. That model was not hygienic for macro-introduced
identifiers and required round-tripping through temporary syntax trees.

The current implementation uses syntactic closures and a fused expansion +
binding pass over `Stx`, implemented in `Binding/Stx.fs` and `Binding/Binder.fs`.
This keeps hygiene and storage resolution in one traversal.

## Design

### Final architecture

1. Parse produces CST `Expression` nodes.
2. `Stx.ofExpr` converts CST into `Stx`:
   - `Stx.Id`, `Stx.Datum`, `Stx.List`, `Stx.Vec`, `Stx.Error`
   - `Stx.Closure(inner, env)` for explicit environment override during macro expansion.
3. Binding walks `Stx` and simultaneously:
   - resolves syntax names through `StxEnvironment`,
   - resolves variable identities through frame resolution scopes,
   - emits `BoundExpr`.

### Hygiene model

- `StxEnvironment = Map<string, StxBinding>` maps names to:
  - `Special` built-ins,
  - `Macro` transformer ids,
  - `Variable` binding ids.
- `Ident.fresh()` creates globally unique binding ids.
- Resolution is two-phase:
  1. Name lookup in `StxEnvironment` to get binding identity/kind.
  2. `FrameCtx.resolve` maps `Ident` to `StorageRef`, traversing parent frames and registering captures for lambda frames.
- `Stx.Closure` ensures macro-transcribed fragments resolve in the captured environment, not always the ambient caller environment.

### Binding context and scope

- `BinderCtx` holds compilation-wide mutable state:
  - diagnostics,
  - library signatures,
  - macro registry.
- `FrameCtx` represents one binding frame (`Global`, `Library`, `Lambda`) with:
  - `ResolutionEnv` stack (`Map<Ident, StorageRef>` scopes),
  - local counter,
  - lambda capture state.
- Lexical forms (`let`, `let*`, `letrec`, `letrec*`) push/pop resolution scopes.
- Top-level/library definitions mint globals in root scope, otherwise locals.

### Macro forms

- `define-syntax` reserves macro ids before parsing transformers.
- `let-syntax` and `letrec-syntax` pre-seed macro names, with `letrec-syntax`
  using the recursive binding environment for transformer creation.
- Macro transformers are stored in `BinderCtx.Macros` and invoked from
  `bindForm` when a head resolves to `StxBinding.Macro`.

### Alternatives considered (from earlier drafts)

1. **Explicit work-stack expander**  
   Rejected for the mainline implementation because continuation management and
   result assembly were more complex than needed for Feersum’s current shape.

2. **Larger context split (`CompilationUnit` + standalone expander module)**  
   Partially adopted conceptually (frame kinds + explicit resolution scopes), but
   not as a separate `Expand.fs` architecture. The implemented design keeps
   fused expansion/binding in `Binder.fs` while still separating syntax
   environment and resolution environment concerns.

## Affected Files

| File | Purpose |
|------|---------|
| `src/Feersum.CompilerServices/Binding/Stx.fs` | `Stx` DU, closure peeling, CST-to-Stx conversion, syntax binding definitions. |
| `src/Feersum.CompilerServices/Binding/Binder.fs` | Fused macro expansion and binding, frame resolution/capture logic, special form handling, macro registration and invocation. |
| `src/Feersum.CompilerServices/Binding/Macros.fs` | Syntax-rules transformer construction used by binder macro forms. |

## Open Questions

1. Should define-introduction behavior under closure-wrapped names move from the
   current compatibility behavior to stricter syntax-set semantics?
2. Should the `letrec` uninitialized-variable check become a depth-aware dataflow
   analysis to reduce conservative false positives?
