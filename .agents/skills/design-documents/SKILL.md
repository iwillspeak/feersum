---
name: design-documents
description: Write a new compiler design document in docs/design-docs/. Use when the user asks to document a design, write a design doc, or record architectural decisions for the Feersum compiler.
---

# Writing a Design Document

Design documents live in `docs/design-docs/`. They are rendered by a static
site generator that **sluggifies** each filename: the numeric prefix and the
`.md` extension are stripped and the remainder becomes a folder path. For
example:

```
docs/design-docs/02-hygienic-macros.md  →  /hygienic-macros/
docs/design-docs/01-syntax-trees.md     →  /syntax-trees/
```

## Steps

### 1. Choose a filename

Use a two-digit zero-padded numeric prefix to control ordering, followed by
lowercase hyphen-separated words that describe the topic:

```
docs/design-docs/03-my-new-topic.md
```

Do not use underscores or camelCase in filenames.

### 2. Write the document

A design document should contain these sections, in order:

1. **Title** (`#` heading) — matches the slug in human-readable form.
2. **Background / Problem** — why this design is needed; what the current
   state of the code is and why it is insufficient.
3. **Design** — the proposed solution, with type signatures, data-flow
   diagrams, or F# pseudocode as appropriate.
4. **Affected Files** — a Markdown table listing files and the nature of each
   change.
5. **Open Questions** — numbered list of decisions not yet made, with a note
   on the preferred direction where one exists.

Sections 4 and 5 can be omitted for short or early-stage documents, but a
Background and Design section are always expected.

Write in present tense for the current state (`The binder does X`) and
future/imperative for proposed changes (`Change Y to Z`).

### 3. Add a link in the README index

`docs/design-docs/README.md` is the index page. Add a bullet using the
**sluggified folder form** of the link, not the raw `.md` filename:

```markdown
 * [My New Topic](./my-new-topic/)
```

### 4. Linking between design docs

Always use the sluggified folder form when linking between documents:

```markdown
<!-- Correct -->
[Syntax Trees](./syntax-trees/)

<!-- Wrong — does not work on the published site -->
[Syntax Trees](./01-syntax-trees.md)
```

## Notes

- Documents are not guaranteed to be kept up to date after implementation.
  Record significant deviations from the design in code comments rather than
  retroactively editing the document.
- The README disclaimer (`The documents aren't guaranteed to be up to date,
  accurate, or even useful.`) is intentional — leave it in place.
