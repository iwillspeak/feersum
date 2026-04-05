---
name: ascii-header-formatting
description: "Enforce ASCII-only section headers and comment banners. Use when writing or reviewing code comments, section dividers, or banner headers in any source file. Do not use Unicode box-drawing characters."
---

# ASCII Header Formatting

All section headers and comment banners in source files must use only plain
ASCII characters. Unicode box-drawing characters (e.g. `─`, `━`, `│`, `╔`,
`═`) are forbidden.

## Rules

- Use `-` for horizontal rules and divider lines.
- Headers must be padded with trailing `-` characters so that the full line
  reaches column 80 (the standard ruler width).
- The format is `// -- <title> <filler>` where `<filler>` is as many `-`
  characters as needed to make the total line length exactly 80 characters.
- Indented headers (inside a module or `module private`) follow the same rule,
  counting the leading spaces as part of the line length.
- Apply consistently across F#, C#, and Scheme source files in this repo.

## Examples

**Correct** — line is exactly 80 characters wide

```fsharp
// -- Public API ---------------------------------------------------------------
// -- Helpers ------------------------------------------------------------------

    // -- Lambda ---------------------------------------------------------------
    // -- Variable lookup with capture tracking --------------------------------
```

To compute filler length: `80 - len("// -- <title> ")` dashes.

**Incorrect** — too short, or using Unicode box-drawing characters

```fsharp
// -- Public API --------------------
// ─── Public API ───────────────────
// ━━━ Helpers ━━━━━━━━━━━━━━━━━━━━━
```
