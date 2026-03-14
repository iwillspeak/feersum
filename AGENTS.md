# Feersum — Agent Guide

Feersum is a Scheme compiler for .NET written in F#. It targets .NET 8 and is
distributed as a .NET tool and MSBuild SDK. The compiler follows a classic
multi-pass pipeline: **parse → bind → lower → emit**.

## Repository Layout

| Path | Contents |
|------|----------|
| `src/Feersum/` | Compiler executable (F#) |
| `src/Feersum.CompilerServices/` | Core compiler passes — syntax, binding, lowering, emit |
| `src/Feersum.Core/` | Scheme core libraries (`.scmproj`), e.g. `base.sld`, `lists.sld` |
| `src/Serehfa/` | Runtime support and builtin implementations (C# + Scheme) |
| `src/Feersum.Sdk/` | MSBuild SDK targets for `.scmproj` files |
| `src/Feersum.Templates/` | `dotnet new` templates |
| `spec/` | Snapshot-tested Scheme source files (`.scm`) and expected ASTs (`.ast`) |
| `test/` | .NET test projects, including `SpecTests.fs` which drives the spec suite |
| `docs/` | Published documentation and design documents |
| `examples/` | Example Scheme projects using the SDK |

## Compiler Pipeline

1. **Parse** (`src/Feersum.CompilerServices/Syntax/Parse.fs`, AST wrappers in `src/Feersum.CompilerServices/Syntax/Tree.fs`) — text → `AstNode` tree with position info; errors
   produce stub nodes rather than hard failures.
2. **Bind** (`Binding/Binder.fs`) — `AstNode` → typed bound tree; resolves
   variables to `StorageRef`s, recognises special forms, identifies captures.
3. **Lower** (`Binding/Lower.fs`) — rewrites captured-variable references to
   environment slots; hoists captured values; simplifies tree structure.
4. **Emit** (`Compile/Compiler.fs`) — walks the bound tree and writes CIL
   bytecode via `Mono.Cecil`; globals become `public static` fields, locals
   stay as IL locals, captures use `object[]` environment slots.

## Key Workflows

```bash
# Build
dotnet build

# Run tests
dotnet test

# Update spec snapshots after intentional output changes
UpdateSnapshots=true dotnet test

# Compile a single Scheme file
dotnet run --project src/Feersum -- yourfile.scm
```

The VS Code tasks `build`, `test`, and `update-snapshots` wrap the commands
above.

### Adding / changing spec tests

Spec files live in `spec/`. Each `.scm` file has a paired `.ast` snapshot. Run
`dotnet test` with `UpdateSnapshots=true` to regenerate snapshots after an
intentional change.

## Further Reading

- [DEVGUIDE.md](DEVGUIDE.md) — detailed compiler structure walk-through
- [CONTRIBUTING.md](CONTRIBUTING.md) — contribution guidelines
- [docs/design-docs/](docs/design-docs/) — architectural design documents

---

## Agent Skills

````skill
path: .agents/skills/design-documents/SKILL.md
name: design-documents
description: Write a new compiler design document in docs/design-docs/. Use when the user asks to document a design, write a design doc, or record architectural decisions for the Feersum compiler.
````
