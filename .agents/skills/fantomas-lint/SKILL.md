---
name: fantomas-lint
description: "Ensure F# source code is formatted cleanly with Fantomas before finishing a task. Use when writing or editing any F# source file, before committing changes, or when CI lint failures need to be fixed. Run Fantomas to reformat, then verify tests still pass."
---

# Fantomas Lint Check

CI runs Fantomas to enforce consistent F# formatting. Any diff that introduces
formatting violations will fail the lint step, even if all tests pass.

## Steps

Always perform these steps after editing F# source files, before considering a
task complete:

### 1. Reformat with Fantomas

Run the repo reformat script from the workspace root:

```bash
bash reformat.sh
```

This restores the .NET local tools, runs `fantomas` on the whole workspace, and
runs `dotnet format`. It is safe to re-run; it is idempotent when the code is
already clean.

Alternatively, to just check without modifying files (matches the CI check):

```bash
dotnet tool restore
dotnet tool run fantomas --check .
```

If `--check` reports violations, run `bash reformat.sh` to fix them.

### 2. Verify tests still pass

```bash
dotnet test
```

Both steps must succeed before the task is done.

## Notes

- Fantomas is a .NET local tool declared in `.config/dotnet-tools.json`; always
  run `dotnet tool restore` before invoking it directly.
- The VS Code task **reformat** (defined in `.vscode/tasks.json`) runs
  `bash reformat.sh` and can be used instead of the terminal command.
- Do not skip the reformat step even for small one-line edits — Fantomas enforces
  indentation, blank lines, and spacing rules that are easy to violate by hand.
