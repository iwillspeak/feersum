# Feersum

[![Build Status](https://dev.azure.com/iwillspeak/GitHub/_apis/build/status/iwillspeak.feersum?branchName=main)](https://dev.azure.com/iwillspeak/GitHub/_build/latest?definitionId=4&branchName=main) [![Feersum on fuget.org](https://www.fuget.org/packages/Feersum/badge.svg)](https://www.fuget.org/packages/Feersum)

> Wots in that thare box yoor holdin?

A Scheme compiler for .NET written in F#. Progress
[livestreamed on Twitch][twitch], [catchup on YouTube][yt]

# Contributing

Feersum is in active development. Contributions are welcome. See
[CONTRIBUTING][contributing] for more imformation. For an introduction to
buiding and testing the compiler, as well as a discussion of the high-level
structure check out the [DEVGUIDE][devguide].

# Getting Started using Feersum

Feersum is distributed as a .NET tool, and MSBuild SDK. For quick use with an
interactive REPL and simple compilation the tool can be installed directly:

```bash
$ dotnet tool install -g Feersum
```

Then from the command line `feersum-scheme` should launch a Scheme REPL. Type in
Scheme code to have it compiled and evaluated. To compile a file into a simple
executable run `feersum-scheme yourfile.scm`. This will produce a new file
`yourfile.exe` which can then be run by `dotnet yourfile.exe`.

To create an SDK style scheme project and build it with `dotnet build` first
install the `Feersum.Templates` template pack:

```
dotnet new --install Feersum.Templates::*
```

Then from the command line:

```
dotnet new console --language Scheme
```

Compile and run with `dotnet build` and `dotnet run` as usual!

# Planned Features

This is a rough list of interesting features I'd like to implement in this project,
and is by no means a guarantee or a strict roadmap.

 * [ ] Implement [all special forms](docs/special-forms.md)
 * [ ] Support [all value types](docs/values.md)
 * [ ] Support [all standard procedures](docs/standard-procedures.md)
 * [x] Macros support.
   * [ ] Macro Hygene (#13).
 * [ ] Support for Scheme exceptions and `guard`.
 * [ ] Full numeric tower support.
 * [ ] Interop with other .NET assemblies.
 * [ ] Multi-statement support in the REPL.
 * [ ] Debugging support
   * [x] Bound tree contains sequence points for debuging
   * [ ] Debuggable REPL. Embedded source in PDBs?
   * [ ] Stop at entry, and step in.
 * [x] MSBUILD SDK support so `.scmproj` can be defined.
   * [x] Proper support for different target frameworks
 * [x] Scheme library and import support.
 * [x] Standard library & builtins from a separate assembly.
 * [x] Quoted expressions.
 * [x] Diagnostics with position
    * [x] Expose position information in parser.
    * [x] Handle multiple parser errors.
    * [x] Turn errors in `bind` into diagnotics too.
 * [x] Perform on-demand compilation & emit to in-memory assembly for REPL.

Future themes of development are also tracked with
[`Roadmap` issues][roadmap_issues] on GitHub.
 
  [twitch]: https://twitch.tv/iwillspeak
  [yt]: https://www.youtube.com/playlist?list=PLCum1jXOlhoRCBewbQD8ELE7B_7EWnWaO
  [contributing]: CONTRIBUTING.md
  [devguide]: DEVGUIDE.md
  [exmaple_gist]: https://gist.github.com/iwillspeak/efc9342c63c07a2a763dd814f555f7ee
  [roadmap_issues]: https://github.com/iwillspeak/feersum/issues?q=is%3Aopen+is%3Aissue+label%3ARoadmap
