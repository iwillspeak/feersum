# Compiler Structure and Development Guide

Feersum is a .NET compiler. It is built using a mixture of [F#][], [C#][], and
[Scheme][]. The compiler itself lives in the `src/Feersum` project and is an F#
exectuable. The project structure is roughly as follows:

 * `src/` - Main compiler source code. These projects are packed and publisehd
    as NuGet packages.
 * `spec/` - Scheme files that cover compilation use cases. These files are used
    in two ways: First we parse them and assert that the parser produces the
    expected syntax tree; secondly we compile them and check that executing them
    produces the expected output. This is mainly driven by **snapshot testing**
    using the [Snapper][] framework. These tests are run as part of `dotnet test`
    or can be run individually with the `runspec.sh` script.
    
    To update the snapshots after a change run `dotnet test` with the environment
    variable `UpdateSnapshots` set to `true`.
 * `test/` - .NET tests that cover parts of the compiler. This includes the
    stubs that run the `spec/` tests in `SpecTests.fs`.

## Compiler Strucutre

The compiler itself is split into 4 main passes: parse, bind, lower, and emit.

 * [**Parse**][]: In this phase we convert the input text into a stuctured tree of
   `Syntax::AstNode`s. Position information for each node is tracked. Missing
   nodes are stubed out with error information. This ensures that all source
   text _should_ parse into _some form_ of syntax tree.
 * [**Bind**][]: The raw syntax tree from the parse phase is converted into a
   semantic, or "bound" tree by the bind phase. We walk each node recursively
   binding inner parts of the tree. In this phase variables are resolved to
   storage locations (`Storageref`s), special formas are recognised and handled
   by specific branches in the binder at this stage. Some minimal re-writing of
   the tree's structure takes place to better encode the semantics of the
   program.

   In the bind phase we also identify captrued variables but _don't_ move their
   storage locations into environment slots. This takes place later in the
   `Lower` phase.
 * [**Lower**][]: In thise phase references to captured variables are re-written
   to refer to their environment location. This ensures that all captured values
   are hoisted into the environment immeidately on entering a scope rather than
   lazily at the point of use. An introduction to this process is available [on
   my blog][captivating].

   The lower phase also performs some other transforms to simplfiy some areas of
   the tree before IL code is emitted.
 * [**Emit**][]: The final phase of the compiler is to write out .NET Common
   Intermediary Language (CIL) bytecode to an Assembly. To perform this task we
   use the `Mono.Cecil` library to create an assembly definition. Again we walk
   the tree recursively and emit types and bytecode as we go.

   As scheme is a runtime typed language all Scheme values must live in `object`
   variables or fields. We represent global scheme values as public static
   fields. Private definitions that don't escape the given block and aren't
   captured live as locals. For captured values a slot index into an `object[]`
   in the `Envrionment` type is used.

   Methods without a closure are emitted as static methods. Methods with a
   clousre are emitted as instance methods on the `Enrivonment` type.

   All function calls are typed as calls to `Func<object[], object>`. When a
   Scheme function is emitted a companion *thunk* is also generated that unpacks
   the arguments array from the `object[]` and calls the real implementaiton.

 [F#]: https://docs.microsoft.com/en-gb/dotnet/fsharp/
 [C#]: https://docs.microsoft.com/en-gb/dotnet/csharp/
 [Scheme]: https://schemers.org/
 [Snapper]: https://theramis.github.io/Snapper/#/
 [**Parse**]: https://github.com/iwillspeak/feersum/blob/main/src/Feersum.CompilerServices/Syntax.fs
 [**Bind**]: https://github.com/iwillspeak/feersum/blob/main/src/Feersum.CompilerServices/Binding/Binder.fs
 [**Lower**]: https://github.com/iwillspeak/feersum/blob/main/src/Feersum.CompilerServices/Binding/Lower.fs
 [**Emit**]: https://github.com/iwillspeak/feersum/blob/main/src/Feersum.CompilerServices/Compile/Compiler.fs
 [captivating]: https://willspeak.me/2020/09/06/a-captivating-resolution.html
