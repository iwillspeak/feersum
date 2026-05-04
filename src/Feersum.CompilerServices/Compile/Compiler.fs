namespace Feersum.CompilerServices.Compile

open System
open System.IO
open Mono.Cecil

open Feersum.CompilerServices
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Targets
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Parse

type CompileResult =
    { Diagnostics: Diagnostic list
      EmittedAssemblyName: AssemblyNameDefinition option }

[<RequireQualifiedAccess>]
type CompileInput =
    | Program of FileCollection
    | Script of SyntaxRoot<ScriptProgram>

/// A bound and lowered program unit, ready for optional emission.
[<NoComparison; NoEquality>]
type Compilation =
    {
        /// Lowered bound tree. Binding diagnostics live here.
        BoundTree: BoundSyntaxTree
        /// All top-level definitions introduced by this unit.
        /// Pass to `Compilation.createDerived` for REPL chaining.
        OutputScope: Scope
        /// Retained for `Compilation.emit`.
        Options: CompilationOptions
        /// Retained for `Compilation.emit`.
        Target: TargetInfo
        /// External type references from referenced assemblies, retained for `Compilation.emit`.
        RefTypes: TypeDefinition list
    }

/// The result of emitting a `Compilation`. Carries everything `createChained`
/// needs to chain the next REPL step: the output scope and accumulated extern
/// types (original references plus every type emitted by prior REPL steps).
[<NoComparison; NoEquality>]
type CompilationOutput =
    {
        Diagnostics: Diagnostic list
        EmittedAssemblyName: AssemblyNameDefinition option
        OutputScope: Scope
        Options: CompilationOptions
        Target: TargetInfo
        /// Accumulated extern TypeDefinitions: original references plus every
        /// type emitted by prior REPL steps. Passed as `externTys` to Emit.emit
        /// in the next step so previous globals are reachable as extern fields.
        RefTypes: TypeDefinition list
    }

module Compilation =

    // -- Helpers --------------------------------------------------------------

    let private resolveTarget options =
        match options.FrameworkAssmPaths with
        | [] -> TargetResolve.fromCurrentRuntime
        | paths -> TargetResolve.fromFrameworkPaths paths

    let private loadReferences options target =
        let coreLibs = Builtins.loadCoreSignatures target

        options.References
        |> Seq.map Builtins.loadReferencedSignatures
        |> Seq.append (Seq.singleton coreLibs)
        |> Seq.fold (fun (tys, sigs) (aTys, aSigs) -> List.append tys aTys, List.append sigs aSigs) ([], [])

    let private buildScope options allLibs =
        let baseScope =
            if options.OutputType = OutputType.Script then
                Scope.ofLibraries allLibs
            else
                { Scope.empty with Libs = allLibs }

        let macros, stxEnv = Builtins.loadBuiltinMacroEnv baseScope.StxEnv

        { baseScope with
            StxEnv = stxEnv
            Macros = macros }

    let private bindAndLower inputScope input =
        let bound, outputScope =
            Instrumentation.withPhase
                "bind"
                (fun () ->
                    match input with
                    | CompileInput.Program progs -> Binder.bindProgram inputScope progs
                    | CompileInput.Script script -> Binder.bindScript inputScope script)
                ()

        let lowered = Instrumentation.withPhase "lower" Lower.lower bound
        lowered, outputScope

    // -- Public API -----------------------------------------------------------

    /// Bind and lower a program or script.
    ///
    /// Resolves references and builds the initial scope from `options`. The
    /// returned `Compilation` can be passed to `emit` to write an assembly, or
    /// inspected directly for diagnostics and the bound tree without emitting.
    let create options input =
        let target = resolveTarget options
        let refTys, allLibs = loadReferences options target
        let inputScope = buildScope options allLibs
        let lowered, outputScope = bindAndLower inputScope input

        { BoundTree = lowered
          OutputScope = outputScope
          Options = options
          Target = target
          RefTypes = refTys }

    /// Bind and lower, chaining scope from a previous emit output.
    ///
    /// Inherits `Target`, `RefTypes`, and `Options` from `previous` (reference
    /// loading is skipped). Uses `previous.OutputScope` as the input scope so
    /// all prior top-level definitions remain visible. The caller supplies
    /// `programName` to give each REPL step a distinct type name.
    let createChained (previous: CompilationOutput) (programName: string) input =
        let nextScope =
            { previous.OutputScope with
                ProgramName = programName }

        let lowered, outputScope = bindAndLower nextScope input

        { BoundTree = lowered
          OutputScope = outputScope
          Options = previous.Options
          Target = previous.Target
          RefTypes = previous.RefTypes }

    /// Emit a lowered compilation as a .NET assembly.
    ///
    /// When the bound tree contains errors, emission is skipped and only the
    /// diagnostics are returned. Returns `CompilationOutput` so the caller can
    /// pass it to `createChained` for REPL chaining.
    let emit compilation outputStream outputName symbolStream =
        let assmName, newRefTypes =
            if not (hasErrors compilation.BoundTree.Diagnostics) then
                let assmName, emittedTypes =
                    compilation.BoundTree
                    |> Instrumentation.withPhase "emit" (fun lowered ->
                        Emit.emit
                            compilation.Options
                            compilation.Target
                            outputStream
                            outputName
                            symbolStream
                            compilation.RefTypes
                            lowered)

                Some assmName, List.append compilation.RefTypes emittedTypes
            else
                None, compilation.RefTypes

        Instrumentation.compilationCount.Add 1L

        Instrumentation.compilationErrors.Add(
            compilation.BoundTree.Diagnostics
            |> Seq.sumBy (fun d -> if isError d then 1L else 0L)
        )

        { Diagnostics = compilation.BoundTree.Diagnostics
          EmittedAssemblyName = assmName
          OutputScope = compilation.OutputScope
          Options = compilation.Options
          Target = compilation.Target
          RefTypes = newRefTypes }

    /// Bind, lower, and emit in a single call.
    ///
    /// Sugar over `create` + `emit`. Preserves the existing call-site signature
    /// for callers that don't need scope chaining.
    let compile options outputStream outputName symbolStream input =
        let output =
            create options input |> fun c -> emit c outputStream outputName symbolStream

        { Diagnostics = output.Diagnostics
          EmittedAssemblyName = output.EmittedAssemblyName }

    // -- File-level entry points ----------------------------------------------

    /// Read a collection of files and compile them to an assembly at `output`.
    let compileFiles (options: CompilationOptions) (output: string) (sources: string list) =

        // Handle the case that the user has specified a path to a directory but
        // is missing the trailing `/`
        let outDir, output =
            if Directory.Exists(output) then
                if Path.EndsInDirectorySeparator(output) then
                    output, output
                else
                    output, sprintf "%s%c" output Path.DirectorySeparatorChar
            else
                let dir = Path.GetDirectoryName(output)

                if not (String.IsNullOrWhiteSpace dir) then
                    Directory.CreateDirectory(dir) |> ignore

                dir, output

        // Normalise the stem and output path.
        let output =
            if String.IsNullOrWhiteSpace(Path.GetFileName(output)) then
                Path.ChangeExtension(
                    Path.Join(outDir, Path.GetFileName(sources |> List.last)),
                    options.DefaultExtension
                )
            else
                output

        let result = FileCollection.ofPaths sources

        if hasErrors result.Diagnostics then
            result.Diagnostics
        else
            use outputStream = File.OpenWrite output

            use symbols =
                match options.Configuration with
                | BuildConfiguration.Debug -> File.OpenWrite(Path.ChangeExtension(output, "pdb")) :> Stream
                | BuildConfiguration.Release -> null

            let result =
                compile
                    options
                    outputStream
                    (Path.GetFileName output)
                    (symbols |> Option.ofObj)
                    (CompileInput.Program result.Root)

            if not (hasErrors result.Diagnostics) && options.OutputType = Exe then
                match result.EmittedAssemblyName with
                | Some assemblyName -> Runtime.writeRuntimeConfig options output assemblyName outDir
                | None -> ()

            result.Diagnostics

    /// Read a single file and compile it to an assembly at `output`.
    let compileFile options output source = compileFiles options output [ source ]
