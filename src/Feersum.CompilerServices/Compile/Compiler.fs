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

[<RequireQualifiedAccess>]
type CompileInput =
    | Program of FileCollection
    | Script of SyntaxRoot<ScriptProgram>

/// A bound and lowered program unit, ready for optional emission.
/// Returned by `Compilation.bind`; can be passed to tooling (e.g. LSP) that
/// needs the bound tree without producing an assembly.
[<NoComparison; NoEquality>]
type BindResult =
    {
        /// Lowered bound tree. Binding diagnostics live here.
        BoundTree: BoundSyntaxTree
        /// All top-level definitions introduced by this unit.
        OutputScope: Scope
        Options: CompilationOptions
        Target: TargetInfo
        RefTypes: TypeDefinition list
    }

/// The result of a full compilation (bind + emit). Carries everything needed
/// to chain the next REPL step via `CompileContext.Chained`.
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

/// The starting point for `Compilation.compile`: either a fresh set of options
/// or the output of a prior compilation to chain from (REPL use).
[<RequireQualifiedAccess>]
type CompileContext =
    | Fresh of CompilationOptions
    /// Chain from a previous emit output. `programName` sets the type name for
    /// globals in this step so each REPL step uses a unique, non-colliding name.
    | Chained of previous: CompilationOutput * programName: string

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

    let private emitBound (bound: BindResult) outputStream outputName symbolStream =
        let assmName, newRefTypes =
            if not (hasErrors bound.BoundTree.Diagnostics) then
                let assmName, emittedTypes =
                    bound.BoundTree
                    |> Instrumentation.withPhase "emit" (fun lowered ->
                        Emit.emit
                            bound.Options
                            bound.Target
                            outputStream
                            outputName
                            symbolStream
                            bound.RefTypes
                            lowered)

                Some assmName, List.append bound.RefTypes emittedTypes
            else
                None, bound.RefTypes

        Instrumentation.compilationCount.Add 1L

        Instrumentation.compilationErrors.Add(
            bound.BoundTree.Diagnostics |> Seq.sumBy (fun d -> if isError d then 1L else 0L)
        )

        { Diagnostics = bound.BoundTree.Diagnostics
          EmittedAssemblyName = assmName
          OutputScope = bound.OutputScope
          Options = bound.Options
          Target = bound.Target
          RefTypes = newRefTypes }

    // -- Public API -----------------------------------------------------------

    /// Bind and lower a program or script without emitting an assembly.
    ///
    /// Use this when only the bound tree is needed (e.g. LSP diagnostics,
    /// hover, go-to-definition). Pass the returned `BindResult` to
    /// `Compilation.compile` to emit when required.
    let bind options input =
        let target = resolveTarget options
        let refTys, allLibs = loadReferences options target
        let inputScope = buildScope options allLibs
        let lowered, outputScope = bindAndLower inputScope input

        { BoundTree = lowered
          OutputScope = outputScope
          Options = options
          Target = target
          RefTypes = refTys }

    /// Bind and emit a program or script in one step.
    ///
    /// Supply `CompileContext.Fresh options` for a standalone compilation or
    /// `CompileContext.Chained(previous, programName)` to inherit scope and
    /// references from a prior REPL step.
    let compile (context: CompileContext) input outputStream outputName symbolStream =
        let bound =
            match context with
            | CompileContext.Fresh options -> bind options input
            | CompileContext.Chained(previous, programName) ->
                let nextScope =
                    { previous.OutputScope with
                        ProgramName = programName }

                let lowered, outputScope = bindAndLower nextScope input

                { BoundTree = lowered
                  OutputScope = outputScope
                  Options = previous.Options
                  Target = previous.Target
                  RefTypes = previous.RefTypes }

        emitBound bound outputStream outputName symbolStream

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
                    (CompileContext.Fresh options)
                    (CompileInput.Program result.Root)
                    outputStream
                    (Path.GetFileName output)
                    (symbols |> Option.ofObj)

            if not (hasErrors result.Diagnostics) && options.OutputType = Exe then
                match result.EmittedAssemblyName with
                | Some assemblyName -> Runtime.writeRuntimeConfig options output assemblyName outDir
                | None -> ()

            result.Diagnostics

    /// Read a single file and compile it to an assembly at `output`.
    let compileFile options output source = compileFiles options output [ source ]
