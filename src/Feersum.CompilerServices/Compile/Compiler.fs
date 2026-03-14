namespace Feersum.CompilerServices.Compile

open System
open System.IO
open Mono.Cecil

open Feersum.CompilerServices
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Targets
open Feersum.CompilerServices.Syntax.Tree

type CompileResult =
    { Diagnostics: Diagnostic list
      EmittedAssemblyName: AssemblyNameDefinition option }

[<RequireQualifiedAccess>]
type CompileInput =
    | Program of (TextDocument * Program) list
    | Script of TextDocument * ScriptProgram

module Compilation =
    open Feersum.CompilerServices.Syntax.Parse

    /// Compile a single AST node into an assembly
    ///
    /// The plan for this is we make multiple passes over the syntax tree. First
    /// pass will be to `bind` theh tree. Resulting in a `BoundExpr`. This will
    /// attach any type information that _can_ be computed to each node, and
    /// resolve variable references to the symbols that they refer to.
    ///
    /// Once the expression is bound we will then `emit` the expression this walks
    /// the expression and writes out the corresponding .NET IL to an `Assembly`
    /// at `outputStream`. The `outputName` controls the root namespace and assembly
    /// name of the output.
    let compile options outputStream outputName symbolStream (input: CompileInput) =
        let target =
            match options.FrameworkAssmPaths with
            | [] -> TargetResolve.fromCurrentRuntime
            | paths -> TargetResolve.fromFrameworkPaths paths

        let (refTys, allLibs) =
            options.References
            |> Seq.map (Builtins.loadReferencedSignatures)
            |> Seq.append (Seq.singleton <| Builtins.loadCoreSignatures target)
            |> Seq.fold (fun (tys, sigs) (aTys, aSigs) -> (List.append tys aTys, List.append sigs aSigs)) ([], [])

        let scope =
            if options.OutputType = OutputType.Script then
                Binder.scopeFromLibraries allLibs
            else
                Binder.emptyScope

        let progs =
            match input with
            | CompileInput.Program progs -> progs |> List.map (fun (doc, prog) -> (doc, prog.Body |> List.ofSeq))
            | CompileInput.Script(doc, script) -> [ (doc, script.Body |> Option.toList) ]

        let bound =
            Instrumentation.withPhase "bind" (Binder.bind scope allLibs) progs

        let assmName =
            if hasErrors bound.Diagnostics |> not then
                bound
                |> Instrumentation.withPhase "lower" Lower.lower
                |> Instrumentation.withPhase "emit" (fun lowered ->
                    Emit.emit options target outputStream outputName symbolStream refTys lowered)
                |> Some
            else
                None

        Instrumentation.compilationCount.Add(1L)

        Instrumentation.compilationErrors.Add(bound.Diagnostics |> Seq.sumBy (fun d -> if isError d then 1L else 0L))

        { Diagnostics = bound.Diagnostics
          EmittedAssemblyName = assmName }

    /// Read a collection of Files and Compile
    ///
    /// Takes the `sources` to an input to read and compile. Compilation results
    /// are written to `output`.
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
                // Ensure the output path exists
                let dir = Path.GetDirectoryName(output)

                if not (String.IsNullOrWhiteSpace dir) then
                    // ensure the output directory exists, no need to check it is missing first
                    Directory.CreateDirectory(dir) |> ignore

                dir, output

        // Normalise the stem and output path. This ensures the output is a file
        // not a directory.
        let output =
            if String.IsNullOrWhiteSpace(Path.GetFileName(output)) then
                Path.ChangeExtension(
                    Path.Join(outDir, Path.GetFileName(sources |> List.last)),
                    options.DefaultExtension
                )
            else
                output

        let result =
            sources
            |> Seq.map (fun path ->
                let contents = File.ReadAllText(path)
                let doc = TextDocument.fromParts path contents
                Parse.readProgram path contents |> ParseResult.map (fun r -> doc, r))
            |> ParseResult.fold (fun (progs) (p) -> List.append progs [ p ]) []

        if Diagnostics.hasErrors result.Diagnostics then
            result.Diagnostics
        else

            // Open the output streams. We don't use an `Option` directly here for
            // the symbols stream so we can drop it with `use`.
            use outputStream = File.OpenWrite output

            use symbols =
                match options.Configuration with
                | BuildConfiguration.Debug -> File.OpenWrite(Path.ChangeExtension(output, "pdb")) :> Stream
                // make a symbol file
                | BuildConfiguration.Release -> null

            let result =
                compile
                    options
                    outputStream
                    (Path.GetFileName(output))
                    (symbols |> Option.ofObj)
                    (CompileInput.Program result.Root)

            if result.Diagnostics.IsEmpty && options.OutputType = OutputType.Exe then
                match result.EmittedAssemblyName with
                | Some(assemblyName) -> Runtime.writeRuntimeConfig options output assemblyName outDir
                | None -> ()

            result.Diagnostics

    /// Read a File and Compile
    ///
    /// Takes the `source` to an input to read and compile. Compilation results
    /// are written to `output`.
    let compileFile options output source = compileFiles options output [ source ]
