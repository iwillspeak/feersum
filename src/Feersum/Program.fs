// Learn more about F# at http://fsharp.org

open System.IO
open System

open Argu.ArguAttributes
open Argu

open Feersum.Repl
open Feersum.Version
open Feersum.CompilerServices
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Compile

/// Command line arguments type. Encompasses the options that the compiler
/// supports.
type CliArguments =
    | Version
    | Configuration of BuildConfiguration
    | OutputType of OutputType
    | CoreLibPath of string
    | GenerateDeps of bool
    | AssemblyVersion of string
    | [<AltCommandLine("-r")>] Reference of string
    | [<AltCommandLine("-o")>] Output of string
    | [<MainCommand>] Sources of source_file: string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "Print the program version and exit."
            | Configuration _ -> "The build configuration (Debug / Release)."
            | OutputType _ -> "The output type (Lib / Exe / Script)."
            | CoreLibPath _ -> "Location of mscorelib.dll, or System.Runtime.dll."
            | GenerateDeps _ -> "Generate .deps.json and .runtimeconfig.json stubs."
            | AssemblyVersion _ -> "Specify the version for the generated assembly."
            | Reference _ -> "Compiled Scheme assembly to reference."
            | Output _ -> "The output path to write compilation results to."
            | Sources _ -> "Scheme source files for compilation."

/// Compile a collection of files file printing an error if
/// there is one.
let private compileAll (options: CompilationOptions) output sources =
    Runtime.GCSettings.LatencyMode <- Runtime.GCLatencyMode.Batch

    let mainSource = List.last sources

    let outputPath =
        match output with
        | Some (path) -> path
        | None -> Path.ChangeExtension(mainSource, options.DefaultExtension)

    match Compilation.compileFiles options outputPath sources with
    | [] -> 0
    | diagnostics ->
        dumpDiagnostics (diagnostics)

        if hasErrors diagnostics then -1 else 0


[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<CliArguments>(programName = "feersum-scheme", errorHandler = errorHandler)

    let args = parser.Parse(argv)

    if args.Contains Version then
        printVersion ()
        exit 0

    let buildConfig =
        args.TryGetResult Configuration
        |> Option.defaultValue Release

    let outputType =
        args.TryGetResult OutputType
        |> Option.defaultValue Exe

    let options =
        { CompilationOptions.Create buildConfig outputType with
            Version =
                args.TryGetResult AssemblyVersion
                |> Option.map Version.Parse
            References =
                args.GetResults Reference
                |> List.append coreReferences
            GenerateDepsFiles =
                (args.TryGetResult GenerateDeps)
                |> Option.defaultValue true
            MsCorePaths = args.GetResults CoreLibPath }

    match args.GetResult(Sources, defaultValue = []), args.TryGetResult(Output) with
    | [], None ->
        runRepl ()
        0
    | [], Some (output) ->
        eprintfn "No source files provided for output %s" output
        exit -1
    | files, output -> compileAll options output files
