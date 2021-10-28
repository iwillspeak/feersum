// Learn more about F# at http://fsharp.org

open System.Reflection
open System.IO

open Argu.ArguAttributes
open Argu

open Options
open Syntax
open System
open Eval
open Compile

/// Command line arguments type. Encompasses the options that the compiler
/// supports.
type CliArguments =
    | Version
    | Configuration of BuildConfiguration
    | OutputType of OutputType
    | CoreLibPath of string
    | GenerateDeps of bool
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
            | Reference _ -> "Compiled Scheme assembly to reference."
            | Output _ -> "The output path to write compilation results to."
            | Sources _ -> "Scheme source files for compilation."

/// Write the diagnostics to the standard error
let private dumpDiagnostics diags =
    diags
    |> List.rev
    |> Seq.iter (fun x -> eprintfn "%s" (x.ToString()))

/// Read a single line of user input and parse it into a
/// syntax tree. If the input can't be parsed then read
/// again.
let rec read () : AstNode =
    let line = ReadLine.Read("§> ")

    match readExpr line with
    | (node, []) -> node
    | (_, diagnostics) ->
        diagnostics |> dumpDiagnostics
        read ()

/// Print an object out to the console. Used to serialise the external
/// representation form an eval
let print value =
    value |> cilExternalRepr |> printfn "}= %s"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl evaluator =
    try
        match (read >> evaluator) () with
        | Result.Ok _ -> ()
        | Result.Error diags -> dumpDiagnostics (diags)
    with
    | ex -> eprintfn "Exception: %A" ex

    repl evaluator

/// Compile a collection of files file printing an error if
/// there is one.
let private compileAll (options: CompilationOptions) output sources =
    let mainSource = List.last sources

    let outputPath =
        match output with
        | Some (path) -> path
        | None -> Path.ChangeExtension(mainSource, options.DefaultExtension)

    match compileFiles options outputPath sources with
    | [] -> 0
    | diagnostics ->
        dumpDiagnostics (diagnostics)

        if Diagnostics.hasErrors diagnostics then
            -1
        else
            0

/// Get the version string for the compiler.
let private versionString =
    let assm = Assembly.GetExecutingAssembly()
    let simpleVersion = assm.GetName().Version

    let infoVersinoAttr =
        Assembly
            .GetExecutingAssembly()
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()

    match infoVersinoAttr with
    | null -> simpleVersion.ToString()
    | attr -> attr.InformationalVersion

/// Print out the compiler's version string
let private printVersion () =
    printfn "Feersum Scheme Compiler - %s" versionString

/// Run the REPL, using the reflection-based evaluator.
let private runRepl () =
    ReadLine.HistoryEnabled <- true
    printVersion ()
    eval >> Result.map print |> repl

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
              References = args.GetResults Reference
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
