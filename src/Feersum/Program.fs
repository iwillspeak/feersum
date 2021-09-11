// Learn more about F# at http://fsharp.org

open Options
open Syntax
open System
open Eval
open Compile
open Argu.ArguAttributes
open Argu
open System.Reflection
open System.IO

/// Command line arguments type. Encompasses the options that the compiler
/// supports.
type CliArguments =
    | Version
    | Configuration of BuildConfiguration
    | OutputType of OutputType
    | [<AltCommandLine("-o")>] Output of string
    | [<MainCommand>] Sources of source_file:string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | OutputType _ -> "The output type (Lib / Exe / Script)."
            | Configuration _ -> "The build configuration (Debug / Release)."
            | Version -> "Print the program version and exit."
            | Sources _ -> "Scheme source files for compilation."
            | Output _ -> "The output path to write compilation results to."

/// Write the diagnostics to the standard error
let private dumpDiagnostics diags =
    diags
    |> List.rev
    |> Seq.iter (fun x -> eprintfn "%s" (x.ToString()))

/// Read a single line of user input and parse it into a
/// syntax tree. If the input can't be parsed then read
/// again.
let rec read (): AstNode =
    let line = ReadLine.Read("§> ")
    match readExpr line with
    | (node, []) -> node
    | (_, diagnostics) ->
        diagnostics |> dumpDiagnostics
        read()

/// Print an object out to the console. Used to serialise the external
/// representation form an eval
let print value =
    value |> cilExternalRepr |> printfn "}= %s"

/// Read, Execute, Print Loop
///
/// Repeatedly reads input and prints output
let rec repl evaluator =
    try
        match (read >> evaluator)() with
        | Result.Ok _ -> ()
        | Result.Error diags -> dumpDiagnostics(diags)
    with
    | ex -> eprintfn "Exception: %A" ex
    repl evaluator

/// Compile a single file printing an error if
/// there is one.
let private compileSingle options output sourcePath =
    let outputPath =
        match output with
        | Some(path) -> path
        | None -> Path.ChangeExtension(sourcePath, options |> getDefaultExtension)
    match compileFile options outputPath sourcePath with
    | [] -> 0
    | diagnostics ->
        dumpDiagnostics(diagnostics)
        List.length diagnostics

/// Get the version string for the compiler.
let private versionString =
    let assm = Assembly.GetExecutingAssembly()
    let simpleVersion = assm.GetName().Version
    let infoVersinoAttr = 
        Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyInformationalVersionAttribute>()
    match infoVersinoAttr with
    | null -> simpleVersion.ToString()
    | attr -> attr.InformationalVersion

/// Print out the compiler's version string
let private printVersion () =
    printfn "Feersum Scheme Compiler - %s" versionString

/// Run the REPL, using the reflection-based evaluator.
let private runRepl () =
    ReadLine.HistoryEnabled <- true
    printVersion()
    eval >> Result.map print
    |> repl

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArguments>(programName = "feersum-scheme", errorHandler = errorHandler)
    let args = parser.Parse(argv)
    
    if args.Contains Version then
        printVersion()
        exit 0

    let buildConfig =
        args.TryGetResult Configuration
        |> Option.defaultValue Release

    let outputType =
        args.TryGetResult OutputType
        |> Option.defaultValue Exe

    let options =
        { Configuration = buildConfig
        ; OutputType = outputType }

    match args.GetResult(Sources, defaultValue = []) with
    | [] ->
        runRepl()
        0
    | files ->
        Seq.sumBy (compileSingle options (args.TryGetResult Output)) files
