// Learn more about F# at http://fsharp.org

open Options
open Syntax
open System
open Interpret
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
    | Interpret
    | [<AltCommandLine("-o")>] Output of string
    | [<MainCommand>] Sources of source_file:string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Configuration _ -> "The build configuration (Debug / Release)."
            | Version -> "Print the program version and exit."
            | Interpret -> "Use the legacy interpreter in the REPL."
            | Sources _ -> "Scheme source files for compilation."
            | Output _ -> "The output path to write compilation results to."

/// Write the diagnostics to the standard error
let private dumpDiagnostics diags =
    diags
    |> List.rev
    |> Seq.iter (fun x -> eprintfn "Error: %s" (x.ToString()))

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

/// Print a value out to the console
let print value =
    value |> externalRepr |> printfn "]= %s"

/// Print an object out to the console. Used to serialise the external
/// representation form an eval
let printObj value =
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
let private compileSingle output sourcePath =
    let outputPath =
        match output with
        | Some(path) -> path
        | None -> Path.ChangeExtension(sourcePath, "exe")
    match compileFile outputPath sourcePath with
    | [] -> ()
    | diagnostics -> dumpDiagnostics(diagnostics)

/// Run the REPL, using either the reflection-based evaluator, or the tree
/// walk interpreter.
let private runRepl interpret =
    ReadLine.HistoryEnabled <- true
    if interpret then
        execute >> Result.map print
    else
        eval >> Result.map printObj
    |> repl

/// Get the version string for the compiler.
let private versionString =
    let assm = Assembly.GetExecutingAssembly()
    let simpleVersion = assm.GetName().Version
    let infoVersinoAttr = 
        Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyInformationalVersionAttribute>()
    match infoVersinoAttr with
    | null -> simpleVersion.ToString()
    | attr -> attr.InformationalVersion

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArguments>(programName = "feersum-scheme", errorHandler = errorHandler)
    let args = parser.Parse(argv)
    
    if args.Contains Version then
        printfn "Feersum Scheme Compiler - %s" versionString
        exit 0

    match args.GetResult(Sources, defaultValue = []) with
    | [] -> runRepl (args.Contains Interpret)
    | files -> Seq.iter (compileSingle (args.TryGetResult Output)) files

    0 // return an integer exit code
