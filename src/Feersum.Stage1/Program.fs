﻿open Argu.ArguAttributes
open Argu

open Feersum.CompilerServices
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Compile
open System

/// Command line arguments type. Cut-down stage1 compiler options
type CliArguments =
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
            | Configuration _ -> "The build configuration (Debug / Release)."
            | OutputType _ -> "The output type (Lib / Exe / Script)."
            | CoreLibPath _ -> "Location of mscorlib.dll, or System.Runtime.dll."
            | GenerateDeps _ -> "Generate .deps.json and .runtimeconfig.json stubs."
            | AssemblyVersion _ -> "Generated assembly version."
            | Reference _ -> "Compiled Scheme assembly to reference."
            | Output _ -> "The output path to write compilation results to."
            | Sources _ -> "Scheme source files for compilation."


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "feersum-stage1")

    let args = parser.Parse(argv)

    let buildConfig = args.TryGetResult Configuration |> Option.defaultValue Release

    let outputType = args.TryGetResult OutputType |> Option.defaultValue Exe

    let options =
        { CompilationOptions.Create buildConfig outputType with
            Version = args.TryGetResult AssemblyVersion |> Option.map Version.Parse
            References = args.GetResults Reference
            GenerateDepsFiles = (args.TryGetResult GenerateDeps) |> Option.defaultValue true
            FrameworkAssmPaths = args.GetResults CoreLibPath }

    let diags =
        Compilation.compileFiles options (args.GetResult(Output)) (args.GetResult(Sources))

    if hasErrors diags then
        dumpDiagnostics diags
        diags |> List.length
    else
        0
