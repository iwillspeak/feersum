module Feersum.CompilerServices.Runtime

open Mono.Cecil
open System
open System.Text.Json
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyModel
open Feersum.CompilerServices
open Feersum.CompilerServices.Compile


/// Create a `RuntimeLibrary` from the given parts
let private intoRuntimeLib kind name version assetName deps =
    RuntimeLibrary(
        kind,
        name,
        version,
        "",
        [ RuntimeAssetGroup("", Seq.singleton assetName |> Seq.cast<string>) ],
        [],
        [],
        deps,
        false,
        "",
        ""
    )

let private serialiserOptions =
    let mutable opts = JsonSerializerOptions JsonSerializerDefaults.Web

    opts.WriteIndented <- true
    opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase

    opts

let private writeSerialised path fileName data =
    let fullPath = Path.Combine(path, fileName)
    File.WriteAllText(fullPath, JsonSerializer.Serialize(data, serialiserOptions))

let private getTfInfoForCurrentRuntime () =
    // For one-shot compilation we assume the compilation target is the same
    // as the current runtime. We generate a runtime config and deps file
    // stub to provide load hints for running locally.
    //
    // This is all _somewhat_ of a hack. It shouldn't be relied on for more
    // than quick local testing. For anything complex MSBuild will generate
    // a real `.deps.json` file for us.
    let tfVersion = Environment.Version

    let target =
        TargetInfo(sprintf ".NETCoreApp,Version=v%i.%i" tfVersion.Major tfVersion.Minor, null, null, true)

    let tfmPrefix =
        if RuntimeInformation.FrameworkDescription.StartsWith ".NET Core" then
            "netcoreapp"
        else
            "net"

    tfVersion, target, tfmPrefix

/// Resolve the full set of reference assembly paths, ensuring the Serehfa
/// runtime is always included.
// FIXME: We shouldn't be worrying about adding the core library to the
// references like this. Feels like a job for the options or the compilation
// before us.
let private resolveReferencePaths (options: CompilationOptions) =
    let sehrefa = typeof<Serehfa.ConsPair>.Assembly

    if List.contains (Path.GetFileName sehrefa.Location) options.References then
        options.References
    else
        sehrefa.Location :: options.References
    |> List.map Path.GetFullPath

/// Emit the `.runtimeconfig.json` and `.runtimeconfig.dev.json` overlay for
/// the given assembly. The .NET host merges the dev overlay with the base
/// config at startup; `APP_PATHS` tells the probe where to find reference
/// assemblies.
let private emitRuntimeConfigFiles
    outputDir
    (assemblyName: AssemblyNameDefinition)
    (referencePaths: string list)
    (tfVersion: Version)
    (tfmPrefix: string)
    =
    let config =
        {| RuntimeOptions =
            {| Tfm = sprintf "%s%i.%i" tfmPrefix tfVersion.Major tfVersion.Minor
               Framework =
                {| Name = "Microsoft.NETCore.App"
                   Version = tfVersion.ToString() |} |} |}

    writeSerialised outputDir (assemblyName.Name + ".runtimeconfig.json") config

    let appPaths =
        referencePaths
        |> List.map Path.GetDirectoryName
        |> List.distinct
        |> String.concat (Path.PathSeparator.ToString())

    let devConfig =
        {| RuntimeOptions = {| ConfigProperties = dict [ "APP_PATHS", box appPaths ] |} |}

    writeSerialised outputDir (assemblyName.Name + ".runtimeconfig.dev.json") devConfig

/// Emit the `.deps.json` file for the given assembly.
let private emitDepsFile
    outputDir
    (assemblyName: AssemblyNameDefinition)
    (assemblyPath: string)
    (referencePaths: string list)
    target
    =
    let deps =
        referencePaths
        |> List.map (fun r ->
            let name = Builtins.getAssemblyName r
            Dependency(name.Name, name.Version.ToString()))

    let refLibs =
        Seq.zip referencePaths deps
        |> Seq.map (fun (ref, dep) -> intoRuntimeLib "reference" dep.Name dep.Version (Path.GetFileName ref) [])

    let baseLib =
        intoRuntimeLib
            "project"
            assemblyName.Name
            (assemblyName.Version.ToString())
            (Path.GetFileName assemblyPath)
            deps

    let context =
        DependencyContext(target, CompilationOptions.Default, Seq.empty, Seq.append [ baseLib ] refLibs, Seq.empty)

    use depsFile =
        File.OpenWrite(Path.Combine(outputDir, assemblyName.Name + ".deps.json"))

    depsFile.SetLength(0L)
    DependencyContextWriter().Write(context, depsFile)

/// Write a runtime config json
let public writeRuntimeConfig
    (options: CompilationOptions)
    (assemblyPath: string)
    (assemblyName: AssemblyNameDefinition)
    outputDir
    =
    if options.GenerateDepsFiles then
        let tfVersion, target, tfmPrefix = getTfInfoForCurrentRuntime ()
        let referencePaths = resolveReferencePaths options
        emitRuntimeConfigFiles outputDir assemblyName referencePaths tfVersion tfmPrefix
        emitDepsFile outputDir assemblyName assemblyPath referencePaths target
