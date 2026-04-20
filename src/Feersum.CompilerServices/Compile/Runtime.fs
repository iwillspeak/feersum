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
    // This is all _somewhat_ of a hack. It shoulldn't be relied on for more
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

/// Write a runtime config json
let public writeRuntimeConfig
    (options: CompilationOptions)
    (assemblyPath: string)
    (assemblyName: AssemblyNameDefinition)
    outputDir
    =
    if options.GenerateDepsFiles then

        let tfVersion, target, tfmPrefix = getTfInfoForCurrentRuntime ()

        let sehrefa = typeof<Serehfa.ConsPair>.Assembly

        let referencePaths =
            // FIXME: We shouldn't be worrying about adding the core
            // library to the references like this. Feels like a job
            // for the options or the compilation before us.
            if List.contains (Path.GetFileName sehrefa.Location) options.References then
                options.References
            else
                sehrefa.Location :: options.References
            |> List.map Path.GetFullPath


        // -- Emit the .runtimeconfig.json files -------------------------------

        let config =
            {| RuntimeOptions =
                {| Tfm = sprintf "%s%i.%i" tfmPrefix tfVersion.Major tfVersion.Minor
                   Framework =
                    {| Name = "Microsoft.NETCore.App"
                       Version = tfVersion.ToString() |} |} |}

        writeSerialised outputDir (assemblyName.Name + ".runtimeconfig.json") config

        // Emit a .runtimeconfig.dev.json overlay. The .NET host merges this file
        // with the base runtimeconfig at startup; reference assemblies are copied
        // into the output directory so the app-base probe can find them flat.
        let searchPaths =
            referencePaths
            |> List.map Path.GetDirectoryName
            |> List.distinct
            |> String.concat ","

        let devConfig =
            {| RuntimeOptions = {| ConfigProperties = dict [ "APP_PATHS", box searchPaths ] |} |}

        writeSerialised outputDir (assemblyName.Name + ".runtimeconfig.dev.json") devConfig


        // -- Emit the .deps.json file -----------------------------------------

        let deps =
            referencePaths
            |> List.map (fun r ->
                let name = Builtins.getAssemblyName r
                Dependency(name.Name, name.Version.ToString()))

        let refLibs =
            Seq.zip referencePaths deps
            |> Seq.map (fun (ref, dep) -> intoRuntimeLib "reference" dep.Name dep.Version (Path.GetFileName(ref)) [])

        let baseLibs =
            [ deps
              |> intoRuntimeLib
                  "project"
                  assemblyName.Name
                  (assemblyName.Version.ToString())
                  (Path.GetFileName(assemblyPath)) ]

        let libs = Seq.append baseLibs refLibs

        let context =
            DependencyContext(target, CompilationOptions.Default, Seq.empty, libs, Seq.empty)

        use depsFile =
            File.OpenWrite(Path.Combine(outputDir, assemblyName.Name + ".deps.json"))

        depsFile.SetLength(0L)
        let writer = DependencyContextWriter()
        writer.Write(context, depsFile)
