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
let private intoRuntimeLib kind name version (path: string) deps =
    RuntimeLibrary(
        kind,
        name,
        version,
        "",
        [ RuntimeAssetGroup("", Seq.singleton (Path.GetFileName(path))) ],
        [],
        [],
        deps,
        false,
        Path.GetDirectoryName(path),
        ""
    )

/// Write a runtime config json
let public writeRuntimeConfig
    (options: CompilationOptions)
    (assemblyPath: string)
    (assemblyName: AssemblyNameDefinition)
    outputDir
    =
    if options.GenerateDepsFiles then

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

        let config =
            {| RuntimeOptions =
                {| Tfm = (sprintf "%s%i.%i" tfmPrefix tfVersion.Major tfVersion.Minor)
                   Framework =
                    {| Name = "Microsoft.NETCore.App"
                       Version = tfVersion.ToString() |}
                   AdditionalProbingPaths = [| outputDir |] |} |}

        let mutable opts = JsonSerializerOptions(JsonSerializerDefaults.Web)

        opts.WriteIndented <- true
        opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase

        File.WriteAllText(
            Path.Combine(outputDir, assemblyName.Name + ".runtimeconfig.json"),
            JsonSerializer.Serialize(config, opts)
        )

        let sehrefa = typeof<Serehfa.ConsPair>.Assembly

        let referencePaths =
            // FIXME: We shouldn't be worrying about adding the core
            // library to the references like this. Feels like a job
            // for the options or the compilation before us.
            if List.contains (Path.GetFileName(sehrefa.Location)) options.References then
                options.References
            else
                sehrefa.Location :: options.References
            |> List.map (Path.GetFullPath)

        let deps =
            referencePaths
            |> List.map (fun r ->
                let name = Builtins.getAssemblyName r
                Dependency(Path.GetFileName(r), name.Version.ToString()))

        let refLibs =
            Seq.zip referencePaths deps
            |> Seq.map (fun (ref, dep) -> intoRuntimeLib "reference" dep.Name dep.Version ref [])

        let baseLibs =
            [ deps
              |> intoRuntimeLib "project" assemblyName.Name (assemblyName.Version.ToString()) assemblyPath ]

        let libs = Seq.append baseLibs refLibs

        let context =
            DependencyContext(target, CompilationOptions.Default, Seq.empty, libs, Seq.empty)

        use depsFile =
            File.OpenWrite(Path.Combine(outputDir, assemblyName.Name + ".deps.json"))

        depsFile.SetLength(0L)
        let writer = DependencyContextWriter()
        writer.Write(context, depsFile)
