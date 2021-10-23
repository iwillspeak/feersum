module Runtime

open Mono.Cecil
open System
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyModel
open Targets

/// Write a runtime config json 
let public writeRuntimeConfig options (assemblyPath: string) (assemblyName: AssemblyNameDefinition) outputDir =
    // TOOD: This metadata needs to be abstracted to deal with different
    //       target framework's prefrences. For now the `.exe` we generate
    //       is compatible with .NET Core and Mono. It would be nice to make
    //       this explicit somewhere in future.
    //       It would be nice to register ourselves as a proper SDK so that
    //       this metadata is generated for us by `dotnet`.
    let tfVersion = Environment.Version
    let target = TargetInfo(sprintf ".NETCoreApp,Version=v%i.%i" tfVersion.Major tfVersion.Minor, null, null, true)
    let tfmPrefix =
        if RuntimeInformation.FrameworkDescription.StartsWith ".NET Core" then
            "netcoreapp"
        else
            "net"
    let config =
        sprintf """
        {
          "runtimeOptions": {
            "tfm": "%s%i.%i",
            "framework": {
              "name": "Microsoft.NETCore.App",
              "version": "%A"
            },
            "additionalProbingPaths": [
              "%s"
            ]
          }
        }
        """ tfmPrefix tfVersion.Major tfVersion.Minor tfVersion outputDir
    File.WriteAllText(Path.Combine(outputDir, assemblyName.Name + ".runtimeconfig.json"), config)

    let sehrefa = typeof<Serehfa.ConsPair>.Assembly
    let sehrefaName = sehrefa.GetName()

    let deps =
        [ Dependency(sehrefaName.Name, sehrefaName.Version.ToString()) ]

    let libs =
        [ RuntimeLibrary("project", assemblyName.Name, assemblyName.Version.ToString(), "", [ RuntimeAssetGroup("", Seq.singleton (Path.GetFileName(assemblyPath))) ], [], [], deps, false)
        ; RuntimeLibrary("reference", sehrefaName.Name, sehrefaName.Version.ToString(), "", [ RuntimeAssetGroup("", Seq.singleton (Path.GetFileName(sehrefa.Location))) ], [], [], [], false, Path.GetDirectoryName(sehrefa.Location), "") ]

    let context = DependencyContext(target, CompilationOptions.Default, Seq.empty, libs, Seq.empty)

    use depsFile = File.OpenWrite(Path.Combine(outputDir, assemblyName.Name + ".deps.json"))
    depsFile.SetLength(0L)
    let writer = DependencyContextWriter()
    writer.Write(context, depsFile)