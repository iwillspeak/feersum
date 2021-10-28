module Runtime

open Mono.Cecil
open System
open System.Text.Json
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyModel
open Options
open Targets

/// Write a runtime config json 
let public writeRuntimeConfig (options: CompilationOptions) (assemblyPath: string) (assemblyName: AssemblyNameDefinition) outputDir =
  if options.GenerateDepsFiles then
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
    let config = {|
        RuntimeOptions =
          {| Tfm = (sprintf "%s%i.%i" tfmPrefix tfVersion.Major tfVersion.Minor);
          Framework =
          {|
            Name = "Microsoft.NETCore.App";
            Version = tfVersion.ToString();
          |};
          AdditionalProbingPaths = [| outputDir |] |} |}
    let mutable opts = JsonSerializerOptions(JsonSerializerDefaults.Web)
    opts.WriteIndented <- true
    opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    File.WriteAllText(
      Path.Combine(outputDir, assemblyName.Name + ".runtimeconfig.json"),
      JsonSerializer.Serialize(config, opts))

    let sehrefa = typeof<Serehfa.ConsPair>.Assembly
    let sehrefaName = sehrefa.GetName()

    let referencePaths =
      if List.contains (Path.GetFileName(sehrefa.Location)) options.References then
        options.References
      else
        sehrefa.Location :: options.References
    let deps =
      referencePaths
      |> List.map (fun r ->
        let name = Builtins.getAssemblyName r
        Dependency(Path.GetFileName(r), name.Version.ToString()))

    let refLibs =
      Seq.zip referencePaths deps
      |> Seq.map (fun (ref, dep) ->
        RuntimeLibrary(
          "reference",
          dep.Name,
          dep.Version,
          "",
          [ RuntimeAssetGroup("", Seq.singleton (Path.GetFileName(ref))) ],
          [],
          [],
          [],
          false,
          Path.GetDirectoryName(ref),
          ""))

    let baseLibs = [
        RuntimeLibrary(
          "project",
          assemblyName.Name,
          assemblyName.Version.ToString(),
          "",
          [ RuntimeAssetGroup("", Seq.singleton (Path.GetFileName(assemblyPath))) ],
          [],
          [],
          deps,
          false)
      ]

    let libs = Seq.append baseLibs refLibs

    let context = DependencyContext(target, CompilationOptions.Default, Seq.empty, libs, Seq.empty)

    use depsFile = File.OpenWrite(Path.Combine(outputDir, assemblyName.Name + ".deps.json"))
    depsFile.SetLength(0L)
    let writer = DependencyContextWriter()
    writer.Write(context, depsFile)
