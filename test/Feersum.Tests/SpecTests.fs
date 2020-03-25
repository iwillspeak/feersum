module SpecTests

open Xunit
open Compile
open System.IO

let specDir = Path.Join(__SOURCE_DIRECTORY__, "..", "..", "spec")

let listSpecs =
    Directory.GetFiles(specDir, "*.scm", SearchOption.AllDirectories)
    |> Seq.map (fun x -> [| Path.GetRelativePath(specDir, x) |])

[<Theory>]
[<MemberDataAttribute("listSpecs")>]
let ``spec tests compile`` s =
    match compileFile (Path.Join(specDir, s)) with
    | Ok _ -> ()
    | Error e -> failwithf "Compilation error: %A" e
