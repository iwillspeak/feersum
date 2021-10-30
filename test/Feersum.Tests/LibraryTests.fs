module LibraryTests

open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Libraries
open Xunit
open Feersum.CompilerServices.Utils

[<Fact>]
let ``Match library names`` () =
    Assert.True(matchLibraryName [] [])
    Assert.True(matchLibraryName ["test"] ["test"])
    Assert.True(matchLibraryName ["scheme";"base"] ["scheme";"base"])
    Assert.False(matchLibraryName ["scheme"] ["scheme";"base"])
    Assert.False(matchLibraryName ["scheme";"base"] ["scheme"])

[<Fact>]
let ``pretty names`` () =
    Assert.Equal("()", prettifyLibraryName [])
    Assert.Equal("(test library)", prettifyLibraryName ["test";"library"])
    Assert.Equal("(foo)", prettifyLibraryName ["foo"])

[<Fact>]
let ``resolve exported bindings`` () =
    let libs = [
        { LibraryName = ["test";"lib"]
        ; Exports = [("foo", StorageRef.Global("Mock", Field "foo-internal"))] }
        ]

    let checkImported import expectedId expected =
        let result = resolveImport libs import
        Assert.True (Result.isOk result)
        let signature = Result.unwrap result
        Assert.Equal<string>(expectedId, signature.LibraryName)
        Assert.Equal<string>(expected, (signature.Exports |> List.map (fun (id, _) -> id)))

    Assert.True (Result.isError (resolveImport libs (ImportSet.Plain(["not";"valid"]))))
    checkImported (ImportSet.Plain(["test";"lib"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Only(ImportSet.Plain(["test";"lib"]), ["test"])) ["test";"lib"] []
    checkImported (ImportSet.Only(ImportSet.Plain(["test";"lib"]), ["foo"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Except(ImportSet.Plain(["test";"lib"]), ["test"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Prefix(ImportSet.Plain(["test";"lib"]), "fds/")) ["test";"lib"] ["fds/foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [{ SymbolRename.From = "missing"; To = "fail" }])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [{ SymbolRename.From = "foo"; To = "super" }])) ["test";"lib"] ["super"]
