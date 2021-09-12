module LibraryTests

open Libraries
open Xunit
open Bind
open Utils

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
    let libs = [(["test";"lib"],[("foo", StorageRef.Global("Mock", "foo-internal"))])]

    let checkImported import expectedId expected =
        let result = resolveImport libs import
        Assert.True (ResultEx.isOk result)
        let (id, exports) = ResultEx.unwrap result
        Assert.Equal<string>(expectedId, id)
        Assert.Equal<string>(expected, (exports |> List.map (fun (id, _) -> id)))

    Assert.True (ResultEx.isError (resolveImport libs (ImportSet.Plain(["not";"valid"]))))
    checkImported (ImportSet.Plain(["test";"lib"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Only(ImportSet.Plain(["test";"lib"]), ["test"])) ["test";"lib"] []
    checkImported (ImportSet.Only(ImportSet.Plain(["test";"lib"]), ["foo"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Except(ImportSet.Plain(["test";"lib"]), ["test"])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Prefix(ImportSet.Plain(["test";"lib"]), "fds/")) ["test";"lib"] ["fds/foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [{ SymbolRename.From = "missing"; To = "fail" }])) ["test";"lib"] ["foo"]
    checkImported (ImportSet.Renamed(ImportSet.Plain(["test";"lib"]), [{ SymbolRename.From = "foo"; To = "super" }])) ["test";"lib"] ["super"]
