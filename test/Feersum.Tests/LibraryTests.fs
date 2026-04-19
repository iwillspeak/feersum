module LibraryTests

open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Libraries
open Feersum.CompilerServices
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Factories.Convenience
open Xunit
open Feersum.CompilerServices.Utils

// Test the new Stx-based Library API
// Helper functions for creating Stx values for testing
let private dummyLoc = TextLocation.Missing

let private id name =
    Feersum.CompilerServices.Binding.Stx.Id(name, dummyLoc)

let private number n =
    Feersum.CompilerServices.Binding.Stx.Datum(Feersum.CompilerServices.Binding.StxDatum.Number n, dummyLoc)

let private list items =
    Feersum.CompilerServices.Binding.Stx.List(items, None, dummyLoc)


[<Fact>]
let ``Match library names`` () =
    Assert.True(matchLibraryName [] [])
    Assert.True(matchLibraryName [ "test" ] [ "test" ])

    Assert.True(matchLibraryName [ "scheme"; "base" ] [ "scheme"; "base" ])

    Assert.False(matchLibraryName [ "scheme" ] [ "scheme"; "base" ])

    Assert.False(matchLibraryName [ "scheme"; "base" ] [ "scheme" ])


[<Fact>]
let ``pretty names`` () =
    Assert.Equal("()", prettifyLibraryName [])

    Assert.Equal("(test library)", prettifyLibraryName [ "test"; "library" ])

    Assert.Equal("(foo)", prettifyLibraryName [ "foo" ])

[<Fact>]
let ``resolve exported bindings`` () =
    let libs =
        [ { LibraryName = [ "test"; "lib" ]
            Exports = [ ("foo", Binding.StorageRef.Global("Mock", Binding.GlobalType.Field "foo-internal")) ] } ]

    let checkImported import expectedId expected =
        let result = resolveImport libs import
        Assert.True(Result.isOk result)
        let signature = Result.unwrap result
        Assert.Equal<string>(expectedId, signature.LibraryName)
        Assert.Equal<string>(expected, (signature.Exports |> List.map (fun (id, _) -> id)))

    Assert.True(Result.isError (resolveImport libs (ImportSet.Plain([ "not"; "valid" ]))))
    checkImported (ImportSet.Plain([ "test"; "lib" ])) [ "test"; "lib" ] [ "foo" ]
    checkImported (ImportSet.Only(ImportSet.Plain([ "test"; "lib" ]), [ "test" ])) [ "test"; "lib" ] []
    checkImported (ImportSet.Only(ImportSet.Plain([ "test"; "lib" ]), [ "foo" ])) [ "test"; "lib" ] [ "foo" ]
    checkImported (ImportSet.Except(ImportSet.Plain([ "test"; "lib" ]), [ "test" ])) [ "test"; "lib" ] [ "foo" ]
    checkImported (ImportSet.Prefix(ImportSet.Plain([ "test"; "lib" ]), "fds/")) [ "test"; "lib" ] [ "fds/foo" ]
    checkImported (ImportSet.Renamed(ImportSet.Plain([ "test"; "lib" ]), [])) [ "test"; "lib" ] [ "foo" ]

    checkImported
        (ImportSet.Renamed(
            ImportSet.Plain([ "test"; "lib" ]),
            [ { SymbolRename.From = "missing"
                To = "fail" } ]
        ))
        [ "test"; "lib" ]
        [ "foo" ]

    checkImported
        (ImportSet.Renamed(
            ImportSet.Plain([ "test"; "lib" ]),
            [ { SymbolRename.From = "foo"
                To = "super" } ]
        ))
        [ "test"; "lib" ]
        [ "super" ]


[<Fact>]
let ``parse library name from identifier list`` () =
    let result = Libraries.parseLibraryDefinition (list [ id "test"; id "lib" ]) []
    Assert.True(Result.isOk result)
    let ok = Result.unwrap result
    let (lib, _) = ok
    Assert.Equal<string list>([ "test"; "lib" ], lib.LibraryName)
    Assert.Empty(lib.Declarations)

[<Fact>]
let ``parse library name with mixed identifiers and numbers`` () =
    let result =
        Libraries.parseLibraryDefinition (list [ id "scheme"; id "base"; number 7 ]) []

    Assert.True(Result.isOk result)
    let ok = Result.unwrap result
    let (lib, _) = ok
    Assert.Equal<string list>([ "scheme"; "base"; "7" ], lib.LibraryName)

[<Fact>]
let ``parse reserved library name prefix`` () =
    let result = Libraries.parseLibraryDefinition (list [ id "scheme"; id "custom" ]) []
    Assert.True(Result.isOk result)
    let ok = Result.unwrap result
    let (lib, diags) = ok
    Assert.Equal<string list>([ "scheme"; "custom" ], lib.LibraryName)
    Assert.NotEmpty(diags)

[<Fact>]
let ``parse library with export declaration`` () =
    let exportDecl = list [ id "export"; id "func1"; id "func2" ]
    let result = Libraries.parseLibraryDefinition (list [ id "mylib" ]) [ exportDecl ]
    Assert.True(Result.isOk result)
    let ok = Result.unwrap result
    let (lib, _) = ok
    Assert.Equal<string list>([ "mylib" ], lib.LibraryName)
    Assert.Equal(1, List.length lib.Declarations)

    let decl = lib.Declarations[0]

    match decl with
    | LibraryDeclaration.Export exports ->
        Assert.Equal(2, List.length exports)

        match exports[0], exports[1] with
        | ExportSet.Plain "func1", ExportSet.Plain "func2" -> ()
        | _ -> Assert.True(false, "Expected plain exports")
    | _ -> Assert.True(false, "Expected export declaration")

[<Fact>]
let ``parse library with import declaration`` () =
    let importDecl = list [ id "import"; list [ id "test"; id "lib" ] ]
    let result = Libraries.parseLibraryDefinition (list [ id "mylib" ]) [ importDecl ]
    Assert.True(Result.isOk result)
    let ok = Result.unwrap result
    let (lib, _) = ok
    Assert.Equal<string list>([ "mylib" ], lib.LibraryName)
    Assert.Equal(1, List.length lib.Declarations)

    let decl = lib.Declarations[0]

    match decl with
    | LibraryDeclaration.Import imports ->
        Assert.Equal(1, List.length imports)

        match imports[0] with
        | ImportSet.Plain [ "test"; "lib" ] -> ()
        | _ -> Assert.True(false, "Expected plain import")
    | _ -> Assert.True(false, "Expected import declaration")

[<Fact>]
let ``parse import with only filter`` () =
    let importStx =
        list [ id "only"; list [ id "test"; id "lib" ]; id "func1"; id "func2" ]

    let diags = Feersum.CompilerServices.Diagnostics.DiagnosticBag.Empty
    let importSet = Libraries.parseImport diags importStx

    match importSet with
    | ImportSet.Only(ImportSet.Plain [ "test"; "lib" ], f) -> Assert.Equal<string list>([ "func1"; "func2" ], f)
    | _ -> Assert.True(false, "Expected only import set")

[<Fact>]
let ``parse import with except filter`` () =
    let importStx = list [ id "except"; list [ id "test"; id "lib" ]; id "private" ]
    let diags = Feersum.CompilerServices.Diagnostics.DiagnosticBag.Empty
    let importSet = Libraries.parseImport diags importStx

    match importSet with
    | ImportSet.Except(ImportSet.Plain [ "test"; "lib" ], e) -> Assert.Equal<string list>([ "private" ], e)
    | _ -> Assert.True(false, "Expected except import set")

[<Fact>]
let ``parse import with prefix`` () =
    let importStx = list [ id "prefix"; list [ id "test"; id "lib" ]; id "lib/" ]
    let diags = Feersum.CompilerServices.Diagnostics.DiagnosticBag.Empty
    let importSet = Libraries.parseImport diags importStx

    match importSet with
    | ImportSet.Prefix(ImportSet.Plain [ "test"; "lib" ], p) -> Assert.Equal("lib/", p)
    | _ -> Assert.True(false, "Expected prefix import set")

[<Fact>]
let ``parse import with rename`` () =
    let importStx =
        list
            [ id "rename"
              list [ id "test"; id "lib" ]
              list [ id "old-name"; id "new-name" ] ]

    let diags = Feersum.CompilerServices.Diagnostics.DiagnosticBag.Empty
    let importSet = Libraries.parseImport diags importStx

    match importSet with
    | ImportSet.Renamed(ImportSet.Plain [ "test"; "lib" ], r) ->
        Assert.Equal(1, List.length r)

        match r[0] with
        | { From = "old-name"; To = "new-name" } -> ()
        | _ -> Assert.True(false, "Expected rename mapping")
    | _ -> Assert.True(false, "Expected renamed import set")
