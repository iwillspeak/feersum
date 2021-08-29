module DiagnosticsTests

open Diagnostics
open Xunit

[<Fact>]
let ``create returns error`` () =
    let diag = Diagnostic.Create Missing "test message"
    Assert.True(isError diag)

[<Fact>]
let ``warnings are not errors`` () =
    let diag = Diagnostic.CreateWarning Missing "warn message"
    Assert.False(isError diag)

[<Fact>]
let ``empty diags list has no errors `` () =
    let diags: Diagnostic list = []
    Assert.False(hasErrors diags)

[<Fact>]
let ``single warning is not an error`` () =
    let diags = [ Diagnostic.CreateWarning Missing "warning" ]
    Assert.False(hasErrors diags)

[<Fact>]
let ``single error is error`` () =
    let diags = [ Diagnostic.Create Missing "an error" ]
    Assert.True(hasErrors diags)
