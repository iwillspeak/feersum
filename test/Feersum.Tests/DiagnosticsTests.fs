module DiagnosticsTests

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Xunit

let errKind = DiagnosticKind.Create Error 123 "test diagnostic"

let warningKind = DiagnosticKind.Create Warning 456 "test diagnostic"

[<Fact>]
let ``create returns error`` () =
    let diag = Diagnostic.Create errKind Missing "test message"
    Assert.True(isError diag)

[<Fact>]
let ``warnings are not errors`` () =
    let diag = Diagnostic.Create warningKind Missing "warn message"

    Assert.False(isError diag)

[<Fact>]
let ``empty diags list has no errors `` () =
    let diags: Diagnostic list = []
    Assert.False(hasErrors diags)

[<Fact>]
let ``single warning is not an error`` () =
    let diags = [ Diagnostic.Create warningKind Missing "warning" ]

    Assert.False(hasErrors diags)

[<Fact>]
let ``single error is error`` () =
    let diags = [ Diagnostic.Create errKind Missing "an error" ]
    Assert.True(hasErrors diags)
