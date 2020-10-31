module MacroTests

open Xunit
open Macros

open SyntaxUtils
open SyntaxFactory
open Syntax

[<Fact>]
let ``patterns with constant number`` () =

    let testConstantMatch c =
        let pattern = MacroPattern.Constant c

        match macroMatch pattern (SyntaxFactory.constant c) with
        | Result.Ok _ -> ()
        | Result.Error e -> failwithf "%A" e
    
    testConstantMatch (Number 101.0)
    testConstantMatch (Number 0.0)
    testConstantMatch (Character 'a')
    testConstantMatch (Boolean true)
    testConstantMatch (Boolean false)
    testConstantMatch (Str "")
    testConstantMatch (Str "§2")

[<Fact>]
let ``variable patterns match anything`` () =

    let testVarMatch syntax =
        let pattern = MacroPattern.Variable "test"

        match macroMatch pattern syntax with
        | Result.Ok [("test", s)] ->
            Assert.Same(syntax, s)
        | o -> failwithf "Pattern variable did not match %A" o
    
    testVarMatch (Number 101.0 |> constant)
    testVarMatch (Number 0.0 |> constant)
    testVarMatch (Character 'a' |> constant)
    testVarMatch (Boolean true |> constant)
    testVarMatch (Boolean false |> constant)
    testVarMatch (Str "" |> constant)
    testVarMatch (Str "§2" |> constant)
    testVarMatch (Form [] |> node)
    testVarMatch (Ident "test" |> node)
    testVarMatch (Form [ Ident "foodafdf" |> node ] |> node)
    testVarMatch (Form [ number 123.0 ] |> node)
