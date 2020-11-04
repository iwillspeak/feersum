module MacroTests

open Xunit
open Macros

open SyntaxUtils
open SyntaxFactory
open Syntax

let private assertMatches pattern syntax =
        match macroMatch pattern syntax with
        | Result.Ok bindings -> bindings
        | o -> failwithf "Pattern variable did not match %A" o

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


[<Fact>]
let ``simple form patterns`` () =

    Assert.Equal<_ list>([], assertMatches (MacroPattern.Form []) (Form [] |> node))
    Assert.Equal<_ list>([], assertMatches (MacroPattern.Form [ MacroPattern.Constant (Boolean false); MacroPattern.Constant (Str "frob"); MacroPattern.Constant (Number 123.56)]) (Form [ constant (Boolean false); constant (Str "frob"); number 123.56] |> node))
    let testNode = (number 123.4)
    Assert.Equal<_ list>([("test", testNode)],
                         assertMatches (MacroPattern.Form [ MacroPattern.Variable "test" ]) (Form [ testNode ] |> node))
    
[<Fact>]
let ``dotted form patterns`` () =

    // FIXME: this pattern is nonsense '( . 123.4)' matching (123.4)
    Assert.Equal<_ list>([], assertMatches (MacroPattern.DottedForm([], MacroPattern.Constant (Number 123.4))) (Form [ number 123.4 ] |> node))
    let headNode = (number 123.4)
    let tailNode = (number 567.8)
    // (head . tail)
    Assert.Equal<_ list>([("tail", tailNode);("head", headNode)],
                         assertMatches (MacroPattern.DottedForm([ MacroPattern.Variable "head" ], MacroPattern.Variable "tail")) (Form [ headNode; tailNode ] |> node))