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
    
    testConstantMatch (SyntaxConstant.Number 101.0)
    testConstantMatch (SyntaxConstant.Number 0.0)
    testConstantMatch (SyntaxConstant.Character 'a')
    testConstantMatch (SyntaxConstant.Boolean true)
    testConstantMatch (SyntaxConstant.Boolean false)
    testConstantMatch (SyntaxConstant.Str "")
    testConstantMatch (SyntaxConstant.Str "§2")

