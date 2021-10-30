module OptionsTests

open Xunit

open Feersum.CompilerServices.Options

[<Fact>]
let ``default extensions for output type`` () =
    let checkOutputType outputType expected =
        let options = CompilationOptions.Create Debug outputType
        let extension = options.DefaultExtension
        Assert.Equal(expected, extension)
    
    checkOutputType OutputType.Exe "exe"
    checkOutputType OutputType.Lib "dll"
    checkOutputType OutputType.Script "dll"
