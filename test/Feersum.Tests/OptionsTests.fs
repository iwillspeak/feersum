module OptionsTests

open Xunit

open Options

[<Fact>]
let ``default extensions for output type`` () =
    let checkOutputType outputType expected =
        let options =
            { Configuration = Debug
            ; OutputType = outputType
            ; References = [] }
        let extension = getDefaultExtension options
        Assert.Equal(expected, extension)
    
    checkOutputType OutputType.Exe "exe"
    checkOutputType OutputType.Lib "dll"
    checkOutputType OutputType.Script "dll"
