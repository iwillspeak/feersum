namespace Options

/// The build configuration for the compiler.
[<Struct>]
type BuildConfiguration =
    /// Emit debugging symbols
    | Debug
    /// No symbols
    | Release

/// Output type for the compiler.
[<Struct>]
type OutputType =
    /// An executable
    | Exe
    /// Pre-compiled library
    | Lib
    /// A script or REPL item
    | Script

/// Options for each compilation
type CompilationOptions =
    { Configuration: BuildConfiguration
    ; OutputType: OutputType 
    ; References: string list }
with

    /// Create a new defaulted configuration options.
    static member Create configuration outputType =
        { Configuration = configuration
        ; OutputType = outputType
        ; References = [] }

    /// Get the default file extension for the compilation options' output type.
    member c.DefaultExtension =
        match c.OutputType with
        | Exe -> "exe"
        | _ -> "dll"

    /// Return a clone of the options with the additional references added.
    member c.WithReferences additionalReferences =
        { c with References = Seq.append c.References additionalReferences |> List.ofSeq }
