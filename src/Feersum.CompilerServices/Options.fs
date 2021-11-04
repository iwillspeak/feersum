namespace Feersum.CompilerServices

open System

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
      OutputType: OutputType
      Version: Version option
      MsCorePaths: string list
      GenerateDepsFiles: bool
      References: string list }

    /// Create a new defaulted configuration options.
    static member Create configuration outputType =
        { Configuration = configuration
          OutputType = outputType
          Version = None
          MsCorePaths = []
          GenerateDepsFiles = false
          References = [] }

    /// Get the default file extension for the compilation options' output type.
    member c.DefaultExtension =
        match c.OutputType with
        | Exe -> "exe"
        | _ -> "dll"
