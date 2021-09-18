module Options

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

/// Get the default file extension for the compilation options' output type.
let public getDefaultExtension options =
    match options.OutputType with
    | Exe -> "exe"
    | _ -> "dll"