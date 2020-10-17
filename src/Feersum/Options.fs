module Options

/// The build configuration for the compiler.
type BuildConfiguration =
    /// Emit debugging symbols
    | Debug
    /// No symbols
    | Release