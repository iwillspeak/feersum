namespace Feersum.CompilerServices.Ice

/// The ICE exception type. This exception is thrown when control flow reaches
/// a state indicating a bug in the compiler itself.
exception public InternalCompilerErrorException of string

[<AutoOpen>]
module IceHelpers =

    /// Throw an internal compiler error with the given error message.
    let public ice message : 'a =
        raise (InternalCompilerErrorException("Internal compiler error: " + message))

    /// Throw an internal compiler error formatting the error message.
    let public icef format : 'a = Printf.ksprintf ice format

    /// Throw helper for unimplemented compiler parts.
    let public unimpl message : 'a =
        raise (System.NotImplementedException(message))
