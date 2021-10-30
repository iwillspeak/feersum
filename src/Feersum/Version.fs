module Feersum.Version
open System.Reflection

/// Get the version string for the compiler.
let versionString =
    let assm = Assembly.GetExecutingAssembly()
    let simpleVersion = assm.GetName().Version

    let infoVersinoAttr =
        Assembly
            .GetExecutingAssembly()
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()

    match infoVersinoAttr with
    | null -> simpleVersion.ToString()
    | attr -> attr.InformationalVersion

/// Print out the compiler's version string
let printVersion () =
    printfn "Feersum Scheme Compiler - %s" versionString
