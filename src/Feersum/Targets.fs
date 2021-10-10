namespace Targets

/// Resolved targetPaths for SDK reference assemblies.
type TargetInfo =
    { LispCoreLocation: string
    ; MSCoreLibLocations: string list }

module TargetResolve =

    // FIXME: The following two hardcoded locations should be replaced by some kind
    //        of SDK resoltuion.
    let private serehfaAssmLoc = typeof<Serehfa.ConsPair>.Assembly.Location
    let private mscorelibAssmLoc = typeof<obj>.Assembly.Location

    /// Return a `TargetInfo` for the currently running process.
    let public fromCurrentRuntime =
        { LispCoreLocation = serehfaAssmLoc
        ; MSCoreLibLocations = [ mscorelibAssmLoc ] }

    // Return a `TargetInfo` for the given MSCoreLib path.
    let public fromMsCoreLibPaths path =
        { LispCoreLocation = serehfaAssmLoc
        ; MSCoreLibLocations = path }
