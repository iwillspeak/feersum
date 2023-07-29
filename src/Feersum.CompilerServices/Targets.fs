namespace Feersum.CompilerServices.Targets

/// Resolved targetPaths for SDK reference assemblies.
type TargetInfo =
    { LispCoreLocation: string
      FrameworkLibLocations: string list }

module TargetResolve =

    // FIXME: The following two hardcoded locations should be replaced by some kind
    //        of SDK resoltuion.
    let private serehfaAssmLoc = typeof<Serehfa.ConsPair>.Assembly.Location

    let private mscorlibAssmLoc = typeof<obj>.Assembly.Location

    /// Return a `TargetInfo` for the currently running process.
    let public fromCurrentRuntime =
        { LispCoreLocation = serehfaAssmLoc
          FrameworkLibLocations = [ mscorlibAssmLoc ] }

    /// Return a `TargetInfo` for the given framework assembly paths
    let public fromFrameworkPaths paths =
        { LispCoreLocation = serehfaAssmLoc
          FrameworkLibLocations = paths }
