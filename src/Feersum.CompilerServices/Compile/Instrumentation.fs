module internal Feersum.CompilerServices.Compile.Instrumentation

open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Metrics

/// The source name used for all Feersum compiler instrumentation.
///
/// This name can be used to subscribe to compiler telemetry:
/// * With `dotnet-counters monitor --counters Feersum.CompilerServices`
/// * Via `ActivityListener` for tracing
[<Literal>]
let SourceName = "Feersum.CompilerServices"

/// Meter used to record Feersum compiler metrics.
///
/// Metrics from this meter can be observed with:
///   dotnet-counters monitor --counters Feersum.CompilerServices
let private meter = new Meter(SourceName)

/// Activity source used to create tracing spans for compiler passes.
let activitySource = new ActivitySource(SourceName)

/// Records the number of compilations performed.
let compilationCount =
    meter.CreateCounter<int64>("feersum.compilations", description = "Number of compilations performed")

/// Records the number of errors produced during compilation.
let compilationErrors =
    meter.CreateCounter<int64>(
        "feersum.compilation.errors",
        description = "Number of errors produced during compilation"
    )

/// Records the duration of individual compiler passes in milliseconds.
let phaseDuration =
    meter.CreateHistogram<float>(
        "feersum.compilation.phase.duration",
        unit = "ms",
        description = "Duration of compiler passes in milliseconds"
    )

/// Run a compiler pass, recording its duration and creating a tracing span.
///
/// The `phase` tag is used to distinguish between different compiler passes
/// such as `bind`, `lower`, and `emit`. The pass function `f` takes `x` as
/// input, enabling pipeline-style usage with `|>`.
let inline withPhase (phase: string) (f: 'a -> 'b) (x: 'a) : 'b =
    use _ = activitySource.StartActivity(phase)
    let startTimestamp = Stopwatch.GetTimestamp()
    let result = f x
    let elapsed = Stopwatch.GetElapsedTime(startTimestamp)
    phaseDuration.Record(elapsed.TotalMilliseconds, KeyValuePair("phase", phase :> obj))
    result
