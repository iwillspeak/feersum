module Feersum.Telemetry

open System.Diagnostics.Metrics
open System.Collections.Generic
open System.Diagnostics

/// Emit Trace Information to a Given Text Writer (e.g. Console.Out)
///
/// This can be used for simple tracing without needing to set up an ActivityListener. It is
/// intended to be used by the CLI args `--trace` or `--trace <path>`.
///
/// Returns a disposable handle that should be disposed to stop tracing.
let traceTo (write: string -> unit) : System.IDisposable =

    let formatTags (tags: KeyValuePair<string, 'a> seq) : string =
        tags
        |> Seq.map (fun kvp -> sprintf "%s=%O" kvp.Key kvp.Value)
        |> String.concat ","

    let activityListener = new ActivityListener()
    activityListener.ShouldListenTo <- fun source -> source.Name = "Feersum.CompilerServices"
    activityListener.Sample <- fun _ -> ActivitySamplingResult.AllDataAndRecorded

    activityListener.ActivityStarted <-
        fun (activity: Activity) ->
            activity.Tags
            |> formatTags
            |> sprintf "Activity started: %s (%s)" activity.OperationName
            |> write

    activityListener.ActivityStopped <-
        fun (activity: Activity) ->
            activity.Tags
            |> formatTags
            |> sprintf "Activity stopped: %s %O (%s)" activity.OperationName activity.Duration
            |> write

    ActivitySource.AddActivityListener activityListener

    let listener = new MeterListener()

    listener.InstrumentPublished <-
        (fun (instrument: Instrument) (listener: MeterListener) ->
            if instrument.Meter.Name = "Feersum.CompilerServices" then
                listener.EnableMeasurementEvents(instrument, null))

    listener.SetMeasurementEventCallback<int64>(fun instrument measurement tags _ ->
        let tagStr = formatTags (tags.ToArray())
        write (sprintf "Measurement: %s=%O (%s)" instrument.Name measurement tagStr))

    listener.SetMeasurementEventCallback<float>(fun instrument measurement tags _ ->
        let tagStr = formatTags (tags.ToArray())
        write (sprintf "Measurement: %s=%f (%s)" instrument.Name measurement tagStr))

    listener.Start()

    { new System.IDisposable with
        member _.Dispose() =
            activityListener.Dispose()
            listener.Dispose() }
