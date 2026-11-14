namespace FSharp.Wasm.Observability

open System

/// <summary>Configuration for automatic instrumentation of requests and patches.</summary>
type InstrumentationConfig =
    { /// <summary>Whether distributed tracing is enabled.</summary>
      EnableTracing: bool
      /// <summary>Whether metrics collection is enabled.</summary>
      EnableMetrics: bool
      /// <summary>Logical service name for telemetry.</summary>
      ServiceName: string
      /// <summary>Optional service version string.</summary>
      ServiceVersion: string option
      /// <summary>Default span kind for auto-instrumented operations.</summary>
      DefaultSpanKind: SpanKind }

/// <summary>Manages automatic instrumentation of requests and patch operations
/// by wrapping them with tracing spans and metrics recording.</summary>
type AutoInstrumentation(config: InstrumentationConfig, tracer: Tracer, metricsCollector: MetricsCollector) =

    /// <summary>Wraps an async operation with tracing and metrics instrumentation.</summary>
    /// <param name="requestName">The name of the request to instrument.</param>
    /// <param name="operation">The async operation to wrap.</param>
    /// <returns>The wrapped async operation producing the same result.</returns>
    member _.instrumentRequest (requestName: string) (operation: Async<'T>) : Async<'T> =
        async {
            let span =
                if config.EnableTracing then
                    let s = tracer.startSpan requestName config.DefaultSpanKind
                    let s = tracer.addAttribute "service.name" config.ServiceName s

                    match config.ServiceVersion with
                    | Some v -> Some(tracer.addAttribute "service.version" v s)
                    | None -> Some s
                else
                    None

            let startTime = DateTimeOffset.UtcNow

            try
                let! result = operation

                match span with
                | Some s ->
                    let s = tracer.setStatus SpanStatus.Ok s
                    tracer.endSpan s |> ignore
                | None -> ()

                let durationMs = (DateTimeOffset.UtcNow - startTime).TotalMilliseconds

                if config.EnableMetrics then
                    metricsCollector.counter
                        "requests.total"
                        [ { Key = "name"; Value = requestName }
                          { Key = "status"; Value = "success" } ]

                    metricsCollector.histogram
                        "requests.duration_ms"
                        durationMs
                        [ { Key = "name"; Value = requestName } ]

                return result
            with ex ->
                match span with
                | Some s ->
                    let s = tracer.setStatus (SpanStatus.Error ex.Message) s
                    tracer.endSpan s |> ignore
                | None -> ()

                let durationMs = (DateTimeOffset.UtcNow - startTime).TotalMilliseconds

                if config.EnableMetrics then
                    metricsCollector.counter
                        "requests.total"
                        [ { Key = "name"; Value = requestName }
                          { Key = "status"; Value = "error" } ]

                    metricsCollector.histogram
                        "requests.duration_ms"
                        durationMs
                        [ { Key = "name"; Value = requestName } ]

                return raise ex
        }

    /// <summary>Records metrics for a completed request.</summary>
    /// <param name="name">The request name.</param>
    /// <param name="durationMs">Duration in milliseconds.</param>
    /// <param name="success">Whether the request succeeded.</param>
    member _.recordRequest (name: string) (durationMs: float) (success: bool) : unit =
        if config.EnableMetrics then
            let status = if success then "success" else "error"

            metricsCollector.counter
                "requests.total"
                [ { Key = "name"; Value = name }
                  { Key = "status"; Value = status } ]

            metricsCollector.histogram
                "requests.duration_ms"
                durationMs
                [ { Key = "name"; Value = name } ]

    /// <summary>Records metrics for a patch operation.</summary>
    /// <param name="direction">The patch direction (e.g. "inbound", "outbound").</param>
    /// <param name="sizeBytes">The patch size in bytes.</param>
    member _.recordPatch (direction: string) (sizeBytes: int) : unit =
        if config.EnableMetrics then
            metricsCollector.counter
                "patches.total"
                [ { Key = "direction"; Value = direction } ]

            metricsCollector.histogram
                "patches.size_bytes"
                (float sizeBytes)
                [ { Key = "direction"; Value = direction } ]

    /// <summary>Returns the tracer instance.</summary>
    member _.getTracer () : Tracer = tracer

    /// <summary>Returns the metrics collector instance.</summary>
    member _.getMetrics () : MetricsCollector = metricsCollector

    /// <summary>Exports all pending telemetry data. This is a no-op placeholder
    /// that completes immediately; attach exporters at the host level.</summary>
    member _.flush () : Async<unit> = async { return () }
