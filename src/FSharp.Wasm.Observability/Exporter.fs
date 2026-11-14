namespace FSharp.Wasm.Observability

open System
open System.Text

/// <summary>Supported export formats for telemetry data.</summary>
type ExportFormat =
    /// <summary>OpenTelemetry Protocol JSON encoding.</summary>
    | OtlpJson
    /// <summary>OpenTelemetry Protocol Protobuf encoding.</summary>
    | OtlpProtobuf
    /// <summary>Prometheus text exposition format.</summary>
    | Prometheus
    /// <summary>Human-readable console output for development.</summary>
    | Console

/// <summary>Configuration for telemetry data exporters.</summary>
type ExporterConfig =
    { /// <summary>Target endpoint URL for exported data.</summary>
      Endpoint: string
      /// <summary>Wire format for the export.</summary>
      Format: ExportFormat
      /// <summary>Additional HTTP headers for the export request.</summary>
      Headers: Map<string, string>
      /// <summary>Number of items to batch before exporting.</summary>
      BatchSize: int
      /// <summary>Interval in milliseconds between automatic flushes.</summary>
      FlushIntervalMs: int }

/// <summary>Interface for exporting metric data points.</summary>
type IMetricExporter =
    /// <summary>Exports a batch of metric points.</summary>
    abstract Export: MetricPoint list -> Async<Result<unit, string>>

/// <summary>Interface for exporting trace spans.</summary>
type ISpanExporter =
    /// <summary>Exports a batch of spans.</summary>
    abstract Export: Span list -> Async<Result<unit, string>>

/// <summary>Formats a SpanStatus for display.</summary>
[<AutoOpen>]
module private ExporterHelpers =
    let formatSpanStatus (status: SpanStatus) =
        match status with
        | SpanStatus.Ok -> "OK"
        | SpanStatus.Error msg -> sprintf "ERROR: %s" msg
        | Unset -> "UNSET"

    let formatSpanKind (kind: SpanKind) =
        match kind with
        | Internal -> "INTERNAL"
        | Server -> "SERVER"
        | Client -> "CLIENT"
        | Producer -> "PRODUCER"
        | Consumer -> "CONSUMER"

    let formatMetricValue (value: MetricValue) =
        match value with
        | CounterValue v -> sprintf "%d" v
        | GaugeValue v -> sprintf "%.4f" v
        | HistogramValue vs -> sprintf "[%s]" (vs |> Array.map (sprintf "%.4f") |> String.concat ", ")
        | SummaryValue ps ->
            ps
            |> Map.toList
            |> List.map (fun (k, v) -> sprintf "p%.0f=%.4f" (k * 100.0) v)
            |> String.concat ", "
            |> sprintf "{%s}"

    let escapeJsonString (s: string) =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")

/// <summary>Exports telemetry data to the console for development and debugging.</summary>
type ConsoleExporter() =

    interface IMetricExporter with
        /// <summary>Prints metric points to the console.</summary>
        member _.Export(metrics: MetricPoint list) =
            async {
                for m in metrics do
                    let labels =
                        m.Labels
                        |> List.map (fun l -> sprintf "%s=%s" l.Key l.Value)
                        |> String.concat ", "

                    printfn "[METRIC] %s (%A) = %s {%s} @ %O"
                        m.Name m.Type (formatMetricValue m.Value) labels m.Timestamp

                return Result.Ok()
            }

    interface ISpanExporter with
        /// <summary>Prints spans to the console.</summary>
        member _.Export(spans: Span list) =
            async {
                for s in spans do
                    let (TraceId tid) = s.TraceId
                    let (SpanId sid) = s.SpanId

                    let parentStr =
                        match s.ParentSpanId with
                        | Some(SpanId pid) -> sprintf " parent=%d" pid
                        | None -> ""

                    let attrs =
                        s.Attributes
                        |> List.map (fun a -> sprintf "%s=%s" a.Key a.Value)
                        |> String.concat ", "

                    let duration =
                        match s.EndTime with
                        | Some endTime -> sprintf "%.2fms" (endTime - s.StartTime).TotalMilliseconds
                        | None -> "active"

                    printfn "[SPAN] %s trace=%d span=%d%s kind=%s status=%s duration=%s {%s}"
                        s.Name tid sid parentStr (formatSpanKind s.Kind)
                        (formatSpanStatus s.Status) duration attrs

                return Result.Ok()
            }

/// <summary>Formats telemetry data as OTLP JSON and stores it for retrieval.
/// Actual HTTP sending requires host integration.</summary>
type OtlpExporter(config: ExporterConfig) =
    let mutable exportedSpanJson: string list = []
    let mutable exportedMetricJson: string list = []
    let lockObj = obj ()

    let spanToJson (s: Span) =
        let (TraceId tid) = s.TraceId
        let (SpanId sid) = s.SpanId
        let sb = StringBuilder()
        sb.Append("{") |> ignore
        sb.AppendFormat("\"traceId\":\"{0}\",", tid) |> ignore
        sb.AppendFormat("\"spanId\":\"{0}\",", sid) |> ignore

        match s.ParentSpanId with
        | Some(SpanId pid) -> sb.AppendFormat("\"parentSpanId\":\"{0}\",", pid) |> ignore
        | None -> ()

        sb.AppendFormat("\"name\":\"{0}\",", escapeJsonString s.Name) |> ignore
        sb.AppendFormat("\"kind\":\"{0}\",", formatSpanKind s.Kind) |> ignore
        sb.AppendFormat("\"status\":{{\"code\":\"{0}\"}},", formatSpanStatus s.Status) |> ignore
        sb.AppendFormat("\"startTimeUnixNano\":\"{0}\",", s.StartTime.ToUnixTimeMilliseconds() * 1_000_000L) |> ignore

        match s.EndTime with
        | Some endTime ->
            sb.AppendFormat("\"endTimeUnixNano\":\"{0}\",", endTime.ToUnixTimeMilliseconds() * 1_000_000L) |> ignore
        | None -> ()

        let attrsJson =
            s.Attributes
            |> List.map (fun a ->
                sprintf "{\"key\":\"%s\",\"value\":{\"stringValue\":\"%s\"}}"
                    (escapeJsonString a.Key) (escapeJsonString a.Value))
            |> String.concat ","

        sb.AppendFormat("\"attributes\":[{0}]", attrsJson) |> ignore
        sb.Append("}") |> ignore
        sb.ToString()

    let metricToJson (m: MetricPoint) =
        let sb = StringBuilder()
        sb.Append("{") |> ignore
        sb.AppendFormat("\"name\":\"{0}\",", escapeJsonString m.Name) |> ignore
        sb.AppendFormat("\"type\":\"{0}\",", m.Type) |> ignore

        match m.Value with
        | CounterValue v -> sb.AppendFormat("\"sum\":{{\"dataPoints\":[{{\"asInt\":\"{0}\"}}]}},", v) |> ignore
        | GaugeValue v -> sb.AppendFormat("\"gauge\":{{\"dataPoints\":[{{\"asDouble\":{0}}}]}},", v) |> ignore
        | HistogramValue vs ->
            let points = vs |> Array.map (sprintf "%f") |> String.concat ","
            sb.AppendFormat("\"histogram\":{{\"dataPoints\":[{{\"explicitBounds\":[{0}]}}]}},", points) |> ignore
        | SummaryValue ps ->
            let quantiles =
                ps
                |> Map.toList
                |> List.map (fun (q, v) -> sprintf "{\"quantile\":%f,\"value\":%f}" q v)
                |> String.concat ","
            sb.AppendFormat("\"summary\":{{\"dataPoints\":[{{\"quantileValues\":[{0}]}}]}},", quantiles) |> ignore

        let labelsJson =
            m.Labels
            |> List.map (fun l ->
                sprintf "{\"key\":\"%s\",\"value\":{\"stringValue\":\"%s\"}}"
                    (escapeJsonString l.Key) (escapeJsonString l.Value))
            |> String.concat ","

        sb.AppendFormat("\"attributes\":[{0}]", labelsJson) |> ignore
        sb.Append("}") |> ignore
        sb.ToString()

    /// <summary>Returns all exported span JSON strings.</summary>
    member _.GetExportedSpans() =
        lock lockObj (fun () -> exportedSpanJson |> List.rev)

    /// <summary>Returns all exported metric JSON strings.</summary>
    member _.GetExportedMetrics() =
        lock lockObj (fun () -> exportedMetricJson |> List.rev)

    /// <summary>Returns the exporter configuration.</summary>
    member _.Config = config

    interface ISpanExporter with
        member _.Export(spans: Span list) =
            async {
                let jsonEntries = spans |> List.map spanToJson
                lock lockObj (fun () -> exportedSpanJson <- jsonEntries @ exportedSpanJson)
                return Result.Ok()
            }

    interface IMetricExporter with
        member _.Export(metrics: MetricPoint list) =
            async {
                let jsonEntries = metrics |> List.map metricToJson
                lock lockObj (fun () -> exportedMetricJson <- jsonEntries @ exportedMetricJson)
                return Result.Ok()
            }
