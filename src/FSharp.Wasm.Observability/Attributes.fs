namespace FSharp.Wasm.Observability

open System

/// <summary>Metric type classification for observability instrumentation.</summary>
type MetricType =
    /// <summary>Monotonically increasing counter.</summary>
    | Counter = 0
    /// <summary>Point-in-time gauge value.</summary>
    | Gauge = 1
    /// <summary>Distribution of values via histogram buckets.</summary>
    | Histogram = 2
    /// <summary>Pre-computed quantile summary.</summary>
    | Summary = 3

/// <summary>Explicitly opts out a function or type from auto-instrumentation.</summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false)>]
type NoTraceAttribute() =
    inherit Attribute()

/// <summary>Marks a function or type for metric collection with the specified metric type.</summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false)>]
type MetricAttribute(metricType: MetricType) =
    inherit Attribute()

    /// <summary>Gets the metric type for this instrumentation point.</summary>
    member _.MetricType = metricType

/// <summary>Overrides the default span name for tracing instrumentation.</summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type SpanNameAttribute(name: string) =
    inherit Attribute()

    /// <summary>Gets the custom span name.</summary>
    member _.Name = name
