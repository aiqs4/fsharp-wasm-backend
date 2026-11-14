namespace FSharp.Wasm.Observability

open System

/// <summary>A metric measurement value tagged by its type.</summary>
type MetricValue =
    /// <summary>Monotonically increasing counter value.</summary>
    | CounterValue of int64
    /// <summary>Point-in-time gauge value.</summary>
    | GaugeValue of float
    /// <summary>Histogram of observed values.</summary>
    | HistogramValue of float array
    /// <summary>Pre-computed percentile summary.</summary>
    | SummaryValue of percentiles: Map<float, float>

/// <summary>Key-value label attached to a metric point.</summary>
type MetricLabel =
    { /// <summary>Label key.</summary>
      Key: string
      /// <summary>Label value.</summary>
      Value: string }

/// <summary>A single metric data point.</summary>
type MetricPoint =
    { /// <summary>Metric name.</summary>
      Name: string
      /// <summary>Classification of this metric.</summary>
      Type: MetricType
      /// <summary>The measured value.</summary>
      Value: MetricValue
      /// <summary>Labels identifying this metric series.</summary>
      Labels: MetricLabel list
      /// <summary>When this metric was recorded.</summary>
      Timestamp: DateTimeOffset }

/// <summary>Collects and stores metrics for counters, gauges, and histograms.</summary>
type MetricsCollector() =
    let mutable metrics: Map<string, MetricPoint> = Map.empty
    let lockObj = obj ()

    /// <summary>Increments a counter by 1.</summary>
    /// <param name="name">The counter name.</param>
    /// <param name="labels">Labels for this metric series.</param>
    member this.counter (name: string) (labels: MetricLabel list) : unit =
        this.counterBy name 1L labels

    /// <summary>Increments a counter by the specified amount.</summary>
    /// <param name="name">The counter name.</param>
    /// <param name="amount">The amount to increment by.</param>
    /// <param name="labels">Labels for this metric series.</param>
    member _.counterBy (name: string) (amount: int64) (labels: MetricLabel list) : unit =
        lock lockObj (fun () ->
            let current =
                match Map.tryFind name metrics with
                | Some point ->
                    match point.Value with
                    | CounterValue v ->
                        { point with
                            Value = CounterValue(v + amount)
                            Timestamp = DateTimeOffset.UtcNow }
                    | _ ->
                        { point with
                            Value = CounterValue amount
                            Timestamp = DateTimeOffset.UtcNow }
                | None ->
                    { Name = name
                      Type = MetricType.Counter
                      Value = CounterValue amount
                      Labels = labels
                      Timestamp = DateTimeOffset.UtcNow }

            metrics <- Map.add name current metrics)

    /// <summary>Sets a gauge to the specified value.</summary>
    /// <param name="name">The gauge name.</param>
    /// <param name="value">The gauge value.</param>
    /// <param name="labels">Labels for this metric series.</param>
    member _.gauge (name: string) (value: float) (labels: MetricLabel list) : unit =
        lock lockObj (fun () ->
            let point =
                { Name = name
                  Type = MetricType.Gauge
                  Value = GaugeValue value
                  Labels = labels
                  Timestamp = DateTimeOffset.UtcNow }

            metrics <- Map.add name point metrics)

    /// <summary>Records a histogram observation.</summary>
    /// <param name="name">The histogram name.</param>
    /// <param name="value">The observed value.</param>
    /// <param name="labels">Labels for this metric series.</param>
    member _.histogram (name: string) (value: float) (labels: MetricLabel list) : unit =
        lock lockObj (fun () ->
            let current =
                match Map.tryFind name metrics with
                | Some point ->
                    match point.Value with
                    | HistogramValue values ->
                        { point with
                            Value = HistogramValue(Array.append values [| value |])
                            Timestamp = DateTimeOffset.UtcNow }
                    | _ ->
                        { point with
                            Value = HistogramValue [| value |]
                            Timestamp = DateTimeOffset.UtcNow }
                | None ->
                    { Name = name
                      Type = MetricType.Histogram
                      Value = HistogramValue [| value |]
                      Labels = labels
                      Timestamp = DateTimeOffset.UtcNow }

            metrics <- Map.add name current metrics)

    /// <summary>Returns all collected metric points.</summary>
    member _.getMetrics () : MetricPoint list =
        lock lockObj (fun () -> metrics |> Map.toList |> List.map snd)

    /// <summary>Returns a specific metric point by name.</summary>
    /// <param name="name">The metric name to look up.</param>
    member _.getMetric (name: string) : MetricPoint option =
        lock lockObj (fun () -> Map.tryFind name metrics)

    /// <summary>Clears all collected metrics.</summary>
    member _.reset () : unit =
        lock lockObj (fun () -> metrics <- Map.empty)
