namespace FSharp.Wasm.Dev

open System
open System.Diagnostics
open System.Text

/// Result of running a single benchmark.
type BenchmarkResult =
    { /// Name of the benchmark.
      Name: string
      /// Number of iterations run.
      Iterations: int
      /// Total elapsed time in milliseconds.
      TotalMs: float
      /// Average time per iteration in milliseconds.
      AvgMs: float
      /// Minimum iteration time in milliseconds.
      MinMs: float
      /// Maximum iteration time in milliseconds.
      MaxMs: float
      /// Median iteration time in milliseconds.
      MedianMs: float
      /// Operations per second.
      OpsPerSec: float }

/// A suite of benchmark results.
type BenchmarkSuite =
    { /// Name of the suite.
      Name: string
      /// Individual benchmark results.
      Results: BenchmarkResult list
      /// When the suite was run.
      RunDate: DateTimeOffset }

/// Benchmark harness for measuring performance.
module Benchmark =

    let private computeStats (name: string) (iterations: int) (timings: float array) =
        let sorted = timings |> Array.sort
        let total = timings |> Array.sum
        let median =
            if sorted.Length % 2 = 0 then
                (sorted.[sorted.Length / 2 - 1] + sorted.[sorted.Length / 2]) / 2.0
            else
                sorted.[sorted.Length / 2]

        { Name = name
          Iterations = iterations
          TotalMs = total
          AvgMs = total / float iterations
          MinMs = sorted.[0]
          MaxMs = sorted.[sorted.Length - 1]
          MedianMs = median
          OpsPerSec = if total > 0.0 then float iterations / (total / 1000.0) else 0.0 }

    /// Run a synchronous benchmark for the given number of iterations.
    let run (name: string) (iterations: int) (action: unit -> unit) : BenchmarkResult =
        // Warmup
        action ()

        let timings = Array.zeroCreate iterations
        let sw = Stopwatch()

        for i in 0 .. iterations - 1 do
            sw.Restart()
            action ()
            sw.Stop()
            timings.[i] <- sw.Elapsed.TotalMilliseconds

        computeStats name iterations timings

    /// Run an asynchronous benchmark for the given number of iterations.
    let runAsync (name: string) (iterations: int) (action: unit -> Async<unit>) : Async<BenchmarkResult> =
        async {
            // Warmup
            do! action ()

            let timings = Array.zeroCreate iterations
            let sw = Stopwatch()

            for i in 0 .. iterations - 1 do
                sw.Restart()
                do! action ()
                sw.Stop()
                timings.[i] <- sw.Elapsed.TotalMilliseconds

            return computeStats name iterations timings
        }

    /// Pretty-print a single benchmark result.
    let formatResult (result: BenchmarkResult) : string =
        sprintf "%s: %d iterations, avg=%.3fms, min=%.3fms, max=%.3fms, median=%.3fms, %.1f ops/sec"
            result.Name result.Iterations result.AvgMs result.MinMs result.MaxMs result.MedianMs result.OpsPerSec

    /// Pretty-print a benchmark suite as a table.
    let formatSuite (suite: BenchmarkSuite) : string =
        let sb = StringBuilder()
        sb.AppendLine(sprintf "Benchmark Suite: %s" suite.Name) |> ignore
        sb.AppendLine(sprintf "Run Date: %s" (suite.RunDate.ToString("o"))) |> ignore
        sb.AppendLine(String.replicate 90 "─") |> ignore
        sb.AppendLine(sprintf "%-25s %10s %10s %10s %10s %12s" "Name" "Avg(ms)" "Min(ms)" "Max(ms)" "Med(ms)" "Ops/sec") |> ignore
        sb.AppendLine(String.replicate 90 "─") |> ignore

        for r in suite.Results do
            sb.AppendLine(sprintf "%-25s %10.3f %10.3f %10.3f %10.3f %12.1f"
                r.Name r.AvgMs r.MinMs r.MaxMs r.MedianMs r.OpsPerSec) |> ignore

        sb.AppendLine(String.replicate 90 "─") |> ignore
        sb.ToString()

    /// Compare two benchmark results (baseline vs candidate).
    let compare (baseline: BenchmarkResult) (candidate: BenchmarkResult) : string =
        let ratio = candidate.AvgMs / baseline.AvgMs
        let pctChange = (ratio - 1.0) * 100.0
        let indicator = if pctChange < 0.0 then "faster ✓" elif pctChange > 0.0 then "slower ✗" else "same"

        sprintf "Comparing '%s' vs '%s': %.3fms → %.3fms (%.1f%% %s)"
            baseline.Name candidate.Name baseline.AvgMs candidate.AvgMs (abs pctChange) indicator
