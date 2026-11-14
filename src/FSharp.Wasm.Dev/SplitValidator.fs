namespace FSharp.Wasm.Dev

open System
open System.Text
open FSharp.Wasm.Sdk

/// Severity level of a split validation entry.
type ValidationLevel =
    | Ok
    | Warning
    | Error

/// A single validation finding for a function or type.
type ValidationEntry =
    { /// Name of the function being validated.
      FunctionName: string
      /// Target platform (e.g., "client", "edge").
      Target: string
      /// Severity level.
      Level: ValidationLevel
      /// Human-readable message.
      Message: string }

/// Aggregated split validation report.
type SplitValidationReport =
    { /// All validation entries.
      Entries: ValidationEntry list
      /// Count of error-level entries.
      ErrorCount: int
      /// Count of warning-level entries.
      WarningCount: int
      /// Count of ok-level entries.
      OkCount: int }

/// Validates split analysis reports and produces human-readable output.
module SplitValidator =

    /// Validate a SplitReport and produce a SplitValidationReport.
    let validate (report: SplitReport) : SplitValidationReport =
        let entries =
            report.Entries
            |> List.collect (fun entry ->
                let target =
                    if report.ClientFunctions |> List.contains entry.FunctionName then "client"
                    elif report.EdgeFunctions |> List.contains entry.FunctionName then "edge"
                    else "shared"

                let baseEntry =
                    { FunctionName = entry.FunctionName
                      Target = target
                      Level = Ok
                      Message = sprintf "Function '%s' correctly assigned to %s" entry.FunctionName target }

                let warningEntries =
                    if target = "shared" then
                        [ { baseEntry with
                              Level = Warning
                              Message = sprintf "Function '%s' is shared — verify it doesn't contain platform-specific code" entry.FunctionName } ]
                    else
                        []

                baseEntry :: warningEntries)

        let warningEntries =
            report.Warnings
            |> List.map (fun w ->
                { FunctionName = ""
                  Target = ""
                  Level = Warning
                  Message = w })

        let allEntries = entries @ warningEntries

        { Entries = allEntries
          ErrorCount = allEntries |> List.filter (fun e -> e.Level = Error) |> List.length
          WarningCount = allEntries |> List.filter (fun e -> e.Level = Warning) |> List.length
          OkCount = allEntries |> List.filter (fun e -> e.Level = Ok) |> List.length }

    /// Pretty-print a SplitValidationReport using ✓/⚠/✗ symbols.
    let formatReport (report: SplitValidationReport) : string =
        let sb = StringBuilder()
        sb.AppendLine("Split Validation Report") |> ignore
        sb.AppendLine(String.replicate 40 "─") |> ignore

        for entry in report.Entries do
            let symbol =
                match entry.Level with
                | Ok -> "✓"
                | Warning -> "⚠"
                | Error -> "✗"

            let line =
                if String.IsNullOrEmpty entry.FunctionName then
                    sprintf "%s  %s" symbol entry.Message
                else
                    sprintf "%s  [%s → %s] %s" symbol entry.FunctionName entry.Target entry.Message

            sb.AppendLine(line) |> ignore

        sb.AppendLine(String.replicate 40 "─") |> ignore
        sb.AppendLine(sprintf "✓ %d ok  ⚠ %d warnings  ✗ %d errors" report.OkCount report.WarningCount report.ErrorCount) |> ignore
        sb.ToString()

    /// Returns true if the report contains any error-level entries.
    let hasErrors (report: SplitValidationReport) : bool =
        report.ErrorCount > 0
