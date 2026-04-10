namespace FSharp.Wasm.Sdk

open System.Text

/// <summary>Classification of where a function should run after split analysis.</summary>
type SplitTarget =
    /// <summary>Function runs only on the client.</summary>
    | ClientOnly
    /// <summary>Function runs only on the edge.</summary>
    | EdgeOnly
    /// <summary>Function is shared between client and edge.</summary>
    | Shared
    /// <summary>Split target is ambiguous and requires manual annotation.</summary>
    | Ambiguous of reason: string

/// <summary>A single entry in the split analysis report.</summary>
type SplitEntry =
    { /// <summary>Fully qualified function name.</summary>
      FunctionName: string
      /// <summary>Module containing the function.</summary>
      Module: string
      /// <summary>Resolved split target.</summary>
      Target: SplitTarget
      /// <summary>Human-readable reason for the split decision.</summary>
      Reason: string }

/// <summary>Complete split analysis report for a project.</summary>
type SplitReport =
    { /// <summary>All split entries.</summary>
      Entries: SplitEntry list
      /// <summary>Functions assigned to the client bundle.</summary>
      ClientFunctions: string list
      /// <summary>Functions assigned to the edge bundle.</summary>
      EdgeFunctions: string list
      /// <summary>Types shared between client and edge.</summary>
      SharedTypes: string list
      /// <summary>Warnings about split decisions requiring attention.</summary>
      Warnings: string list }

/// <summary>Analyzes function purity to produce a client/edge split report.
/// Uses the PurityAnalyzer to determine which functions belong where.</summary>
type SplitAnalyzer(purityAnalyzer: PurityAnalyzer) =

    /// <summary>Map a purity tag to a split target with an appropriate reason.</summary>
    let classifyFunction (info: FunctionInfo) : SplitTarget * string =
        match info.Purity with
        | PurityTag.Pure -> Shared, "Pure function, safe for both client and edge"
        | PurityTag.ClientOk -> ClientOnly, "Explicitly marked as client-safe"
        | PurityTag.EdgeOnly ->
            if info.IsHigherOrder then
                SplitTarget.EdgeOnly, "Higher-order function with edge-only classification"
            elif not (Set.isEmpty info.TaintSources) then
                let sources =
                    info.TaintSources
                    |> Set.toList
                    |> List.map (sprintf "%A")
                    |> String.concat ", "
                SplitTarget.EdgeOnly, sprintf "Tainted by: %s" sources
            else
                SplitTarget.EdgeOnly, "Classified as edge-only by purity analysis"

    /// <summary>Run split analysis on all functions registered in the purity analyzer.</summary>
    /// <returns>A complete split report.</returns>
    member _.analyze() : SplitReport =
        let report = purityAnalyzer.getReport()
        let entries =
            report
            |> List.map (fun info ->
                let target, reason = classifyFunction info
                { FunctionName = info.Name
                  Module = info.Module
                  Target = target
                  Reason = reason })

        let clientFns =
            entries
            |> List.choose (fun e ->
                match e.Target with
                | ClientOnly | Shared -> Some e.FunctionName
                | _ -> None)

        let edgeFns =
            entries
            |> List.choose (fun e ->
                match e.Target with
                | SplitTarget.EdgeOnly | Shared -> Some e.FunctionName
                | _ -> None)

        let sharedTypes =
            entries
            |> List.choose (fun e ->
                match e.Target with
                | Shared -> Some e.Module
                | _ -> None)
            |> List.distinct

        let warnings =
            entries
            |> List.choose (fun e ->
                match e.Target with
                | Ambiguous reason ->
                    Some(sprintf "Ambiguous split for '%s': %s" e.FunctionName reason)
                | _ -> None)

        { Entries = entries
          ClientFunctions = clientFns
          EdgeFunctions = edgeFns
          SharedTypes = sharedTypes
          Warnings = warnings }

    /// <summary>Serialize a split report to JSON format.</summary>
    /// <param name="report">The split report to serialize.</param>
    /// <returns>JSON string representation of the report.</returns>
    member _.toJson(report: SplitReport) : string =
        let sb = StringBuilder()
        let escapeJson (s: string) =
            s.Replace("\\", "\\\\").Replace("\"", "\\\"")

        let targetToString = function
            | ClientOnly -> "clientOnly"
            | SplitTarget.EdgeOnly -> "edgeOnly"
            | Shared -> "shared"
            | Ambiguous _ -> "ambiguous"

        sb.Append("{\n") |> ignore
        // Entries
        sb.Append("  \"entries\": [\n") |> ignore
        report.Entries
        |> List.iteri (fun i entry ->
            let comma = if i < report.Entries.Length - 1 then "," else ""
            sb.AppendFormat(
                "    {{\"functionName\": \"{0}\", \"module\": \"{1}\", \"target\": \"{2}\", \"reason\": \"{3}\"}}{4}\n",
                escapeJson entry.FunctionName,
                escapeJson entry.Module,
                targetToString entry.Target,
                escapeJson entry.Reason,
                comma) |> ignore)
        sb.Append("  ],\n") |> ignore
        // Client functions
        sb.Append("  \"clientFunctions\": [") |> ignore
        let clientStr =
            report.ClientFunctions
            |> List.map (fun s -> sprintf "\"%s\"" (escapeJson s))
            |> String.concat ", "
        sb.Append(clientStr).Append("],\n") |> ignore
        // Edge functions
        sb.Append("  \"edgeFunctions\": [") |> ignore
        let edgeStr =
            report.EdgeFunctions
            |> List.map (fun s -> sprintf "\"%s\"" (escapeJson s))
            |> String.concat ", "
        sb.Append(edgeStr).Append("],\n") |> ignore
        // Shared types
        sb.Append("  \"sharedTypes\": [") |> ignore
        let sharedStr =
            report.SharedTypes
            |> List.map (fun s -> sprintf "\"%s\"" (escapeJson s))
            |> String.concat ", "
        sb.Append(sharedStr).Append("],\n") |> ignore
        // Warnings
        sb.Append("  \"warnings\": [") |> ignore
        let warningsStr =
            report.Warnings
            |> List.map (fun s -> sprintf "\"%s\"" (escapeJson s))
            |> String.concat ", "
        sb.Append(warningsStr).Append("]\n") |> ignore
        sb.Append("}") |> ignore
        sb.ToString()

    /// <summary>Validate a split report for boundary violations.
    /// Checks that no client function depends on edge-only functions.</summary>
    /// <param name="report">The split report to validate.</param>
    /// <returns>Ok if valid, Error with list of violation messages otherwise.</returns>
    member _.validate(report: SplitReport) : Result<unit, string list> =
        let purityReport = purityAnalyzer.getReport()
        let edgeOnlySet =
            report.Entries
            |> List.choose (fun e ->
                match e.Target with
                | SplitTarget.EdgeOnly -> Some e.FunctionName
                | _ -> None)
            |> Set.ofList

        let violations =
            purityReport
            |> List.choose (fun info ->
                let entry =
                    report.Entries
                    |> List.tryFind (fun e -> e.FunctionName = info.Name)
                match entry with
                | Some e ->
                    match e.Target with
                    | ClientOnly ->
                        let edgeDeps =
                            info.CalledFunctions
                            |> Set.filter (fun callee -> Set.contains callee edgeOnlySet)
                        if Set.isEmpty edgeDeps then
                            None
                        else
                            let depList = edgeDeps |> Set.toList |> String.concat ", "
                            Some(sprintf "Client function '%s' depends on edge-only functions: %s" info.Name depList)
                    | _ -> None
                | None -> None)

        let ambiguousWarnings =
            report.Entries
            |> List.choose (fun e ->
                match e.Target with
                | Ambiguous reason ->
                    Some(sprintf "Unresolved ambiguity for '%s': %s" e.FunctionName reason)
                | _ -> None)

        let allErrors = violations @ ambiguousWarnings
        if List.isEmpty allErrors then
            Ok()
        else
            Error allErrors
