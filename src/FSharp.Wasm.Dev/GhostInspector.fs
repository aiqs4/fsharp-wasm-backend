namespace FSharp.Wasm.Dev

open System
open System.Collections.Generic
open System.Text
open FSharp.Wasm.Ghost
open FSharp.Wasm.Protocol

/// Summary view of a single ghost node for inspection.
type NodeView =
    { /// Node identifier.
      Id: int
      /// Current version of the node.
      Version: int
      /// Dirty state description.
      Dirty: string
      /// Summary of the node's data.
      DataSummary: string
      /// Number of dependencies this node has.
      DependencyCount: int }

/// A log entry recording a patch that was sent or received.
type PatchLogEntry =
    { /// When the patch was logged.
      Timestamp: DateTimeOffset
      /// Direction of the patch (e.g., "outbound", "inbound").
      Direction: string
      /// The patch itself.
      Patch: Patch
      /// Size of the serialized patch in bytes.
      SizeBytes: int }

/// Snapshot of the inspector's state for display in dev tools.
type InspectorState =
    { /// Views of all tracked nodes.
      Nodes: NodeView list
      /// Recent patch log entries.
      PatchLog: PatchLogEntry list
      /// Total number of registered nodes.
      TotalNodes: int
      /// Number of nodes with dirty fields.
      DirtyNodes: int
      /// Number of pending (uncommitted) transactions.
      PendingTransactions: int }

/// Interactive inspector for ghost/shadow state during development.
type GhostInspector(manager: ShadowManager) =
    let patchLog = List<PatchLogEntry>()
    let lockObj = obj ()

    let buildNodeView (node: GhostNode) : NodeView =
        let dirtyStr =
            match node.Dirty with
            | DirtyState.Clean -> "clean"
            | DirtyState.Pending -> "pending"
            | DirtyState.Committed -> "committed"
            | _ -> "unknown"

        let dataSummary =
            match node.Data with
            | null -> "<null>"
            | d -> d.ToString() |> fun s -> if s.Length > 80 then s.Substring(0, 80) + "..." else s

        { Id = node.Id
          Version = node.Version
          Dirty = dirtyStr
          DataSummary = dataSummary
          DependencyCount = 0 }

    /// Get a full snapshot of the current inspector state.
    member _.getState() : InspectorState =
        let allNodes = manager.getAllNodes () |> Seq.toList
        let nodeViews = allNodes |> List.map buildNodeView

        let dirtyCount =
            allNodes
            |> List.filter (fun n -> n.Dirty <> DirtyState.Clean)
            |> List.length

        let logSnapshot = lock lockObj (fun () -> patchLog |> Seq.toList)

        { Nodes = nodeViews
          PatchLog = logSnapshot
          TotalNodes = allNodes.Length
          DirtyNodes = dirtyCount
          PendingTransactions = 0 }

    /// Get a view of a single node by ID.
    member _.getNodeView(nodeId: int) : NodeView option =
        manager.getNode nodeId
        |> Option.map buildNodeView

    /// Log a patch event with direction ("inbound" or "outbound").
    member _.logPatch (direction: string) (patch: Patch) : unit =
        let entry =
            { Timestamp = DateTimeOffset.UtcNow
              Direction = direction
              Patch = patch
              SizeBytes = patch.Ops.Length * 8 }

        lock lockObj (fun () -> patchLog.Add(entry))

    /// Get the most recent patch log entries, up to count.
    member _.getPatchLog(count: int) : PatchLogEntry list =
        lock lockObj (fun () ->
            patchLog
            |> Seq.toList
            |> List.rev
            |> List.truncate count)

    /// Clear all patch log entries.
    member _.clearPatchLog() : unit =
        lock lockObj (fun () -> patchLog.Clear())

    /// Get all patch log entries in chronological order.
    member _.getTimeline() : PatchLogEntry list =
        lock lockObj (fun () -> patchLog |> Seq.toList)

    /// Replay patches in a given index range (time-travel debugging).
    member _.replayPatches (fromIndex: int) (toIndex: int) : unit =
        let entries =
            lock lockObj (fun () ->
                patchLog
                |> Seq.toList
                |> List.skip fromIndex
                |> List.take (toIndex - fromIndex + 1))

        for entry in entries do
            manager.applyPatch entry.Patch

    /// Export the current inspector state as a JSON string for debugging.
    member this.exportState() : string =
        let state = this.getState()
        let sb = StringBuilder()
        sb.AppendLine("{") |> ignore
        sb.AppendLine(sprintf "  \"totalNodes\": %d," state.TotalNodes) |> ignore
        sb.AppendLine(sprintf "  \"dirtyNodes\": %d," state.DirtyNodes) |> ignore
        sb.AppendLine(sprintf "  \"pendingTransactions\": %d," state.PendingTransactions) |> ignore
        sb.AppendLine("  \"nodes\": [") |> ignore

        let nodeStrs =
            state.Nodes
            |> List.map (fun n ->
                sprintf "    {\"id\": %d, \"version\": %d, \"dirty\": \"%s\", \"dataSummary\": \"%s\", \"dependencyCount\": %d}"
                    n.Id n.Version (n.Dirty.Replace("\"", "\\\"")) (n.DataSummary.Replace("\"", "\\\"")) n.DependencyCount)

        sb.AppendLine(String.concat ",\n" nodeStrs) |> ignore
        sb.AppendLine("  ],") |> ignore
        sb.AppendLine(sprintf "  \"patchLogCount\": %d" state.PatchLog.Length) |> ignore
        sb.AppendLine("}") |> ignore
        sb.ToString()
