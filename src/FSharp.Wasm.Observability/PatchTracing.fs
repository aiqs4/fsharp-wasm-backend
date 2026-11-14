namespace FSharp.Wasm.Observability

open System
open FSharp.Wasm.Protocol

/// <summary>Integration between ghost patch operations and distributed tracing.</summary>
module PatchTracing =

    /// <summary>Encodes a patch with a trace_id prepended as an 8-byte header.</summary>
    /// <param name="traceId">The trace identifier to attach.</param>
    /// <param name="patch">The patch to encode.</param>
    /// <returns>A byte array with the trace_id header followed by patch data.</returns>
    let attachTraceId (traceId: TraceId) (patch: Patch) : byte array =
        let (TraceId id) = traceId
        let traceBytes = BitConverter.GetBytes(id)
        let patchBytes = GhostPatch.encode patch
        Array.append traceBytes patchBytes

    /// <summary>Extracts a trace_id from the first 8 bytes of encoded data.</summary>
    /// <param name="data">The encoded data with trace_id header.</param>
    /// <returns>The extracted trace_id and remaining data, or None if data is too short.</returns>
    let extractTraceId (data: byte array) : (TraceId * byte array) option =
        if data.Length < 8 then
            None
        else
            let id = BitConverter.ToUInt64(data, 0)
            let remaining = data.[8..]
            Some(TraceId id, remaining)

    /// <summary>Creates a tracing span for a patch operation.</summary>
    /// <param name="tracer">The tracer instance to use.</param>
    /// <param name="patchDirection">Direction descriptor (e.g. "inbound" or "outbound").</param>
    /// <param name="patch">The patch being traced.</param>
    /// <returns>A new span describing the patch operation.</returns>
    let tracePatch (tracer: Tracer) (patchDirection: string) (patch: Patch) : Span =
        let spanName = sprintf "patch.%s" patchDirection
        let span = tracer.startSpan spanName Client

        let span = tracer.addAttribute "patch.direction" patchDirection span
        let span = tracer.addAttribute "patch.ops_count" (string (List.length patch.Ops)) span

        let (TransactionId txId) = patch.TransactionId
        let span = tracer.addAttribute "patch.transaction_id" (string txId) span

        span
