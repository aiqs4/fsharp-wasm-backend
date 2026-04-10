namespace FSharp.Wasm.Observability

open System
open System.Threading

/// <summary>Unique identifier for a distributed trace.</summary>
[<Struct>]
type TraceId = TraceId of uint64

/// <summary>Unique identifier for a span within a trace.</summary>
[<Struct>]
type SpanId = SpanId of uint64

/// <summary>Status of a completed span.</summary>
type SpanStatus =
    /// <summary>The operation completed successfully.</summary>
    | Ok
    /// <summary>The operation failed with an error message.</summary>
    | Error of message: string
    /// <summary>Status has not been set.</summary>
    | Unset

/// <summary>Kind of span indicating the role in a distributed trace.</summary>
type SpanKind =
    /// <summary>Internal operation within a service.</summary>
    | Internal
    /// <summary>Server-side handling of a request.</summary>
    | Server
    /// <summary>Client-side request to a remote service.</summary>
    | Client
    /// <summary>Producer sending a message.</summary>
    | Producer
    /// <summary>Consumer receiving a message.</summary>
    | Consumer

/// <summary>Key-value attribute attached to a span.</summary>
type SpanAttribute =
    { /// <summary>Attribute key.</summary>
      Key: string
      /// <summary>Attribute value.</summary>
      Value: string }

/// <summary>A single span in a distributed trace.</summary>
type Span =
    { /// <summary>Trace this span belongs to.</summary>
      TraceId: TraceId
      /// <summary>Unique identifier for this span.</summary>
      SpanId: SpanId
      /// <summary>Parent span identifier, if this is a child span.</summary>
      ParentSpanId: SpanId option
      /// <summary>Human-readable name of the operation.</summary>
      Name: string
      /// <summary>Role of this span in the trace.</summary>
      Kind: SpanKind
      /// <summary>Completion status of the span.</summary>
      Status: SpanStatus
      /// <summary>When the span started.</summary>
      StartTime: DateTimeOffset
      /// <summary>When the span ended, if completed.</summary>
      EndTime: DateTimeOffset option
      /// <summary>Key-value attributes attached to this span.</summary>
      Attributes: SpanAttribute list }

/// <summary>Creates and manages spans for distributed tracing.</summary>
type Tracer() =
    let mutable idCounter = 0L
    let mutable activeSpans: Span list = []
    let mutable completedSpans: Span list = []
    let lockObj = obj ()

    let nextId () =
        uint64 (Interlocked.Increment(&idCounter))

    /// <summary>Starts a new root span with the given name and kind.</summary>
    /// <param name="name">The operation name for the span.</param>
    /// <param name="kind">The span kind indicating its role.</param>
    /// <returns>A new active span.</returns>
    member _.startSpan (name: string) (kind: SpanKind) : Span =
        let span =
            { TraceId = TraceId(nextId ())
              SpanId = SpanId(nextId ())
              ParentSpanId = None
              Name = name
              Kind = kind
              Status = Unset
              StartTime = DateTimeOffset.UtcNow
              EndTime = None
              Attributes = [] }

        lock lockObj (fun () -> activeSpans <- span :: activeSpans)
        span

    /// <summary>Starts a child span under the given parent span.</summary>
    /// <param name="parent">The parent span.</param>
    /// <param name="name">The operation name for the child span.</param>
    /// <param name="kind">The span kind indicating its role.</param>
    /// <returns>A new child span sharing the parent's trace ID.</returns>
    member _.startChildSpan (parent: Span) (name: string) (kind: SpanKind) : Span =
        let span =
            { TraceId = parent.TraceId
              SpanId = SpanId(nextId ())
              ParentSpanId = Some parent.SpanId
              Name = name
              Kind = kind
              Status = Unset
              StartTime = DateTimeOffset.UtcNow
              EndTime = None
              Attributes = [] }

        lock lockObj (fun () -> activeSpans <- span :: activeSpans)
        span

    /// <summary>Ends a span by setting its EndTime and moving it to the completed list.</summary>
    /// <param name="span">The span to end.</param>
    /// <returns>The ended span with EndTime set.</returns>
    member _.endSpan (span: Span) : Span =
        let ended = { span with EndTime = Some DateTimeOffset.UtcNow }

        lock lockObj (fun () ->
            activeSpans <- activeSpans |> List.filter (fun s -> s.SpanId <> span.SpanId)
            completedSpans <- ended :: completedSpans)

        ended

    /// <summary>Sets the status on a span.</summary>
    /// <param name="status">The status to set.</param>
    /// <param name="span">The span to update.</param>
    /// <returns>The span with updated status.</returns>
    member _.setStatus (status: SpanStatus) (span: Span) : Span =
        let updated = { span with Status = status }

        lock lockObj (fun () ->
            activeSpans <-
                activeSpans
                |> List.map (fun s -> if s.SpanId = span.SpanId then updated else s))

        updated

    /// <summary>Adds a key-value attribute to a span.</summary>
    /// <param name="key">The attribute key.</param>
    /// <param name="value">The attribute value.</param>
    /// <param name="span">The span to update.</param>
    /// <returns>The span with the new attribute appended.</returns>
    member _.addAttribute (key: string) (value: string) (span: Span) : Span =
        let attr = { Key = key; Value = value }
        let updated = { span with Attributes = span.Attributes @ [ attr ] }

        lock lockObj (fun () ->
            activeSpans <-
                activeSpans
                |> List.map (fun s -> if s.SpanId = span.SpanId then updated else s))

        updated

    /// <summary>Returns all currently active (unfinished) spans.</summary>
    member _.getActiveSpans () : Span list =
        lock lockObj (fun () -> activeSpans)

    /// <summary>Returns all completed spans.</summary>
    member _.getCompletedSpans () : Span list =
        lock lockObj (fun () -> completedSpans)

    /// <summary>Clears all active and completed spans.</summary>
    member _.clear () : unit =
        lock lockObj (fun () ->
            activeSpans <- []
            completedSpans <- [])
