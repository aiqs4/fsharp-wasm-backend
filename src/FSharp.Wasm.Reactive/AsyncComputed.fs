namespace FSharp.Wasm.Reactive

open System.Collections.Generic
open System.Threading
open FSharp.Wasm.Ghost

/// <summary>Represents the lifecycle state of an asynchronous computed value.</summary>
type AsyncComputedState<'T> =
    /// <summary>The async computation has not been started.</summary>
    | NotStarted
    /// <summary>The computation is in progress.</summary>
    | Loading
    /// <summary>The computation completed successfully with a value.</summary>
    | Loaded of 'T
    /// <summary>A streaming computation has produced a value and may produce more.</summary>
    | Streaming of 'T
    /// <summary>The computation failed with an exception.</summary>
    | Failed of exn

/// <summary>
/// A node tracking an asynchronous computation, including its current
/// state and an optional cancellation token for cooperative cancellation.
/// </summary>
type AsyncComputedNode<'T> =
    { /// <summary>The signal ID in the reactive graph for this async value.</summary>
      SignalId: int
      /// <summary>Current lifecycle state of the computation.</summary>
      State: AsyncComputedState<'T>
      /// <summary>Cancellation token source for cooperative cancellation, if active.</summary>
      CancellationToken: CancellationTokenSource option }

/// <summary>
/// Internal representation stored in the engine, holding the untyped async
/// computation and streaming source alongside the node state.
/// </summary>
type internal AsyncComputedEntry =
    { /// <summary>The signal ID in the reactive graph.</summary>
      SignalId: int
      /// <summary>Current lifecycle state (untyped).</summary>
      State: AsyncComputedState<obj>
      /// <summary>Cancellation token source for cooperative cancellation.</summary>
      CancellationToken: CancellationTokenSource option
      /// <summary>The async computation function, if this is a one-shot async node.</summary>
      AsyncCompute: (CancellationToken -> Async<obj>) option
      /// <summary>The streaming source function, if this is a streaming async node.</summary>
      StreamingSource: (CancellationToken -> IAsyncEnumerable<obj>) option }

/// <summary>
/// Engine that manages asynchronous computed values. Supports both one-shot
/// async computations and streaming <see cref="IAsyncEnumerable{'T}"/> sources.
/// Integrates with the <see cref="ComputedEngine"/> for signal tracking.
/// </summary>
type AsyncComputedEngine(shadowManager: ShadowManager, computedEngine: ComputedEngine) =

    let entries = Dictionary<int, AsyncComputedEntry>()

    /// <summary>The shadow manager this engine is associated with.</summary>
    member _.ShadowManager = shadowManager

    /// <summary>The computed engine used for signal graph integration.</summary>
    member _.ComputedEngine = computedEngine

    /// <summary>Total number of registered async computed nodes.</summary>
    member _.Count = entries.Count

    /// <summary>
    /// Register a one-shot async computed value. Creates a signal in the graph
    /// and stores the computation for later execution via <see cref="start"/>.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="compute">The async computation to execute.</param>
    /// <returns>The signal ID assigned to this async computed node.</returns>
    member _.register (name: string) (compute: CancellationToken -> Async<'T>) : int =
        let signal = computedEngine.SignalGraph.createSignal name NotStarted
        let id = signal.Id

        let entry =
            { SignalId = id
              State = NotStarted
              CancellationToken = None
              AsyncCompute =
                Some(fun ct ->
                    async {
                        let! result = compute ct
                        return result :> obj
                    })
              StreamingSource = None }

        entries.[id] <- entry
        id

    /// <summary>
    /// Register a streaming async computed value backed by an
    /// <see cref="IAsyncEnumerable{'T}"/>. Each emitted value updates the signal
    /// to the Streaming state.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="source">The async enumerable source factory.</param>
    /// <returns>The signal ID assigned to this async computed node.</returns>
    member _.registerStreaming (name: string) (source: CancellationToken -> IAsyncEnumerable<'T>) : int =
        let signal = computedEngine.SignalGraph.createSignal name NotStarted
        let id = signal.Id

        let entry =
            { SignalId = id
              State = NotStarted
              CancellationToken = None
              AsyncCompute = None
              StreamingSource =
                Some(fun ct ->
                    { new IAsyncEnumerable<obj> with
                        member _.GetAsyncEnumerator(ct2) =
                            let innerCt =
                                if ct = CancellationToken.None then ct2
                                else ct

                            let inner = (source innerCt).GetAsyncEnumerator(innerCt)

                            { new IAsyncEnumerator<obj> with
                                member _.Current = inner.Current :> obj

                                member _.MoveNextAsync() = inner.MoveNextAsync()

                                member _.DisposeAsync() = inner.DisposeAsync() } }) }

        entries.[id] <- entry
        id

    /// <summary>
    /// Begin execution of the async computed node. For one-shot computations,
    /// runs the async function and updates the signal to Loaded or Failed.
    /// For streaming sources, iterates the async enumerable updating the signal
    /// to Streaming for each emitted value.
    /// </summary>
    /// <param name="signalId">The signal ID to start.</param>
    /// <returns>An async computation that completes when the node finishes.</returns>
    member this.start(signalId: int) : Async<unit> =
        async {
            match entries.TryGetValue(signalId) with
            | true, entry ->
                let cts = new CancellationTokenSource()

                entries.[signalId] <-
                    { entry with
                        State = Loading
                        CancellationToken = Some cts }

                match entry.AsyncCompute with
                | Some compute ->
                    try
                        let! result = compute cts.Token
                        this.updateState signalId (Loaded result)
                        computedEngine.SignalGraph.updateSignal signalId result
                    with ex ->
                        this.updateState signalId (Failed ex)
                | None ->
                    match entry.StreamingSource with
                    | Some source ->
                        try
                            let enumerable = source cts.Token
                            let enumerator = enumerable.GetAsyncEnumerator(cts.Token)

                            try
                                let mutable hasMore = true

                                while hasMore do
                                    let! moveResult =
                                        enumerator.MoveNextAsync().AsTask()
                                        |> Async.AwaitTask

                                    if moveResult then
                                        let value = enumerator.Current
                                        this.updateState signalId (Streaming value)
                                        computedEngine.SignalGraph.updateSignal signalId value
                                    else
                                        hasMore <- false

                                // Streaming completed — promote last value to Loaded if available
                                match entries.TryGetValue(signalId) with
                                | true, e ->
                                    match e.State with
                                    | Streaming v -> this.updateState signalId (Loaded v)
                                    | _ -> ()
                                | _ -> ()
                            finally
                                enumerator.DisposeAsync().AsTask()
                                |> Async.AwaitTask
                                |> Async.RunSynchronously
                        with ex ->
                            this.updateState signalId (Failed ex)
                    | None -> ()
            | _ -> ()
        }

    /// <summary>
    /// Cancel a running async computed node by signalling its cancellation token.
    /// </summary>
    /// <param name="signalId">The signal ID to cancel.</param>
    member _.cancel(signalId: int) : unit =
        match entries.TryGetValue(signalId) with
        | true, entry ->
            match entry.CancellationToken with
            | Some cts ->
                cts.Cancel()

                entries.[signalId] <-
                    { entry with
                        CancellationToken = None }
            | None -> ()
        | _ -> ()

    /// <summary>
    /// Get the current lifecycle state of an async computed node.
    /// </summary>
    /// <param name="signalId">The signal ID to query.</param>
    /// <returns>The current state, or NotStarted if the signal is not found.</returns>
    member _.getState(signalId: int) : AsyncComputedState<obj> =
        match entries.TryGetValue(signalId) with
        | true, entry -> entry.State
        | _ -> NotStarted

    /// <summary>
    /// Get the current value if the async computation has completed or is streaming.
    /// </summary>
    /// <param name="signalId">The signal ID to query.</param>
    /// <returns>The value if Loaded or Streaming, or None otherwise.</returns>
    member _.getValue(signalId: int) : 'T option =
        match entries.TryGetValue(signalId) with
        | true, entry ->
            match entry.State with
            | Loaded v ->
                try
                    Some(v :?> 'T)
                with :? System.InvalidCastException ->
                    None
            | Streaming v ->
                try
                    Some(v :?> 'T)
                with :? System.InvalidCastException ->
                    None
            | _ -> None
        | _ -> None

    /// <summary>Update the internal state for a given signal ID.</summary>
    member private _.updateState (signalId: int) (state: AsyncComputedState<obj>) =
        match entries.TryGetValue(signalId) with
        | true, entry -> entries.[signalId] <- { entry with State = state }
        | _ -> ()
