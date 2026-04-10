namespace FSharp.Wasm.Protocol

open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

/// <summary>Supported transport mechanisms for GGBP message exchange.</summary>
type TransportKind =
    | WebSocket = 0
    | Nats = 1
    | Sse = 2
    | FetchResponse = 3
    | WasiStreams = 4

/// <summary>
/// Abstraction over a bidirectional GGBP transport channel.
/// Implementations must be safe for concurrent send/receive.
/// </summary>
type ITransport =
    /// <summary>Send a patch to the remote peer.</summary>
    abstract Send: Patch -> Async<unit>

    /// <summary>
    /// Receive the next available patch from the remote peer.
    /// Returns <c>None</c> when the channel is closed or cancelled.
    /// </summary>
    abstract Receive: CancellationToken -> Async<Patch option>

/// <summary>
/// In-memory transport backed by a <see cref="ConcurrentQueue{T}"/>.
/// Suitable for unit and integration testing without real I/O.
/// </summary>
type InMemoryTransport() =
    let queue = ConcurrentQueue<Patch>()
    let signal = new SemaphoreSlim(0)

    interface ITransport with
        /// <summary>Enqueue a patch for the receiver.</summary>
        member _.Send(patch: Patch) =
            async {
                queue.Enqueue(patch)
                signal.Release() |> ignore
            }

        /// <summary>
        /// Dequeue the next patch, waiting asynchronously if the queue is empty.
        /// Returns <c>None</c> if cancellation is requested.
        /// </summary>
        member _.Receive(ct: CancellationToken) =
            async {
                try
                    do!
                        signal.WaitAsync(ct)
                        |> Async.AwaitTask

                    match queue.TryDequeue() with
                    | true, patch -> return Some patch
                    | false, _ -> return None
                with :? TaskCanceledException ->
                    return None
            }

    interface System.IDisposable with
        member _.Dispose() = signal.Dispose()

/// <summary>
/// Stub WebSocket transport that uses an internal queue
/// for testing without a real WebSocket connection.
/// </summary>
type WebSocketTransport() =
    let queue = ConcurrentQueue<Patch>()
    let signal = new SemaphoreSlim(0)

    interface ITransport with
        /// <summary>Enqueue a patch as if sending over a WebSocket.</summary>
        member _.Send(patch: Patch) =
            async {
                queue.Enqueue(patch)
                signal.Release() |> ignore
            }

        /// <summary>
        /// Dequeue a patch as if receiving from a WebSocket.
        /// Returns <c>None</c> if cancellation is requested.
        /// </summary>
        member _.Receive(ct: CancellationToken) =
            async {
                try
                    do!
                        signal.WaitAsync(ct)
                        |> Async.AwaitTask

                    match queue.TryDequeue() with
                    | true, patch -> return Some patch
                    | false, _ -> return None
                with :? TaskCanceledException ->
                    return None
            }

    interface System.IDisposable with
        member _.Dispose() = signal.Dispose()
