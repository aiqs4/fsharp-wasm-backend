namespace FSharp.Wasm.Components

open System
open System.Collections.Concurrent

/// <summary>A message received from a queue, including metadata.</summary>
type QueueMessage<'T> =
    { /// <summary>Unique message identifier.</summary>
      Id: string
      /// <summary>The deserialized message body.</summary>
      Body: 'T
      /// <summary>Timestamp when the message was enqueued.</summary>
      Timestamp: DateTimeOffset
      /// <summary>Number of times this message has been delivered.</summary>
      RetryCount: int }

/// <summary>
/// Interface for a typed message queue supporting send, receive, batch,
/// and acknowledgement operations.
/// </summary>
type IMessageQueue =
    /// <summary>Send a single message to the queue.</summary>
    /// <param name="message">The message to send.</param>
    abstract member Send<'T> : message: 'T -> Async<unit>

    /// <summary>Send a batch of messages to the queue.</summary>
    /// <param name="messages">The messages to send.</param>
    abstract member SendBatch<'T> : messages: 'T list -> Async<unit>

    /// <summary>Receive the next available message from the queue.</summary>
    /// <returns>The next message, or None if the queue is empty.</returns>
    abstract member Receive<'T> : unit -> Async<'T option>

    /// <summary>Acknowledge successful processing of a message.</summary>
    /// <param name="messageId">The identifier of the message to acknowledge.</param>
    abstract member Acknowledge : messageId: string -> Async<unit>

/// <summary>Configuration for queue consumer behavior.</summary>
type QueueConsumerConfig =
    { /// <summary>Name of the queue to consume from.</summary>
      QueueName: string
      /// <summary>Maximum number of messages to process per batch.</summary>
      BatchSize: int
      /// <summary>Seconds before an unacknowledged message becomes visible again.</summary>
      VisibilityTimeout: int
      /// <summary>Maximum number of delivery attempts before dead-lettering.</summary>
      MaxRetries: int }

/// <summary>
/// In-memory implementation of <see cref="IMessageQueue"/> for testing.
/// Backed by a <see cref="ConcurrentQueue{T}"/>.
/// </summary>
type MemoryQueue<'T>() =
    let queue = ConcurrentQueue<obj>()
    let acknowledged = ConcurrentDictionary<string, bool>()

    interface IMessageQueue with
        member _.Send<'U>(message: 'U) =
            async { queue.Enqueue(message :> obj) }

        member _.SendBatch<'U>(messages: 'U list) =
            async {
                for msg in messages do
                    queue.Enqueue(msg :> obj)
            }

        member _.Receive<'U>() =
            async {
                match queue.TryDequeue() with
                | true, value -> return Some(value :?> 'U)
                | _ -> return None
            }

        member _.Acknowledge(messageId: string) =
            async { acknowledged.[messageId] <- true }

/// <summary>
/// Processes messages from an <see cref="IMessageQueue"/> using a provided handler function.
/// Tracks processed and failed message counts.
/// </summary>
type QueueProcessor<'T>(queue: IMessageQueue, handler: 'T -> Async<unit>) =

    let mutable running = false
    let mutable processedCount = 0
    let mutable failedCount = 0

    /// <summary>Number of messages successfully processed.</summary>
    member _.ProcessedCount = processedCount

    /// <summary>Number of messages that failed processing.</summary>
    member _.FailedCount = failedCount

    /// <summary>
    /// Begin consuming messages from the queue. Runs in a loop until
    /// <see cref="stop"/> is called. Each message is passed to the handler;
    /// failures increment the failed count without stopping the processor.
    /// </summary>
    member _.start() : Async<unit> =
        async {
            running <- true

            while running do
                let! msg = queue.Receive<'T>()

                match msg with
                | Some body ->
                    try
                        do! handler body
                        processedCount <- processedCount + 1
                    with _ ->
                        failedCount <- failedCount + 1
                | None -> do! Async.Sleep(100)
        }

    /// <summary>Stop the message processing loop.</summary>
    member _.stop() = running <- false
