namespace FSharp.Wasm.Components

open System

/// <summary>
/// Marks a type for automatic persistence with optional TTL in seconds.
/// When applied, the runtime will serialize and persist the type's state
/// to the configured durable state store.
/// </summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
type DurableStateAttribute(ttl: int) =
    inherit Attribute()

    /// <summary>Time-to-live in seconds for the persisted state. Use 0 for no expiration.</summary>
    member _.Ttl = ttl

    /// <summary>Creates a DurableState attribute with no TTL (infinite retention).</summary>
    new() = DurableStateAttribute(0)

/// <summary>
/// Marks a function as a queue message handler that consumes messages
/// from the specified queue.
/// </summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type QueueConsumerAttribute(queueName: string) =
    inherit Attribute()

    /// <summary>The name of the queue to consume messages from.</summary>
    member _.QueueName = queueName

/// <summary>
/// Marks a function as able to send messages to the specified queue.
/// </summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type QueueProducerAttribute(queueName: string) =
    inherit Attribute()

    /// <summary>The name of the queue to send messages to.</summary>
    member _.QueueName = queueName

/// <summary>
/// Marks a function for scheduled execution using a cron expression.
/// The runtime will invoke the function according to the specified schedule.
/// </summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type CronScheduleAttribute(expression: string) =
    inherit Attribute()

    /// <summary>The cron expression defining the execution schedule (e.g., "0 * * * *").</summary>
    member _.Expression = expression
