namespace FSharp.Wasm.Components

open System.Collections.Concurrent

/// <summary>Options for KV store operations.</summary>
type KvOptions =
    { /// <summary>Optional time-to-live in seconds.</summary>
      Ttl: int option
      /// <summary>Optional metadata key-value pairs.</summary>
      Metadata: Map<string, string> option }

/// <summary>
/// Typed key-value store interface for edge platform KV primitives.
/// Values are stored and retrieved with generic type safety.
/// </summary>
type IKvStore =
    /// <summary>Get a value by key, returning None if the key does not exist.</summary>
    /// <param name="key">The key to look up.</param>
    /// <returns>The deserialized value, or None.</returns>
    abstract member Get<'T> : key: string -> Async<'T option>

    /// <summary>Set a value for the given key, creating or overwriting.</summary>
    /// <param name="key">The key to set.</param>
    /// <param name="value">The value to store.</param>
    abstract member Set<'T> : key: string -> value: 'T -> Async<unit>

    /// <summary>Delete a key from the store.</summary>
    /// <param name="key">The key to delete.</param>
    /// <returns>True if the key existed and was deleted.</returns>
    abstract member Delete : key: string -> Async<bool>

    /// <summary>List all keys matching the given prefix.</summary>
    /// <param name="prefix">The key prefix to filter by.</param>
    /// <returns>A list of matching keys.</returns>
    abstract member List : prefix: string -> Async<string list>

    /// <summary>Check whether a key exists in the store.</summary>
    /// <param name="key">The key to check.</param>
    /// <returns>True if the key exists.</returns>
    abstract member Exists : key: string -> Async<bool>

/// <summary>
/// In-memory implementation of <see cref="IKvStore"/> for testing.
/// Backed by a <see cref="ConcurrentDictionary{TKey,TValue}"/>.
/// </summary>
type MemoryKvStore() =
    let store = ConcurrentDictionary<string, obj>()

    interface IKvStore with
        member _.Get<'T>(key: string) =
            async {
                match store.TryGetValue(key) with
                | true, value -> return Some(value :?> 'T)
                | _ -> return None
            }

        member _.Set<'T>(key: string) (value: 'T) =
            async { store.[key] <- (value :> obj) }

        member _.Delete(key: string) =
            async {
                let mutable removed = Unchecked.defaultof<obj>
                return store.TryRemove(key, &removed)
            }

        member _.List(prefix: string) =
            async {
                return
                    store.Keys
                    |> Seq.filter (fun k -> k.StartsWith(prefix))
                    |> Seq.sort
                    |> Seq.toList
            }

        member _.Exists(key: string) =
            async { return store.ContainsKey(key) }

/// <summary>
/// Convenience module for KV store operations.
/// Delegates to a configurable <see cref="IKvStore"/> instance.
/// </summary>
[<RequireQualifiedAccess>]
module Kv =

    let mutable private store: IKvStore = MemoryKvStore() :> IKvStore

    /// <summary>Configure the backing KV store implementation.</summary>
    /// <param name="kvStore">The <see cref="IKvStore"/> implementation to use.</param>
    let configure (kvStore: IKvStore) = store <- kvStore

    /// <summary>Get a value by key.</summary>
    /// <param name="key">The key to look up.</param>
    /// <returns>The deserialized value, or None.</returns>
    let get<'T> (key: string) : Async<'T option> = store.Get<'T>(key)

    /// <summary>Set a value for the given key.</summary>
    /// <param name="key">The key to set.</param>
    /// <param name="value">The value to store.</param>
    let set<'T> (key: string) (value: 'T) : Async<unit> = store.Set<'T> key value

    /// <summary>Delete a key from the store.</summary>
    /// <param name="key">The key to delete.</param>
    /// <returns>True if the key existed and was deleted.</returns>
    let delete (key: string) : Async<bool> = store.Delete(key)

    /// <summary>List all keys matching the given prefix.</summary>
    /// <param name="prefix">The key prefix to filter by.</param>
    /// <returns>A list of matching keys.</returns>
    let list (prefix: string) : Async<string list> = store.List(prefix)
