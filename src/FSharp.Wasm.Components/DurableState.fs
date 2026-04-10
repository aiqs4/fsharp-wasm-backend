namespace FSharp.Wasm.Components

open System.Collections.Concurrent
open FSharp.Wasm.Ghost

/// <summary>
/// Interface for durable state persistence.
/// Provides CRUD operations for serialized state identified by string IDs.
/// </summary>
type IDurableStateStore =
    /// <summary>Save state for the given identifier.</summary>
    /// <param name="id">The state identifier.</param>
    /// <param name="state">The state to persist.</param>
    abstract member Save<'T> : id: string -> state: 'T -> Async<unit>

    /// <summary>Load state for the given identifier.</summary>
    /// <param name="id">The state identifier.</param>
    /// <returns>The deserialized state, or None if not found.</returns>
    abstract member Load<'T> : id: string -> Async<'T option>

    /// <summary>Delete state for the given identifier.</summary>
    /// <param name="id">The state identifier.</param>
    /// <returns>True if the state existed and was deleted.</returns>
    abstract member Delete : id: string -> Async<bool>

    /// <summary>List all persisted state identifiers.</summary>
    /// <returns>A list of all state identifiers.</returns>
    abstract member ListIds : unit -> Async<string list>

/// <summary>Configuration for durable state storage.</summary>
type DurableStateConfig =
    { /// <summary>Name of the backing store.</summary>
      StoreName: string
      /// <summary>Optional time-to-live in seconds.</summary>
      Ttl: int option
      /// <summary>Target platform identifier (e.g., "cloudflare", "fastly").</summary>
      Platform: string }

/// <summary>
/// In-memory implementation of <see cref="IDurableStateStore"/> for testing.
/// Backed by a <see cref="ConcurrentDictionary{TKey,TValue}"/>.
/// </summary>
type MemoryDurableStateStore() =
    let store = ConcurrentDictionary<string, obj>()

    interface IDurableStateStore with
        member _.Save<'T>(id: string) (state: 'T) =
            async { store.[id] <- (state :> obj) }

        member _.Load<'T>(id: string) =
            async {
                match store.TryGetValue(id) with
                | true, value -> return Some(value :?> 'T)
                | _ -> return None
            }

        member _.Delete(id: string) =
            async {
                let mutable removed = Unchecked.defaultof<obj>
                return store.TryRemove(id, &removed)
            }

        member _.ListIds() =
            async { return store.Keys |> Seq.sort |> Seq.toList }

/// <summary>
/// Manages durable state persistence integrated with the ghost-graph
/// <see cref="ShadowManager"/>. Supports save, restore, and periodic
/// auto-save of the ghost-graph state.
/// </summary>
type DurableStateManager(stateStore: IDurableStateStore, shadowManager: ShadowManager) =

    let mutable autoSaveRunning = false

    /// <summary>
    /// Serialize the current ghost-graph state and persist it under the given identifier.
    /// </summary>
    /// <param name="id">The state identifier to save under.</param>
    member _.save(id: string) : Async<unit> =
        async {
            let data = shadowManager.serialize ()
            do! stateStore.Save<byte array> id data
        }

    /// <summary>
    /// Load persisted state and restore it into the ghost-graph.
    /// </summary>
    /// <param name="id">The state identifier to restore from.</param>
    /// <returns>True if state was found and restored, false otherwise.</returns>
    member _.restore(id: string) : Async<bool> =
        async {
            let! loaded = stateStore.Load<byte array>(id)

            match loaded with
            | Some data ->
                shadowManager.restore data
                return true
            | None -> return false
        }

    /// <summary>
    /// Start periodic auto-save of the ghost-graph state.
    /// Saves the state at the specified interval in milliseconds.
    /// </summary>
    /// <param name="intervalMs">The interval between saves in milliseconds.</param>
    member this.autoSave(intervalMs: int) : Async<unit> =
        async {
            autoSaveRunning <- true

            while autoSaveRunning do
                do! Async.Sleep(intervalMs)

                if shadowManager.DirtyCount > 0 then
                    do! this.save ("auto")
        }

    /// <summary>Stop the auto-save loop.</summary>
    member _.stopAutoSave() = autoSaveRunning <- false
