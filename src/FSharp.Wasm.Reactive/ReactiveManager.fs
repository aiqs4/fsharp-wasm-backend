namespace FSharp.Wasm.Reactive

open System.Threading
open FSharp.Wasm.Ghost

/// <summary>
/// Unified orchestrator for all reactive subsystems. Provides a single entry
/// point for registering computed values, effects, and async computations,
/// and drives the reactive tick loop that recomputes stale values and runs
/// pending effects.
/// </summary>
type ReactiveManager(shadowManager: ShadowManager) =

    let computedEngine = ComputedEngine(shadowManager)
    let effectEngine = EffectEngine(shadowManager)
    let asyncComputedEngine = AsyncComputedEngine(shadowManager, computedEngine)
    let asyncSignalIds = System.Collections.Generic.List<int>()
    let mutable disposed = false

    let ensureNotDisposed () =
        if disposed then
            invalidOp "ReactiveManager has been disposed"

    /// <summary>Total number of registered computed nodes.</summary>
    member _.ComputedCount = computedEngine.Count

    /// <summary>Total number of registered effect nodes.</summary>
    member _.EffectCount = effectEngine.getRegisteredEffects().Length

    /// <summary>Total number of registered async computed nodes.</summary>
    member _.AsyncCount = asyncComputedEngine.Count

    /// <summary>The underlying computed engine for advanced scenarios.</summary>
    member _.ComputedEngine = computedEngine

    /// <summary>The underlying effect engine for advanced scenarios.</summary>
    member _.EffectEngine = effectEngine

    /// <summary>The underlying async computed engine for advanced scenarios.</summary>
    member _.AsyncComputedEngine = asyncComputedEngine

    /// <summary>
    /// Register a computed reactive value that derives from the given dependency nodes.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="dependencyNodeIds">IDs of the dependency nodes.</param>
    /// <param name="compute">The computation function producing the derived value.</param>
    /// <returns>The signal ID assigned to the computed node.</returns>
    member _.computed (name: string) (dependencyNodeIds: int list) (compute: unit -> 'T) : int =
        ensureNotDisposed ()
        computedEngine.register name dependencyNodeIds compute

    /// <summary>
    /// Register a side-effect that fires on dependency change.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="dependencyNodeIds">IDs of the dependency nodes to watch.</param>
    /// <param name="action">The side-effecting action to run.</param>
    /// <returns>The effect ID.</returns>
    member _.effect (name: string) (dependencyNodeIds: int list) (action: 'T -> unit) : int =
        ensureNotDisposed ()
        effectEngine.register name dependencyNodeIds OnChange action

    /// <summary>
    /// Register a side-effect that fires after a transaction commit.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="dependencyNodeIds">IDs of the dependency nodes to watch.</param>
    /// <param name="action">The side-effecting action to run.</param>
    /// <returns>The effect ID.</returns>
    member _.effectOnCommit (name: string) (dependencyNodeIds: int list) (action: 'T -> unit) : int =
        ensureNotDisposed ()
        effectEngine.register name dependencyNodeIds OnCommit action

    /// <summary>
    /// Register a one-shot async computed value.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="compute">The async computation function.</param>
    /// <returns>The signal ID assigned to the async node.</returns>
    member _.asyncComputed (name: string) (compute: CancellationToken -> Async<'T>) : int =
        ensureNotDisposed ()
        let id = asyncComputedEngine.register name compute
        asyncSignalIds.Add(id)
        id

    /// <summary>
    /// Drive one reactive tick: recompute all stale computed values, then
    /// run all pending side-effects. This is the core scheduling loop.
    /// </summary>
    member _.tick() : unit =
        ensureNotDisposed ()
        computedEngine.recomputeAll ()
        effectEngine.runPendingEffects ()

    /// <summary>
    /// Dispose the reactive manager, cancelling all async computations and
    /// preventing further registrations.
    /// </summary>
    member _.dispose() : unit =
        if not disposed then
            disposed <- true

            // Cancel all running async computations by tracking registered IDs
            for signalId in asyncSignalIds do
                asyncComputedEngine.cancel signalId
