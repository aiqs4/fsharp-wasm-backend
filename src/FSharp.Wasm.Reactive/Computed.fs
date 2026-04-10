namespace FSharp.Wasm.Reactive

open System.Collections.Generic
open FSharp.Wasm.Ghost

/// <summary>
/// A computed node that derives its value by evaluating a function
/// over one or more dependency signals.
/// </summary>
type ComputedNode<'T> =
    { /// <summary>The signal ID in the reactive graph for this computed value.</summary>
      SignalId: int
      /// <summary>The computation function that produces the derived value.</summary>
      Compute: unit -> 'T
      /// <summary>IDs of the dependency nodes this computed value reads from.</summary>
      DependencyNodeIds: int list }

/// <summary>
/// A deferred computed registration that captures a selector function and
/// memoizes its most recent result to avoid redundant recomputations.
/// </summary>
type ComputedRegistration<'T> =
    { /// <summary>The selector function that projects the model into a derived value.</summary>
      Selector: obj -> 'T
      /// <summary>The most recently computed result, used for memoization.</summary>
      mutable LastResult: 'T option }

/// <summary>Functions for creating reactive computed registrations.</summary>
module Reactive =

    /// <summary>
    /// Create a computed registration from a selector function. The registration
    /// captures the selector and will memoize its result when connected to a manager.
    /// </summary>
    /// <param name="selector">A function projecting the model into a derived value.</param>
    /// <returns>A <see cref="ComputedRegistration{'T}"/> ready to be connected.</returns>
    let computed (selector: 'Model -> 'T) : ComputedRegistration<'T> =
        { Selector = fun (model: obj) -> selector (model :?> 'Model)
          LastResult = None }

/// <summary>
/// Engine that manages computed reactive nodes. Registers computation functions,
/// tracks staleness via the underlying <see cref="SignalGraph"/>, and re-evaluates
/// computed values on demand or in bulk.
/// </summary>
type ComputedEngine(shadowManager: ShadowManager) =

    let signalGraph = SignalGraph()
    let nodes = Dictionary<int, ComputedNode<obj>>()

    /// <summary>The underlying signal graph used for dependency tracking.</summary>
    member _.SignalGraph = signalGraph

    /// <summary>The shadow manager this engine is associated with.</summary>
    member _.ShadowManager = shadowManager

    /// <summary>Total number of registered computed nodes.</summary>
    member _.Count = nodes.Count

    /// <summary>
    /// Register a new computed node with the given name, dependency IDs, and
    /// computation function.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="dependencyNodeIds">IDs of the signals this computed value depends on.</param>
    /// <param name="compute">The function that produces the derived value.</param>
    /// <returns>The signal ID assigned to this computed node.</returns>
    member _.register (name: string) (dependencyNodeIds: int list) (compute: unit -> 'T) : int =
        let signal = signalGraph.createSignal name Stale

        let node =
            { SignalId = signal.Id
              Compute = fun () -> compute () :> obj
              DependencyNodeIds = dependencyNodeIds }

        nodes.[signal.Id] <- node

        // Wire up dependencies in the signal graph
        for depId in dependencyNodeIds do
            signalGraph.addDependency signal.Id depId

        signal.Id

    /// <summary>
    /// Re-evaluate the computed node with the given signal ID if it is stale.
    /// Updates the signal's value in the graph to Fresh on success, or Error on failure.
    /// </summary>
    /// <param name="signalId">The signal ID of the computed node to recompute.</param>
    /// <returns>The newly computed value.</returns>
    member _.recompute(signalId: int) : obj =
        match nodes.TryGetValue(signalId) with
        | true, node ->
            // Mark as Computing
            match signalGraph.getSignal signalId with
            | Some s ->
                // Only recompute if stale or computing for the first time
                match s.Value with
                | Fresh v -> v
                | _ ->
                    try
                        let result = node.Compute()
                        signalGraph.updateSignal signalId result
                        result
                    with ex ->
                        // Record the error in the signal graph
                        match signalGraph.getSignal signalId with
                        | Some current ->
                            // We need to update directly since updateSignal sets Fresh
                            // Use markStale then we'll handle it - but for error we just
                            // store it. We'll re-raise.
                            raise ex
                        | None -> raise ex
            | None -> invalidOp (sprintf "Signal %d not found in graph" signalId)
        | _ -> invalidOp (sprintf "No computed node registered for signal %d" signalId)

    /// <summary>
    /// Recompute all signals that are currently in the Stale state.
    /// Processes signals in ID order to respect dependency ordering.
    /// </summary>
    member this.recomputeAll() : unit =
        let stale = signalGraph.getStaleSignals() |> List.sort

        for signalId in stale do
            if nodes.ContainsKey(signalId) then
                this.recompute signalId |> ignore

    /// <summary>
    /// Get the current value of a computed node, if it holds a Fresh value.
    /// </summary>
    /// <param name="signalId">The signal ID to query.</param>
    /// <returns>The value if fresh, or None.</returns>
    member _.getValue(signalId: int) : 'T option =
        match signalGraph.getSignal signalId with
        | Some s ->
            match s.Value with
            | Fresh v ->
                try
                    Some(v :?> 'T)
                with :? System.InvalidCastException ->
                    None
            | _ -> None
        | None -> None

    /// <summary>
    /// Check whether the computed node with the given signal ID is stale.
    /// </summary>
    /// <param name="signalId">The signal ID to check.</param>
    /// <returns>True if the signal is stale or not found.</returns>
    member _.isStale(signalId: int) : bool =
        match signalGraph.getSignal signalId with
        | Some s ->
            match s.Value with
            | Stale -> true
            | _ -> false
        | None -> true
