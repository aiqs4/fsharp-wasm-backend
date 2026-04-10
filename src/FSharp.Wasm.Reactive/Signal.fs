namespace FSharp.Wasm.Reactive

open System.Collections.Generic

/// <summary>Represents the current evaluation state of a reactive signal.</summary>
type SignalState<'T> =
    /// <summary>The signal holds a fresh, up-to-date value.</summary>
    | Fresh of 'T
    /// <summary>The signal's value is outdated because a dependency changed.</summary>
    | Stale
    /// <summary>The signal is currently being recomputed.</summary>
    | Computing
    /// <summary>The signal's last computation failed with an exception.</summary>
    | Error of exn

/// <summary>
/// An immutable descriptor for a single reactive signal node, including its
/// current value state and its position in the dependency graph.
/// </summary>
type Signal<'T> =
    { /// <summary>Unique identifier for this signal.</summary>
      Id: int
      /// <summary>Human-readable name for diagnostics.</summary>
      Name: string
      /// <summary>Current evaluation state and value.</summary>
      Value: SignalState<'T>
      /// <summary>Set of signal IDs this signal depends on.</summary>
      Dependencies: Set<int>
      /// <summary>Set of signal IDs that depend on this signal.</summary>
      Dependents: Set<int> }

/// <summary>
/// Mutable graph that tracks reactive signals and their dependency relationships.
/// Provides operations for creating, updating, and querying signals, as well as
/// propagating staleness through the dependency graph.
/// </summary>
type SignalGraph() =

    let signals = Dictionary<int, Signal<obj>>()
    let mutable nextId = 1

    /// <summary>Total number of signals in the graph.</summary>
    member _.Count = signals.Count

    /// <summary>
    /// Create a new signal with the given name and initial value.
    /// </summary>
    /// <param name="name">Human-readable name for the signal.</param>
    /// <param name="initialValue">The initial value of the signal.</param>
    /// <returns>The newly created signal.</returns>
    member _.createSignal (name: string) (initialValue: 'T) : Signal<'T> =
        let id = nextId
        nextId <- nextId + 1

        let signal =
            { Id = id
              Name = name
              Value = Fresh(initialValue :> obj)
              Dependencies = Set.empty
              Dependents = Set.empty }

        signals.[id] <- signal
        // Return a typed view
        { Id = id
          Name = name
          Value = Fresh initialValue
          Dependencies = Set.empty
          Dependents = Set.empty }

    /// <summary>
    /// Look up a signal by its identifier.
    /// </summary>
    /// <param name="id">The signal identifier.</param>
    /// <returns>The signal if found, or None.</returns>
    member _.getSignal(id: int) : Signal<obj> option =
        match signals.TryGetValue(id) with
        | true, s -> Some s
        | _ -> None

    /// <summary>
    /// Update a signal's value to Fresh and mark all transitive dependents as Stale.
    /// </summary>
    /// <param name="id">The signal to update.</param>
    /// <param name="value">The new value.</param>
    member this.updateSignal (id: int) (value: 'T) : unit =
        match signals.TryGetValue(id) with
        | true, s ->
            signals.[id] <- { s with Value = Fresh(value :> obj) }
            // Mark all direct dependents as stale
            for depId in s.Dependents do
                this.markStale depId
        | _ -> ()

    /// <summary>
    /// Recursively mark a signal and all its transitive dependents as Stale.
    /// </summary>
    /// <param name="id">The signal to mark stale.</param>
    member this.markStale(id: int) : unit =
        match signals.TryGetValue(id) with
        | true, s ->
            match s.Value with
            | Stale -> () // Already stale, stop propagation
            | _ ->
                signals.[id] <- { s with Value = Stale }

                for depId in s.Dependents do
                    this.markStale depId
        | _ -> ()

    /// <summary>
    /// Register a dependency relationship: signalId depends on dependsOn.
    /// </summary>
    /// <param name="signalId">The signal that depends on another.</param>
    /// <param name="dependsOn">The signal being depended upon.</param>
    member _.addDependency (signalId: int) (dependsOn: int) : unit =
        match signals.TryGetValue(signalId) with
        | true, s ->
            signals.[signalId] <-
                { s with
                    Dependencies = s.Dependencies.Add(dependsOn) }
        | _ -> ()

        match signals.TryGetValue(dependsOn) with
        | true, s ->
            signals.[dependsOn] <-
                { s with
                    Dependents = s.Dependents.Add(signalId) }
        | _ -> ()

    /// <summary>
    /// Return the IDs of all signals currently in the Stale state.
    /// </summary>
    /// <returns>List of stale signal IDs.</returns>
    member _.getStaleSignals() : int list =
        [ for kvp in signals do
              match kvp.Value.Value with
              | Stale -> yield kvp.Key
              | _ -> () ]
