namespace FSharp.Wasm.Reactive

open System.Collections.Generic
open FSharp.Wasm.Ghost

/// <summary>Specifies when an effect should be triggered relative to the reactive lifecycle.</summary>
type EffectTrigger =
    /// <summary>Run whenever any dependency signal changes value.</summary>
    | OnChange
    /// <summary>Run after a transaction is committed.</summary>
    | OnCommit
    /// <summary>Run after a transaction is rolled back.</summary>
    | OnRollback
    /// <summary>Run on every reactive tick regardless of changes.</summary>
    | Always

/// <summary>
/// An immutable descriptor for a registered side-effect, including its
/// trigger condition, dependency set, and action to execute.
/// </summary>
type EffectRegistration =
    { /// <summary>Unique identifier for this effect.</summary>
      Id: int
      /// <summary>Human-readable name for diagnostics.</summary>
      Name: string
      /// <summary>IDs of the dependency nodes this effect watches.</summary>
      DependencyNodeIds: int list
      /// <summary>The condition under which this effect fires.</summary>
      Trigger: EffectTrigger
      /// <summary>The side-effecting action to run, receiving the dependency value.</summary>
      Action: obj -> unit }

/// <summary>
/// Engine that manages reactive side-effects. Registers effect actions with
/// trigger conditions and dependency sets, and executes pending effects
/// in registration order when their conditions are met.
/// </summary>
type EffectEngine(shadowManager: ShadowManager) =

    let effects = Dictionary<int, EffectRegistration>()
    let registrationOrder = List<int>()
    let mutable nextId = 1
    let pendingEffectIds = HashSet<int>()

    /// <summary>The shadow manager this engine is associated with.</summary>
    member _.ShadowManager = shadowManager

    /// <summary>Number of effects currently pending execution.</summary>
    member _.PendingCount = pendingEffectIds.Count

    /// <summary>
    /// Register a new effect with the given name, dependencies, trigger, and action.
    /// </summary>
    /// <param name="name">Human-readable name for diagnostics.</param>
    /// <param name="dependencyNodeIds">IDs of the dependency nodes this effect watches.</param>
    /// <param name="trigger">The condition under which this effect fires.</param>
    /// <param name="action">The side-effecting action to run.</param>
    /// <returns>The unique ID assigned to this effect.</returns>
    member _.register (name: string) (dependencyNodeIds: int list) (trigger: EffectTrigger) (action: 'T -> unit) : int =
        let id = nextId
        nextId <- nextId + 1

        let registration =
            { Id = id
              Name = name
              DependencyNodeIds = dependencyNodeIds
              Trigger = trigger
              Action = fun (v: obj) -> action (v :?> 'T) }

        effects.[id] <- registration
        registrationOrder.Add(id)
        id

    /// <summary>
    /// Mark an effect as pending so it will run on the next call to
    /// <see cref="runPendingEffects"/>.
    /// </summary>
    /// <param name="effectId">The effect ID to mark pending.</param>
    member _.markPending(effectId: int) =
        if effects.ContainsKey(effectId) then
            pendingEffectIds.Add(effectId) |> ignore

    /// <summary>
    /// Run all pending effects whose dependencies are dirty or whose trigger
    /// condition is met. Effects execute in registration order. After execution,
    /// the pending set is cleared.
    /// </summary>
    member this.runPendingEffects() : unit =
        // Collect effects to run in registration order
        let toRun =
            [ for id in registrationOrder do
                  match effects.TryGetValue(id) with
                  | true, reg ->
                      let shouldRun =
                          match reg.Trigger with
                          | Always -> true
                          | OnChange -> pendingEffectIds.Contains(id) || this.hasDirtyDependency reg
                          | OnCommit -> pendingEffectIds.Contains(id)
                          | OnRollback -> pendingEffectIds.Contains(id)

                      if shouldRun then
                          yield reg
                  | _ -> () ]

        pendingEffectIds.Clear()

        for reg in toRun do
            // Gather current dependency values from the shadow manager
            let depValue = this.gatherDependencyValue reg
            reg.Action depValue

    /// <summary>
    /// Check whether any dependency node of the given effect is currently dirty.
    /// </summary>
    member private _.hasDirtyDependency(reg: EffectRegistration) : bool =
        reg.DependencyNodeIds
        |> List.exists (fun nodeId ->
            match shadowManager.getNode nodeId with
            | Some node -> node.Dirty = DirtyState.Pending
            | None -> false)

    /// <summary>
    /// Gather the first available dependency value for the effect action.
    /// </summary>
    member private _.gatherDependencyValue(reg: EffectRegistration) : obj =
        match reg.DependencyNodeIds with
        | [] -> () :> obj
        | firstDep :: _ ->
            match shadowManager.getNode firstDep with
            | Some node -> node.Data
            | None -> () :> obj

    /// <summary>
    /// Remove a registered effect by its ID.
    /// </summary>
    /// <param name="effectId">The effect ID to remove.</param>
    member _.remove(effectId: int) : unit =
        effects.Remove(effectId) |> ignore
        registrationOrder.Remove(effectId) |> ignore
        pendingEffectIds.Remove(effectId) |> ignore

    /// <summary>
    /// Get all currently registered effect descriptors.
    /// </summary>
    /// <returns>List of all registered effects.</returns>
    member _.getRegisteredEffects() : EffectRegistration list =
        [ for id in registrationOrder do
              match effects.TryGetValue(id) with
              | true, reg -> yield reg
              | _ -> () ]
