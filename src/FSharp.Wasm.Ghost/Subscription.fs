namespace FSharp.Wasm.Ghost

open System.Collections.Generic
open FSharp.Wasm.Protocol

/// <summary>A subscription binding a subscriber to a node path with a callback.</summary>
type Subscription =
    { /// <summary>Unique identifier for the subscriber.</summary>
      Id: string
      /// <summary>Path pattern this subscription matches against.</summary>
      NodePath: string
      /// <summary>Callback invoked when a matching patch is emitted.</summary>
      Callback: Patch -> unit }

/// <summary>
/// Manages pub/sub subscriptions for model branches in the ghost graph.
/// Subscribers register for specific paths and receive patches when those paths are modified.
/// </summary>
type SubscriptionManager() =

    // path -> list of subscriptions
    let subscriptions = Dictionary<string, ResizeArray<Subscription>>()

    /// <summary>
    /// Subscribe to changes at the given path.
    /// </summary>
    /// <param name="path">The node path to subscribe to.</param>
    /// <param name="subscriberId">Unique identifier for the subscriber.</param>
    /// <param name="callback">Function invoked with the patch when changes occur.</param>
    member _.subscribe (path: string) (subscriberId: string) (callback: Patch -> unit) =
        let sub =
            { Id = subscriberId
              NodePath = path
              Callback = callback }

        match subscriptions.TryGetValue(path) with
        | true, subs -> subs.Add(sub)
        | _ ->
            let subs = ResizeArray<Subscription>()
            subs.Add(sub)
            subscriptions.[path] <- subs

    /// <summary>
    /// Unsubscribe a subscriber from a specific path.
    /// </summary>
    /// <param name="subscriberId">The subscriber to remove.</param>
    /// <param name="path">The path to unsubscribe from.</param>
    member _.unsubscribe (subscriberId: string) (path: string) =
        match subscriptions.TryGetValue(path) with
        | true, subs ->
            subs.RemoveAll(fun s -> s.Id = subscriberId) |> ignore

            if subs.Count = 0 then
                subscriptions.Remove(path) |> ignore
        | _ -> ()

    /// <summary>
    /// Notify all subscribers for the given path with a patch.
    /// </summary>
    /// <param name="path">The path that changed.</param>
    /// <param name="patch">The patch describing the changes.</param>
    member _.notify (path: string) (patch: Patch) =
        match subscriptions.TryGetValue(path) with
        | true, subs ->
            for sub in subs do
                sub.Callback patch
        | _ -> ()

    /// <summary>
    /// Get all current subscribers for a given path.
    /// </summary>
    /// <param name="path">The path to query.</param>
    /// <returns>List of subscriptions for the path.</returns>
    member _.getSubscribers(path: string) : Subscription list =
        match subscriptions.TryGetValue(path) with
        | true, subs -> subs |> Seq.toList
        | _ -> []
