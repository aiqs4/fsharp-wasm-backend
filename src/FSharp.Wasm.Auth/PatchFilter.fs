namespace FSharp.Wasm.Auth

open FSharp.Wasm.Protocol

/// <summary>Row-level security for Ghost patches. Filters patch operations
/// based on authorization predicates, removing unauthorized data before delivery.</summary>
[<RequireQualifiedAccess>]
module PatchFilter =

    /// <summary>
    /// Filters patch operations using a predicate, keeping only the operations
    /// that satisfy the condition.
    /// </summary>
    /// <param name="predicate">A function that returns true for operations to keep.</param>
    /// <param name="patch">The patch to filter.</param>
    /// <returns>A new patch containing only the matching operations.</returns>
    let filter (predicate: PatchOp -> bool) (patch: Patch) : Patch =
        { patch with Ops = patch.Ops |> List.filter predicate }

    /// <summary>
    /// Extracts the NodeId from a PatchOp.
    /// </summary>
    let private getNodeId (op: PatchOp) : int =
        match op with
        | PatchOp.UpdateLeaf(nodeId = NodeId id) -> id
        | PatchOp.ReplaceBranch(nodeId = NodeId id) -> id
        | PatchOp.ListMutation(nodeId = NodeId id) -> id
        | PatchOp.DeleteNode(NodeId id) -> id

    /// <summary>
    /// Filters patch operations to include only those targeting nodes in the allowed set.
    /// </summary>
    /// <param name="allowedIds">The set of node IDs the caller is authorized to see.</param>
    /// <param name="patch">The patch to filter.</param>
    /// <returns>A new patch containing only operations on allowed nodes.</returns>
    let filterByNodeIds (allowedIds: Set<int>) (patch: Patch) : Patch =
        filter (fun op -> Set.contains (getNodeId op) allowedIds) patch

    /// <summary>
    /// Filters patch operations based on the identity's capability.
    /// If the identity has the required role, the full patch is returned.
    /// Otherwise, an empty patch (no operations) is returned.
    /// </summary>
    /// <param name="capability">The required role name.</param>
    /// <param name="identity">The identity to check.</param>
    /// <param name="patch">The patch to filter.</param>
    /// <returns>The full patch if authorized, or an empty patch if not.</returns>
    let filterByCapability (capability: string) (identity: Identity) (patch: Patch) : Patch =
        match CapabilityGuard.require capability identity with
        | Authorized -> patch
        | _ -> { patch with Ops = [] }

/// <summary>
/// Represents a subscription to patch updates with row-level security filtering.
/// </summary>
type SecureSubscription =
    { /// <summary>The subscription path pattern.</summary>
      Path: string
      /// <summary>The user ID that owns this subscription.</summary>
      UserId: string
      /// <summary>A filter function applied to patches before delivery.</summary>
      Filter: Patch -> Patch }
