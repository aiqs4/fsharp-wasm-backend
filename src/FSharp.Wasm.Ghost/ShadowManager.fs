namespace FSharp.Wasm.Ghost

open System.Collections.Generic
open FSharp.Wasm.Protocol

/// <summary>
/// Central mutable manager for the ghost-graph Smart-AST state.
/// Maintains an identity map of nodes, a dependency graph, dirty tracking,
/// and pending transaction state. Integrates with the GGBP protocol for
/// patch serialization, application, commit, and rollback.
/// </summary>
type ShadowManager() =

    let identityMap = Dictionary<int, GhostNode>()
    let dependencyGraph = DependencyGraph()
    let dirtySet = HashSet<int>()
    let pendingCommits = Dictionary<uint32, PatchOp list>()
    let mutable nextTransactionId = 1u

    /// <summary>Total number of registered nodes.</summary>
    member _.NodeCount = identityMap.Count

    /// <summary>Number of currently dirty nodes.</summary>
    member _.DirtyCount = dirtySet.Count

    /// <summary>The internal dependency graph for direct manipulation.</summary>
    member _.DependencyGraph = dependencyGraph

    /// <summary>
    /// Register a new node in the identity map and dependency graph.
    /// </summary>
    /// <param name="id">Unique node identifier.</param>
    /// <param name="data">Initial data payload.</param>
    /// <returns>The newly created <see cref="GhostNode"/>.</returns>
    member _.registerNode (id: int) (data: obj) : GhostNode =
        let node = GhostNode.create id data
        identityMap.[id] <- node
        dependencyGraph.addNode id
        node

    /// <summary>
    /// Look up a node by its identifier.
    /// </summary>
    /// <param name="id">The node identifier.</param>
    /// <returns>The node if found, or None.</returns>
    member _.getNode(id: int) : GhostNode option =
        match identityMap.TryGetValue(id) with
        | true, node -> Some node
        | _ -> None

    /// <summary>Get all registered nodes.</summary>
    member _.getAllNodes() : GhostNode seq = identityMap.Values :> GhostNode seq

    /// <summary>
    /// Signal that a field on a node has changed. Sets the dirty bit on the node
    /// and propagates dirty state to all transitive dependents in the dependency graph.
    /// </summary>
    /// <param name="nodeId">The node that changed.</param>
    /// <param name="fieldIdx">The field index that changed (for future fine-grained tracking).</param>
    member this.signal (nodeId: int) (_fieldIdx: int) =
        match identityMap.TryGetValue(nodeId) with
        | true, node ->
            identityMap.[nodeId] <- GhostNode.markDirty node
            dirtySet.Add(nodeId) |> ignore

            // Propagate dirty state to all transitive dependents
            let affected = dependencyGraph.getTransitiveDependents(nodeId)

            for depId in affected do
                match identityMap.TryGetValue(depId) with
                | true, depNode ->
                    identityMap.[depId] <- GhostNode.markDirty depNode
                    dirtySet.Add(depId) |> ignore
                | _ -> ()
        | _ -> ()

    /// <summary>
    /// Update a specific field on a node. Serializes the value into the node data
    /// and signals the change through the dependency graph.
    /// </summary>
    /// <param name="nodeId">The node to update.</param>
    /// <param name="fieldIdx">The field index to update.</param>
    /// <param name="value">The new field value.</param>
    member this.updateField (nodeId: int) (fieldIdx: int) (value: obj) =
        match identityMap.TryGetValue(nodeId) with
        | true, node ->
            // Store the field update as a (fieldIdx, value) map on the node data
            let fields =
                match node.Data with
                | :? Dictionary<int, obj> as d -> d
                | _ ->
                    let d = Dictionary<int, obj>()
                    d

            fields.[fieldIdx] <- value

            identityMap.[nodeId] <-
                node
                |> GhostNode.updateData (fields :> obj)
                |> GhostNode.incrementVersion

            this.signal nodeId fieldIdx
        | _ -> ()

    /// <summary>
    /// Serialize all dirty nodes into a GGBP <see cref="Patch"/> using Protocol types.
    /// Each dirty node produces a ReplaceBranch operation with its serialized data.
    /// </summary>
    /// <returns>A <see cref="Patch"/> containing operations for all dirty nodes.</returns>
    member _.currentDirtyPatch() : Patch =
        let ops =
            [ for nodeId in dirtySet do
                  match identityMap.TryGetValue(nodeId) with
                  | true, node ->
                      let payload =
                          System.Text.Encoding.UTF8.GetBytes(sprintf "%A" node.Data)

                      yield PatchOp.ReplaceBranch(NodeId nodeId, payload)
                  | _ -> () ]

        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = ops }

    /// <summary>
    /// Apply an incoming patch to the identity map. Each operation updates
    /// the corresponding node's data and version.
    /// </summary>
    /// <param name="patch">The patch to apply.</param>
    member _.applyPatch(patch: Patch) =
        for op in patch.Ops do
            match op with
            | PatchOp.UpdateLeaf(NodeId nodeId, FieldIndex fieldIdx, _wasmType, value) ->
                match identityMap.TryGetValue(nodeId) with
                | true, node ->
                    let fields =
                        match node.Data with
                        | :? Dictionary<int, obj> as d -> d
                        | _ -> Dictionary<int, obj>()

                    fields.[int fieldIdx] <- (value :> obj)

                    identityMap.[nodeId] <-
                        node
                        |> GhostNode.updateData (fields :> obj)
                        |> GhostNode.incrementVersion
                        |> GhostNode.markClean
                | _ -> ()

            | PatchOp.ReplaceBranch(NodeId nodeId, payload) ->
                match identityMap.TryGetValue(nodeId) with
                | true, node ->
                    identityMap.[nodeId] <-
                        node
                        |> GhostNode.updateData (payload :> obj)
                        |> GhostNode.incrementVersion
                        |> GhostNode.markClean
                | _ ->
                    // Create the node if it doesn't exist
                    let node = GhostNode.create nodeId (payload :> obj)
                    identityMap.[nodeId] <- node

            | PatchOp.DeleteNode(NodeId nodeId) ->
                identityMap.Remove(nodeId) |> ignore
                dirtySet.Remove(nodeId) |> ignore
                dependencyGraph.removeNode nodeId

            | PatchOp.ListMutation(NodeId nodeId, _fieldIdx, _listOp, _index, _value) ->
                // Mark the node dirty for re-evaluation; full list mutation
                // handling deferred to typed layer
                match identityMap.TryGetValue(nodeId) with
                | true, node ->
                    identityMap.[nodeId] <- GhostNode.incrementVersion node
                | _ -> ()

    /// <summary>
    /// Mark all pending (dirty) nodes as committed for the given transaction.
    /// Stores the current dirty ops for potential rollback.
    /// </summary>
    /// <param name="transactionId">The transaction to commit.</param>
    member _.commit(transactionId: uint32) =
        let ops =
            [ for nodeId in dirtySet do
                  match identityMap.TryGetValue(nodeId) with
                  | true, node ->
                      identityMap.[nodeId] <- GhostNode.markCommitted node

                      let payload =
                          System.Text.Encoding.UTF8.GetBytes(sprintf "%A" node.Data)

                      yield PatchOp.ReplaceBranch(NodeId nodeId, payload)
                  | _ -> () ]

        pendingCommits.[transactionId] <- ops
        dirtySet.Clear()

    /// <summary>
    /// Roll back a committed transaction by applying the provided rollback patch.
    /// Removes the transaction from pending commits.
    /// </summary>
    /// <param name="transactionId">The transaction to roll back.</param>
    /// <param name="rollbackPatch">The patch that restores pre-transaction state.</param>
    member this.rollback (transactionId: uint32) (rollbackPatch: Patch) =
        pendingCommits.Remove(transactionId) |> ignore
        this.applyPatch rollbackPatch

    /// <summary>
    /// Begin a new transaction, returning a unique transaction identifier.
    /// </summary>
    /// <returns>A new transaction ID.</returns>
    member _.beginTransaction() : uint32 =
        let txId = nextTransactionId
        nextTransactionId <- nextTransactionId + 1u
        txId

    /// <summary>Get all nodes currently in dirty state.</summary>
    member _.getDirtyNodes() : GhostNode list =
        [ for nodeId in dirtySet do
              match identityMap.TryGetValue(nodeId) with
              | true, node -> yield node
              | _ -> () ]

    /// <summary>Clear all dirty flags, resetting all dirty nodes to Clean state.</summary>
    member _.clearDirty() =
        for nodeId in dirtySet do
            match identityMap.TryGetValue(nodeId) with
            | true, node -> identityMap.[nodeId] <- GhostNode.markClean node
            | _ -> ()

        dirtySet.Clear()

    /// <summary>
    /// Serialize the full state (all nodes) to a GGBP binary payload.
    /// Produces a single patch containing ReplaceBranch ops for every node.
    /// </summary>
    /// <returns>Byte array containing the serialized state.</returns>
    member _.serialize() : byte array =
        let ops =
            [ for kvp in identityMap do
                  let payload =
                      System.Text.Encoding.UTF8.GetBytes(sprintf "%A" kvp.Value.Data)

                  yield PatchOp.ReplaceBranch(NodeId kvp.Key, payload) ]

        let patch =
            { Magic = GhostPatch.MagicByte
              Version = ModelVersion 0u
              TransactionId = TransactionId 0u
              Ops = ops }

        GhostPatch.encode patch

    /// <summary>
    /// Deserialize and restore full state from a GGBP binary payload.
    /// Clears existing state and rebuilds from the decoded patch.
    /// </summary>
    /// <param name="data">Byte array containing serialized state.</param>
    /// <exception cref="System.InvalidOperationException">Thrown when the payload cannot be decoded.</exception>
    member _.restore(data: byte array) =
        match GhostPatch.decode data with
        | PatchResult.Ok patch ->
            identityMap.Clear()
            dirtySet.Clear()

            for op in patch.Ops do
                match op with
                | PatchOp.ReplaceBranch(NodeId nodeId, payload) ->
                    let node = GhostNode.create nodeId (payload :> obj)
                    identityMap.[nodeId] <- node
                    dependencyGraph.addNode nodeId
                | _ -> ()
        | PatchResult.Corrupted msg -> invalidOp (sprintf "Failed to restore state: %s" msg)
        | PatchResult.InvalidMagic -> invalidOp "Failed to restore state: invalid magic byte"
        | PatchResult.VersionMismatch(expected, got) ->
            invalidOp (sprintf "Failed to restore state: version mismatch (expected %d, got %d)" expected got)
