namespace FSharp.Wasm.Ghost

open System.Collections.Generic

/// <summary>
/// Tracks directed dependencies between ghost nodes.
/// An edge from A to B means "A depends on B" — when B changes, A is affected.
/// </summary>
type DependencyGraph() =

    // Forward edges: nodeId -> set of nodes it depends on
    let dependencies = Dictionary<int, HashSet<int>>()

    // Reverse edges: nodeId -> set of nodes that depend on it
    let dependents = Dictionary<int, HashSet<int>>()

    let ensureNode (nodeId: int) =
        if not (dependencies.ContainsKey(nodeId)) then
            dependencies.[nodeId] <- HashSet<int>()

        if not (dependents.ContainsKey(nodeId)) then
            dependents.[nodeId] <- HashSet<int>()

    /// <summary>Register a node in the dependency graph.</summary>
    /// <param name="nodeId">The node identifier to add.</param>
    member _.addNode(nodeId: int) = ensureNode nodeId

    /// <summary>
    /// Add a dependency edge: <paramref name="from"/> depends on <paramref name="to_"/>.
    /// Both nodes are created if not already present.
    /// </summary>
    /// <param name="from">The dependent node.</param>
    /// <param name="to_">The dependency (upstream) node.</param>
    member _.addDependency(from: int, to_: int) =
        ensureNode from
        ensureNode to_
        dependencies.[from].Add(to_) |> ignore
        dependents.[to_].Add(from) |> ignore

    /// <summary>Remove a dependency edge from <paramref name="from"/> to <paramref name="to_"/>.</summary>
    member _.removeDependency(from: int, to_: int) =
        match dependencies.TryGetValue(from) with
        | true, deps -> deps.Remove(to_) |> ignore
        | _ -> ()

        match dependents.TryGetValue(to_) with
        | true, deps -> deps.Remove(from) |> ignore
        | _ -> ()

    /// <summary>Remove a node and all its edges from the graph.</summary>
    /// <param name="nodeId">The node to remove.</param>
    member _.removeNode(nodeId: int) =
        // Remove forward edges
        match dependencies.TryGetValue(nodeId) with
        | true, deps ->
            for dep in deps do
                match dependents.TryGetValue(dep) with
                | true, revSet -> revSet.Remove(nodeId) |> ignore
                | _ -> ()

            dependencies.Remove(nodeId) |> ignore
        | _ -> ()

        // Remove reverse edges
        match dependents.TryGetValue(nodeId) with
        | true, deps ->
            for dep in deps do
                match dependencies.TryGetValue(dep) with
                | true, fwdSet -> fwdSet.Remove(nodeId) |> ignore
                | _ -> ()

            dependents.Remove(nodeId) |> ignore
        | _ -> ()

    /// <summary>
    /// Get nodes that directly depend on the given node (one level up).
    /// </summary>
    /// <param name="nodeId">The upstream node.</param>
    /// <returns>List of node ids that depend on <paramref name="nodeId"/>.</returns>
    member _.getDependents(nodeId: int) : int list =
        match dependents.TryGetValue(nodeId) with
        | true, deps -> deps |> Seq.toList
        | _ -> []

    /// <summary>
    /// Get nodes that the given node directly depends on (one level down).
    /// </summary>
    /// <param name="nodeId">The dependent node.</param>
    /// <returns>List of node ids that <paramref name="nodeId"/> depends on.</returns>
    member _.getDependencies(nodeId: int) : int list =
        match dependencies.TryGetValue(nodeId) with
        | true, deps -> deps |> Seq.toList
        | _ -> []

    /// <summary>
    /// Walk upward from the given node and collect all transitive dependents.
    /// Uses breadth-first traversal to avoid cycles.
    /// </summary>
    /// <param name="nodeId">The starting node.</param>
    /// <returns>Set of all node ids that transitively depend on <paramref name="nodeId"/>.</returns>
    member this.getTransitiveDependents(nodeId: int) : int Set =
        let visited = HashSet<int>()
        let queue = Queue<int>()
        queue.Enqueue(nodeId)

        while queue.Count > 0 do
            let current = queue.Dequeue()

            for dep in this.getDependents(current) do
                if visited.Add(dep) then
                    queue.Enqueue(dep)

        visited |> Set.ofSeq

    /// <summary>
    /// Compute a topological ordering of all nodes in the graph.
    /// Nodes with no dependencies appear first.
    /// </summary>
    /// <returns>A list of node ids in topological order.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the graph contains a cycle.</exception>
    member _.topologicalSort() : int list =
        let inDegree = Dictionary<int, int>()

        for kvp in dependencies do
            if not (inDegree.ContainsKey(kvp.Key)) then
                inDegree.[kvp.Key] <- 0

            for dep in kvp.Value do
                if not (inDegree.ContainsKey(dep)) then
                    inDegree.[dep] <- 0

        // Calculate in-degree based on forward dependency edges (reversed)
        for kvp in dependencies do
            for _ in kvp.Value do
                inDegree.[kvp.Key] <- inDegree.[kvp.Key] + 1

        let queue = Queue<int>()

        for kvp in inDegree do
            if kvp.Value = 0 then
                queue.Enqueue(kvp.Key)

        let result = ResizeArray<int>()

        while queue.Count > 0 do
            let node = queue.Dequeue()
            result.Add(node)

            // For each node that depends on 'node', decrease its in-degree
            match dependents.TryGetValue(node) with
            | true, deps ->
                for dep in deps do
                    inDegree.[dep] <- inDegree.[dep] - 1

                    if inDegree.[dep] = 0 then
                        queue.Enqueue(dep)
            | _ -> ()

        if result.Count <> inDegree.Count then
            invalidOp "Dependency graph contains a cycle"

        result |> Seq.toList
