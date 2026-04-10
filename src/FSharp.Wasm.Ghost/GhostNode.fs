namespace FSharp.Wasm.Ghost

/// <summary>Tracks the dirty state of a ghost node through its lifecycle.</summary>
type DirtyState =
    /// <summary>Node is in sync and unchanged.</summary>
    | Clean = 0
    /// <summary>Node has local modifications not yet sent.</summary>
    | Pending = 1
    /// <summary>Node changes have been sent and acknowledged.</summary>
    | Committed = 2

/// <summary>
/// An immutable node in the ghost graph, representing a tracked piece of state
/// with version and dirty information.
/// </summary>
type GhostNode =
    { /// <summary>Unique identifier for this node.</summary>
      Id: int
      /// <summary>Monotonically increasing version counter.</summary>
      Version: int
      /// <summary>The data payload associated with this node.</summary>
      Data: obj
      /// <summary>Current dirty state of the node.</summary>
      Dirty: DirtyState }

/// <summary>Functions for creating and manipulating ghost nodes.</summary>
[<RequireQualifiedAccess>]
module GhostNode =

    /// <summary>Create a new ghost node with the given id and data, starting at version 1 in Clean state.</summary>
    /// <param name="id">Unique node identifier.</param>
    /// <param name="data">Initial data payload.</param>
    /// <returns>A new <see cref="GhostNode"/> instance.</returns>
    let create (id: int) (data: obj) : GhostNode =
        { Id = id
          Version = 1
          Data = data
          Dirty = DirtyState.Clean }

    /// <summary>Mark a node as having unsent local modifications.</summary>
    let markDirty (node: GhostNode) : GhostNode =
        { node with Dirty = DirtyState.Pending }

    /// <summary>Mark a node as pending (commit in flight, awaiting acknowledgement).</summary>
    let markPending (node: GhostNode) : GhostNode =
        { node with Dirty = DirtyState.Pending }

    /// <summary>Mark a node as committed (changes acknowledged).</summary>
    let markCommitted (node: GhostNode) : GhostNode =
        { node with Dirty = DirtyState.Committed }

    /// <summary>Mark a node as clean (fully in sync).</summary>
    let markClean (node: GhostNode) : GhostNode =
        { node with Dirty = DirtyState.Clean }

    /// <summary>Replace the data payload of a node.</summary>
    /// <param name="data">New data payload.</param>
    /// <param name="node">The node to update.</param>
    /// <returns>A new node with updated data.</returns>
    let updateData (data: obj) (node: GhostNode) : GhostNode =
        { node with Data = data }

    /// <summary>Increment the version counter of a node by one.</summary>
    let incrementVersion (node: GhostNode) : GhostNode =
        { node with Version = node.Version + 1 }
