module FSharp.Wasm.Tests.GhostTests

open Xunit
open FSharp.Wasm.Protocol
open FSharp.Wasm.Ghost

// ── GhostNode ────────────────────────────────────────────────────────────────

[<Fact>]
let ``GhostNode create starts clean at version 1`` () =
    let node = GhostNode.create 1 ("data" :> obj)
    Assert.Equal(1, node.Id)
    Assert.Equal(1, node.Version)
    Assert.Equal(DirtyState.Clean, node.Dirty)

[<Fact>]
let ``GhostNode markDirty sets Pending state`` () =
    let node = GhostNode.create 1 (obj()) |> GhostNode.markDirty
    Assert.Equal(DirtyState.Pending, node.Dirty)

[<Fact>]
let ``GhostNode markCommitted sets Committed state`` () =
    let node = GhostNode.create 1 (obj()) |> GhostNode.markCommitted
    Assert.Equal(DirtyState.Committed, node.Dirty)

[<Fact>]
let ``GhostNode markClean resets to Clean`` () =
    let node = GhostNode.create 1 (obj()) |> GhostNode.markDirty |> GhostNode.markClean
    Assert.Equal(DirtyState.Clean, node.Dirty)

[<Fact>]
let ``GhostNode incrementVersion increments by one`` () =
    let node = GhostNode.create 1 (obj()) |> GhostNode.incrementVersion
    Assert.Equal(2, node.Version)

[<Fact>]
let ``GhostNode updateData replaces payload`` () =
    let node = GhostNode.create 1 ("old" :> obj) |> GhostNode.updateData ("new" :> obj)
    Assert.Equal("new", node.Data :?> string)

// ── DependencyGraph ──────────────────────────────────────────────────────────

[<Fact>]
let ``DependencyGraph addNode and getDependents returns empty initially`` () =
    let g = DependencyGraph()
    g.addNode 1
    Assert.Empty(g.getDependents 1)

[<Fact>]
let ``DependencyGraph addDependency propagates correctly`` () =
    let g = DependencyGraph()
    g.addNode 1
    g.addNode 2
    // 2 depends on 1 → 1's dependents includes 2
    g.addDependency(2, 1)
    Assert.Contains(2, g.getDependents 1)

[<Fact>]
let ``DependencyGraph getTransitiveDependents walks multiple levels`` () =
    let g = DependencyGraph()
    g.addNode 1
    g.addNode 2
    g.addNode 3
    g.addDependency(2, 1)
    g.addDependency(3, 2)
    let deps = g.getTransitiveDependents 1
    Assert.Contains(2, deps)
    Assert.Contains(3, deps)

[<Fact>]
let ``DependencyGraph removeNode cleans up`` () =
    let g = DependencyGraph()
    g.addNode 1
    g.addNode 2
    g.addDependency(2, 1)
    g.removeNode 1
    Assert.Empty(g.getDependents 1)

[<Fact>]
let ``DependencyGraph removeDependency removes edge`` () =
    let g = DependencyGraph()
    g.addNode 1
    g.addNode 2
    g.addDependency(2, 1)
    g.removeDependency(2, 1)
    Assert.Empty(g.getDependents 1)

// ── ShadowManager ────────────────────────────────────────────────────────────

[<Fact>]
let ``ShadowManager registerNode adds node to identity map`` () =
    let mgr = ShadowManager()
    let node = mgr.registerNode 1 ("hello" :> obj)
    Assert.Equal(1, node.Id)
    Assert.Equal(1, mgr.NodeCount)

[<Fact>]
let ``ShadowManager getNode returns registered node`` () =
    let mgr = ShadowManager()
    mgr.registerNode 42 (obj()) |> ignore
    match mgr.getNode 42 with
    | Some n -> Assert.Equal(42, n.Id)
    | None -> Assert.True(false, "Node not found")

[<Fact>]
let ``ShadowManager getNode returns None for unknown id`` () =
    let mgr = ShadowManager()
    Assert.Equal(None, mgr.getNode 999)

[<Fact>]
let ``ShadowManager signal marks node dirty`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 (obj()) |> ignore
    mgr.signal 1 0
    Assert.Equal(1, mgr.DirtyCount)
    match mgr.getNode 1 with
    | Some n -> Assert.Equal(DirtyState.Pending, n.Dirty)
    | None -> Assert.True(false, "Node not found")

[<Fact>]
let ``ShadowManager signal propagates dirty to dependents`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 (obj()) |> ignore
    mgr.registerNode 2 (obj()) |> ignore
    mgr.DependencyGraph.addDependency(2, 1)
    mgr.signal 1 0
    Assert.Equal(2, mgr.DirtyCount)

[<Fact>]
let ``ShadowManager clearDirty resets all dirty nodes to Clean`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 (obj()) |> ignore
    mgr.signal 1 0
    mgr.clearDirty()
    Assert.Equal(0, mgr.DirtyCount)
    match mgr.getNode 1 with
    | Some n -> Assert.Equal(DirtyState.Clean, n.Dirty)
    | None -> Assert.True(false, "Node not found")

[<Fact>]
let ``ShadowManager currentDirtyPatch contains ops for dirty nodes`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 (obj()) |> ignore
    mgr.signal 1 0
    let patch = mgr.currentDirtyPatch()
    Assert.Equal(GhostPatch.MagicByte, patch.Magic)
    Assert.Equal(1, patch.Ops.Length)

[<Fact>]
let ``ShadowManager serialize and restore round-trip`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 ("payload" :> obj) |> ignore
    mgr.registerNode 2 ("payload2" :> obj) |> ignore
    let bytes = mgr.serialize()

    let mgr2 = ShadowManager()
    mgr2.restore bytes
    Assert.Equal(2, mgr2.NodeCount)

[<Fact>]
let ``ShadowManager applyPatch creates node when not present`` () =
    let mgr = ShadowManager()
    let payload = System.Text.Encoding.UTF8.GetBytes "data"
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [ PatchOp.ReplaceBranch(NodeId 5, payload) ] }
    mgr.applyPatch patch
    match mgr.getNode 5 with
    | Some _ -> ()
    | None -> Assert.True(false, "Node should have been created by applyPatch")

[<Fact>]
let ``ShadowManager applyPatch DeleteNode removes node`` () =
    let mgr = ShadowManager()
    mgr.registerNode 7 (obj()) |> ignore
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [ PatchOp.DeleteNode(NodeId 7) ] }
    mgr.applyPatch patch
    Assert.Equal(None, mgr.getNode 7)

[<Fact>]
let ``ShadowManager beginTransaction returns incrementing ids`` () =
    let mgr = ShadowManager()
    let t1 = mgr.beginTransaction()
    let t2 = mgr.beginTransaction()
    Assert.True(t2 > t1)

[<Fact>]
let ``ShadowManager commit clears dirty set`` () =
    let mgr = ShadowManager()
    mgr.registerNode 1 (obj()) |> ignore
    mgr.signal 1 0
    mgr.commit 1u
    Assert.Equal(0, mgr.DirtyCount)

// ── SubscriptionManager ──────────────────────────────────────────────────────

[<Fact>]
let ``SubscriptionManager subscribe and notify delivers patch`` () =
    let smgr = SubscriptionManager()
    let mutable received: Patch option = None

    smgr.subscribe "model.chat" "user1" (fun p -> received <- Some p)

    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [] }

    smgr.notify "model.chat" patch
    Assert.Equal(Some patch, received)

[<Fact>]
let ``SubscriptionManager unsubscribe stops delivery`` () =
    let smgr = SubscriptionManager()
    let mutable count = 0
    smgr.subscribe "model.x" "u1" (fun _ -> count <- count + 1)
    smgr.unsubscribe "u1" "model.x"

    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [] }

    smgr.notify "model.x" patch
    Assert.Equal(0, count)
