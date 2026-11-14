module FSharp.Wasm.Tests.ProtocolTests

open Xunit
open FSharp.Wasm.Protocol

// ── GhostPatch codec ────────────────────────────────────────────────────────

[<Fact>]
let ``MagicByte is 0x47`` () =
    Assert.Equal(0x47uy, GhostPatch.MagicByte)

[<Fact>]
let ``encode empty patch round-trips via decode`` () =
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 1u
          TransactionId = TransactionId 42u
          Ops = [] }

    let bytes = GhostPatch.encode patch

    match GhostPatch.decode bytes with
    | PatchResult.Ok decoded ->
        Assert.Equal(GhostPatch.MagicByte, decoded.Magic)
        Assert.Equal(ModelVersion 1u, decoded.Version)
        Assert.Equal(TransactionId 42u, decoded.TransactionId)
        Assert.Empty(decoded.Ops)
    | other -> Assert.True(false, sprintf "Expected Ok but got %A" other)

[<Fact>]
let ``encode UpdateLeaf round-trips via decode`` () =
    let value = [| 0x01uy; 0x02uy; 0x03uy |]

    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 2u
          TransactionId = TransactionId 7u
          Ops =
            [ PatchOp.UpdateLeaf(NodeId 1, FieldIndex 3uy, WasmType.I32, value) ] }

    match GhostPatch.decode (GhostPatch.encode patch) with
    | PatchResult.Ok decoded ->
        Assert.Single(decoded.Ops) |> ignore

        match decoded.Ops.[0] with
        | PatchOp.UpdateLeaf(NodeId nid, FieldIndex fi, wt, v) ->
            Assert.Equal(1, nid)
            Assert.Equal(3uy, fi)
            Assert.Equal(WasmType.I32, wt)
            Assert.Equal<byte array>(value, v)
        | other -> Assert.True(false, sprintf "Unexpected op: %A" other)
    | other -> Assert.True(false, sprintf "Expected Ok but got %A" other)

[<Fact>]
let ``encode ReplaceBranch round-trips via decode`` () =
    let payload = System.Text.Encoding.UTF8.GetBytes "hello"

    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [ PatchOp.ReplaceBranch(NodeId 99, payload) ] }

    match GhostPatch.decode (GhostPatch.encode patch) with
    | PatchResult.Ok decoded ->
        match decoded.Ops.[0] with
        | PatchOp.ReplaceBranch(NodeId nid, pl) ->
            Assert.Equal(99, nid)
            Assert.Equal<byte array>(payload, pl)
        | other -> Assert.True(false, sprintf "Unexpected op: %A" other)
    | other -> Assert.True(false, sprintf "Expected Ok but got %A" other)

[<Fact>]
let ``encode DeleteNode round-trips via decode`` () =
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [ PatchOp.DeleteNode(NodeId 5) ] }

    match GhostPatch.decode (GhostPatch.encode patch) with
    | PatchResult.Ok decoded ->
        match decoded.Ops.[0] with
        | PatchOp.DeleteNode(NodeId nid) -> Assert.Equal(5, nid)
        | other -> Assert.True(false, sprintf "Unexpected op: %A" other)
    | other -> Assert.True(false, sprintf "Expected Ok but got %A" other)

[<Fact>]
let ``encode ListMutation round-trips via decode`` () =
    let value = [| 0xFFuy |]

    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops = [ PatchOp.ListMutation(NodeId 10, FieldIndex 2uy, ListOp.Insert, 3, value) ] }

    match GhostPatch.decode (GhostPatch.encode patch) with
    | PatchResult.Ok decoded ->
        match decoded.Ops.[0] with
        | PatchOp.ListMutation(NodeId nid, FieldIndex fi, lop, idx, v) ->
            Assert.Equal(10, nid)
            Assert.Equal(2uy, fi)
            Assert.Equal(ListOp.Insert, lop)
            Assert.Equal(3, idx)
            Assert.Equal<byte array>(value, v)
        | other -> Assert.True(false, sprintf "Unexpected op: %A" other)
    | other -> Assert.True(false, sprintf "Expected Ok but got %A" other)

[<Fact>]
let ``decode returns InvalidMagic for wrong magic byte`` () =
    let bytes = [| 0x00uy; 0x01uy; 0x02uy |]
    Assert.Equal(PatchResult.InvalidMagic, GhostPatch.decode bytes)

[<Fact>]
let ``decode returns Corrupted for empty payload`` () =
    match GhostPatch.decode [||] with
    | PatchResult.Corrupted _ -> ()
    | other -> Assert.True(false, sprintf "Expected Corrupted but got %A" other)

[<Fact>]
let ``merge combines ops and takes higher version`` () =
    let a =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 3u
          TransactionId = TransactionId 1u
          Ops = [ PatchOp.DeleteNode(NodeId 1) ] }

    let b =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 5u
          TransactionId = TransactionId 2u
          Ops = [ PatchOp.DeleteNode(NodeId 2) ] }

    let merged = GhostPatch.merge a b
    Assert.Equal(ModelVersion 5u, merged.Version)
    Assert.Equal(TransactionId 2u, merged.TransactionId)
    Assert.Equal(2, merged.Ops.Length)

[<Fact>]
let ``encodeVarint and decodeVarint round-trip`` () =
    for value in [ 0; 1; 127; 128; 255; 1000; 16383; 16384 ] do
        use ms = new System.IO.MemoryStream(GhostPatch.encodeVarint value)
        use r = new System.IO.BinaryReader(ms)
        Assert.Equal(value, GhostPatch.decodeVarint r)

// ── VersionManager ───────────────────────────────────────────────────────────

[<Fact>]
let ``VersionManager create starts at zero`` () =
    let state = VersionManager.create ()
    Assert.Equal(0u, state.CurrentVersion)
    Assert.Equal(0u, state.LastAcknowledged)

[<Fact>]
let ``VersionManager increment advances version`` () =
    let state = VersionManager.create () |> VersionManager.increment
    Assert.Equal(1u, state.CurrentVersion)

[<Fact>]
let ``VersionManager shouldResync returns true when gap is more than one`` () =
    Assert.True(VersionManager.shouldResync 5u 7u)

[<Fact>]
let ``VersionManager shouldResync returns false for adjacent version`` () =
    Assert.False(VersionManager.shouldResync 5u 6u)

[<Fact>]
let ``VersionManager validatePatch accepts expected next version`` () =
    let state = VersionManager.create () |> VersionManager.increment // v = 1
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 2u
          TransactionId = TransactionId 0u
          Ops = [] }
    match VersionManager.validatePatch state patch with
    | Ok newState -> Assert.Equal(2u, newState.CurrentVersion)
    | Error msg -> Assert.True(false, msg)

[<Fact>]
let ``VersionManager validatePatch rejects stale patch`` () =
    let state = { VersionManager.create () with CurrentVersion = 5u; LastAcknowledged = 0u }
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 3u
          TransactionId = TransactionId 0u
          Ops = [] }
    match VersionManager.validatePatch state patch with
    | Error _ -> ()
    | Ok _ -> Assert.True(false, "Expected Error but got Ok")

// ── InMemoryTransport ────────────────────────────────────────────────────────

[<Fact>]
let ``InMemoryTransport send and receive round-trip`` () =
    use transport = new InMemoryTransport()
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 1u
          TransactionId = TransactionId 10u
          Ops = [] }
    let t = transport :> ITransport
    t.Send(patch) |> Async.RunSynchronously
    let received = t.Receive(System.Threading.CancellationToken.None) |> Async.RunSynchronously
    Assert.Equal(Some patch, received)
