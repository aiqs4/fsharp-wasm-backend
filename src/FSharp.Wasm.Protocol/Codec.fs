namespace FSharp.Wasm.Protocol

open System
open System.IO

/// <summary>
/// GGBP (Ghost-Graph Binary Patch) codec.
/// Provides encode/decode for the binary wire format and patch merging.
/// </summary>
[<RequireQualifiedAccess>]
module GhostPatch =

    /// <summary>Magic byte identifying a GGBP payload ('G' = 0x47).</summary>
    [<Literal>]
    let MagicByte: byte = 0x47uy

    // ── LEB128 varint helpers ────────────────────────────────────────

    /// <summary>Encode a non-negative integer as an unsigned LEB128 varint.</summary>
    let encodeVarint (value: int) : byte array =
        if value = 0 then
            [| 0uy |]
        else
            let buf = ResizeArray<byte>()
            let mutable v = value

            while v > 0 do
                let mutable b = byte (v &&& 0x7F)
                v <- v >>> 7

                if v > 0 then
                    b <- b ||| 0x80uy

                buf.Add(b)

            buf.ToArray()

    /// <summary>Decode an unsigned LEB128 varint from a BinaryReader.</summary>
    /// <returns>The decoded integer value.</returns>
    let decodeVarint (reader: BinaryReader) : int =
        let mutable result = 0
        let mutable shift = 0
        let mutable keepGoing = true

        while keepGoing do
            let b = reader.ReadByte()
            result <- result ||| (int (b &&& 0x7Fuy) <<< shift)
            shift <- shift + 7
            keepGoing <- (b &&& 0x80uy) <> 0uy

        result

    // ── Primitive encoding by WasmType ───────────────────────────────

    /// <summary>Encode a raw value blob prefixed with a 2-byte length.</summary>
    let private writeBlob (w: BinaryWriter) (data: byte array) =
        w.Write(uint16 data.Length)
        w.Write(data)

    /// <summary>Read a length-prefixed blob from a BinaryReader.</summary>
    let private readBlob (r: BinaryReader) : byte array =
        let len = r.ReadUInt16() |> int
        r.ReadBytes(len)

    // ── PatchOp tag bytes ────────────────────────────────────────────

    [<Literal>]
    let private TagUpdateLeaf: byte = 0x01uy

    [<Literal>]
    let private TagReplaceBranch: byte = 0x02uy

    [<Literal>]
    let private TagListMutation: byte = 0x03uy

    [<Literal>]
    let private TagDeleteNode: byte = 0x04uy

    // ── Encode ───────────────────────────────────────────────────────

    let private writeNodeId (w: BinaryWriter) (NodeId id) =
        let varBytes = encodeVarint id
        w.Write(varBytes)

    let private writeOp (w: BinaryWriter) (op: PatchOp) =
        match op with
        | PatchOp.UpdateLeaf(nodeId, FieldIndex fi, wt, value) ->
            w.Write(TagUpdateLeaf)
            writeNodeId w nodeId
            w.Write(fi)
            w.Write(byte wt)
            writeBlob w value

        | PatchOp.ReplaceBranch(nodeId, payload) ->
            w.Write(TagReplaceBranch)
            writeNodeId w nodeId
            writeBlob w payload

        | PatchOp.ListMutation(nodeId, FieldIndex fi, listOp, index, value) ->
            w.Write(TagListMutation)
            writeNodeId w nodeId
            w.Write(fi)
            w.Write(byte listOp)
            let idxBytes = encodeVarint index
            w.Write(idxBytes)
            writeBlob w value

        | PatchOp.DeleteNode nodeId ->
            w.Write(TagDeleteNode)
            writeNodeId w nodeId

    /// <summary>
    /// Encode a <see cref="Patch"/> into the GGBP binary wire format.
    /// Layout: Magic(1) | Version(4) | TransactionId(4) | OpCount(varint) | Ops…
    /// </summary>
    let encode (patch: Patch) : byte array =
        use ms = new MemoryStream()
        use w = new BinaryWriter(ms)
        w.Write(patch.Magic)
        let (ModelVersion v) = patch.Version
        w.Write(v)
        let (TransactionId tid) = patch.TransactionId
        w.Write(tid)
        let countBytes = encodeVarint patch.Ops.Length
        w.Write(countBytes)

        for op in patch.Ops do
            writeOp w op

        w.Flush()
        ms.ToArray()

    // ── Decode ───────────────────────────────────────────────────────

    let private readNodeId (r: BinaryReader) : NodeId = NodeId(decodeVarint r)

    let private readOp (r: BinaryReader) : PatchOp =
        let tag = r.ReadByte()

        match tag with
        | t when t = TagUpdateLeaf ->
            let nid = readNodeId r
            let fi = r.ReadByte()
            let wt = LanguagePrimitives.EnumOfValue<byte, WasmType>(r.ReadByte())
            let value = readBlob r
            PatchOp.UpdateLeaf(nid, FieldIndex fi, wt, value)

        | t when t = TagReplaceBranch ->
            let nid = readNodeId r
            let payload = readBlob r
            PatchOp.ReplaceBranch(nid, payload)

        | t when t = TagListMutation ->
            let nid = readNodeId r
            let fi = r.ReadByte()
            let listOp = LanguagePrimitives.EnumOfValue<byte, ListOp>(r.ReadByte())
            let index = decodeVarint r
            let value = readBlob r
            PatchOp.ListMutation(nid, FieldIndex fi, listOp, index, value)

        | t when t = TagDeleteNode ->
            let nid = readNodeId r
            PatchOp.DeleteNode nid

        | unknown -> failwithf "Unknown PatchOp tag: 0x%02X" unknown

    /// <summary>
    /// Decode a GGBP binary payload into a <see cref="PatchResult"/>.
    /// Validates the magic byte and catches structural errors.
    /// </summary>
    let decode (data: byte array) : PatchResult =
        if data.Length < 1 then
            PatchResult.Corrupted "Empty payload"
        elif data.[0] <> MagicByte then
            PatchResult.InvalidMagic
        else
            try
                use ms = new MemoryStream(data)
                use r = new BinaryReader(ms)
                let magic = r.ReadByte()
                let version = r.ReadUInt32()
                let txnId = r.ReadUInt32()
                let opCount = decodeVarint r

                let ops =
                    [ for _ in 1..opCount do
                          yield readOp r ]

                PatchResult.Ok
                    { Magic = magic
                      Version = ModelVersion version
                      TransactionId = TransactionId txnId
                      Ops = ops }
            with ex ->
                PatchResult.Corrupted ex.Message

    // ── Merge ────────────────────────────────────────────────────────

    /// <summary>
    /// Merge two patches into one by concatenating their ops.
    /// The resulting patch takes the higher model version and the
    /// transaction ID of the second (later) patch.
    /// </summary>
    let merge (a: Patch) (b: Patch) : Patch =
        let (ModelVersion va) = a.Version
        let (ModelVersion vb) = b.Version

        { Magic = MagicByte
          Version = ModelVersion(max va vb)
          TransactionId = b.TransactionId
          Ops = a.Ops @ b.Ops }
