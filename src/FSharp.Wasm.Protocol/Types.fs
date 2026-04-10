namespace FSharp.Wasm.Protocol

open System

/// <summary>Uniquely identifies a node in the ghost graph.</summary>
[<Struct>]
type NodeId = NodeId of int

/// <summary>Identifies a transaction for ordered patch delivery.</summary>
[<Struct>]
type TransactionId = TransactionId of uint32

/// <summary>Monotonically increasing model version for optimistic concurrency.</summary>
[<Struct>]
type ModelVersion = ModelVersion of uint32

/// <summary>Index of a field within a record type (0–255).</summary>
[<Struct>]
type FieldIndex = FieldIndex of byte

/// <summary>Identifies a record type in the field map (0–65535).</summary>
[<Struct>]
type RecordId = RecordId of uint16

/// <summary>Wasm-compatible value types used in the ghost graph.</summary>
type WasmType =
    | I32 = 0uy
    | I64 = 1uy
    | F32 = 2uy
    | F64 = 3uy
    | RefStruct = 4uy
    | RefArray = 5uy
    | RefString = 6uy
    | RefFunc = 7uy
    | RefAny = 8uy

/// <summary>Mutation operations on list-typed fields.</summary>
type ListOp =
    | Insert = 0uy
    | Remove = 1uy
    | Update = 2uy
    | Clear = 3uy

/// <summary>An individual patch operation targeting a node in the ghost graph.</summary>
[<RequireQualifiedAccess>]
type PatchOp =
    /// <summary>Update a single leaf field on a node.</summary>
    | UpdateLeaf of nodeId: NodeId * fieldIdx: FieldIndex * wasmType: WasmType * value: byte array
    /// <summary>Replace an entire branch sub-tree rooted at a node.</summary>
    | ReplaceBranch of nodeId: NodeId * payload: byte array
    /// <summary>Mutate a list-typed field on a node.</summary>
    | ListMutation of nodeId: NodeId * fieldIdx: FieldIndex * listOp: ListOp * index: int * value: byte array
    /// <summary>Delete a node and all its descendants.</summary>
    | DeleteNode of nodeId: NodeId

/// <summary>A complete GGBP patch containing a header and a list of operations.</summary>
type Patch =
    { /// <summary>Magic byte identifying the GGBP format (0x47 = 'G').</summary>
      Magic: byte
      /// <summary>Model version this patch was created against.</summary>
      Version: ModelVersion
      /// <summary>Transaction identifier for ordering and acknowledgement.</summary>
      TransactionId: TransactionId
      /// <summary>Ordered list of patch operations.</summary>
      Ops: PatchOp list }

/// <summary>Result of decoding a GGBP binary payload.</summary>
[<RequireQualifiedAccess>]
type PatchResult =
    /// <summary>Successfully decoded patch.</summary>
    | Ok of Patch
    /// <summary>Version mismatch between expected and received versions.</summary>
    | VersionMismatch of expected: uint32 * got: uint32
    /// <summary>The magic byte did not match the expected GGBP magic.</summary>
    | InvalidMagic
    /// <summary>The binary payload is structurally invalid.</summary>
    | Corrupted of string

/// <summary>Maps a field name hash to its index and Wasm type within a record.</summary>
type FieldMapEntry =
    { /// <summary>FNV-1a hash of the field name.</summary>
      FieldNameHash: uint32
      /// <summary>Positional index of the field.</summary>
      FieldIdx: byte
      /// <summary>Wasm type of the field value.</summary>
      WasmType: WasmType }

/// <summary>Field map for a single record type.</summary>
type RecordMap =
    { /// <summary>Identifier of the record type.</summary>
      RecordId: RecordId
      /// <summary>Ordered fields of the record.</summary>
      Fields: FieldMapEntry array }

/// <summary>Complete field map covering all known record types.</summary>
type FieldMap =
    { /// <summary>All record type mappings.</summary>
      Records: RecordMap array }

/// <summary>Messages exchanged over the GGBP transport layer.</summary>
[<RequireQualifiedAccess>]
type TransportMessage =
    /// <summary>A patch to apply to the ghost graph.</summary>
    | PatchMessage of Patch
    /// <summary>Full state resync payload.</summary>
    | FullResync of byte array
    /// <summary>Acknowledge successful application of a transaction.</summary>
    | Ack of TransactionId
    /// <summary>Negative acknowledgement with the conflicting patch.</summary>
    | Nack of TransactionId * Patch
    /// <summary>Keep-alive ping.</summary>
    | Ping
    /// <summary>Keep-alive pong.</summary>
    | Pong
