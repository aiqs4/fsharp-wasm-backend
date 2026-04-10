namespace FSharp.Wasm.Sdk

open System
open FSharp.Wasm.Protocol

/// <summary>Compile-time field map generation from record definitions.</summary>
module FieldMapGenerator =

    /// <summary>A record type definition for field map generation.</summary>
    type RecordDefinition =
        { /// <summary>Record type name.</summary>
          Name: string
          /// <summary>Unique record identifier.</summary>
          RecordId: uint16
          /// <summary>Fields as (fieldName, fieldIdx, wasmType) tuples.</summary>
          Fields: (string * byte * WasmType) list }

    /// <summary>Compute the FNV-1a hash of a field name string.</summary>
    /// <param name="name">The field name to hash.</param>
    /// <returns>32-bit FNV-1a hash.</returns>
    let hashFieldName (name: string) : uint32 =
        let fnvOffsetBasis = 2166136261u
        let fnvPrime = 16777619u
        let bytes = Text.Encoding.UTF8.GetBytes(name)
        let mutable hash = fnvOffsetBasis
        for b in bytes do
            hash <- hash ^^^ (uint32 b)
            hash <- hash * fnvPrime
        hash

    /// <summary>Generate a FieldMap from a list of record definitions.</summary>
    /// <param name="definitions">Record definitions to include in the field map.</param>
    /// <returns>A FieldMap containing all record type mappings.</returns>
    let generateFieldMap (definitions: RecordDefinition list) : FieldMap =
        let records =
            definitions
            |> List.map (fun def ->
                let fields =
                    def.Fields
                    |> List.map (fun (fieldName, fieldIdx, wasmType) ->
                        { FieldNameHash = hashFieldName fieldName
                          FieldIdx = fieldIdx
                          WasmType = wasmType })
                    |> List.toArray
                { RecordId = RecordId def.RecordId
                  Fields = fields })
            |> List.toArray
        { Records = records }

    /// <summary>Encode a FieldMap to binary format.
    /// Format: record_count (uint16), then per record: record_id (uint16),
    /// field_count (uint16), then per field: field_name_hash (uint32),
    /// field_idx (byte), wasm_type (byte).</summary>
    /// <param name="fieldMap">The field map to encode.</param>
    /// <returns>Binary representation of the field map.</returns>
    let encodeFieldMap (fieldMap: FieldMap) : byte array =
        let estimatedSize =
            2 + fieldMap.Records.Length * (2 + 2 + 6 * 10)
        let buffer = Collections.Generic.List<byte>(estimatedSize)

        let writeUint16 (v: uint16) =
            buffer.Add(byte (v &&& 0xFFus))
            buffer.Add(byte ((v >>> 8) &&& 0xFFus))

        let writeUint32 (v: uint32) =
            buffer.Add(byte (v &&& 0xFFu))
            buffer.Add(byte ((v >>> 8) &&& 0xFFu))
            buffer.Add(byte ((v >>> 16) &&& 0xFFu))
            buffer.Add(byte ((v >>> 24) &&& 0xFFu))

        writeUint16 (uint16 fieldMap.Records.Length)
        for record in fieldMap.Records do
            let (RecordId rid) = record.RecordId
            writeUint16 rid
            writeUint16 (uint16 record.Fields.Length)
            for field in record.Fields do
                writeUint32 field.FieldNameHash
                buffer.Add(field.FieldIdx)
                buffer.Add(byte field.WasmType)

        buffer.ToArray()

    /// <summary>Decode a FieldMap from binary format.</summary>
    /// <param name="data">Binary data to decode.</param>
    /// <returns>The decoded FieldMap.</returns>
    let decodeFieldMap (data: byte array) : FieldMap =
        let mutable offset = 0

        let readUint16 () =
            let v = uint16 data.[offset] ||| (uint16 data.[offset + 1] <<< 8)
            offset <- offset + 2
            v

        let readUint32 () =
            let v =
                uint32 data.[offset]
                ||| (uint32 data.[offset + 1] <<< 8)
                ||| (uint32 data.[offset + 2] <<< 16)
                ||| (uint32 data.[offset + 3] <<< 24)
            offset <- offset + 4
            v

        let recordCount = int (readUint16())
        let records = Array.zeroCreate recordCount
        for i in 0 .. recordCount - 1 do
            let rid = readUint16()
            let fieldCount = int (readUint16())
            let fields = Array.zeroCreate fieldCount
            for j in 0 .. fieldCount - 1 do
                let hash = readUint32()
                let fieldIdx = data.[offset]
                offset <- offset + 1
                let wasmType = data.[offset]
                offset <- offset + 1
                fields.[j] <-
                    { FieldNameHash = hash
                      FieldIdx = fieldIdx
                      WasmType = LanguagePrimitives.EnumOfValue wasmType }
            records.[i] <-
                { RecordId = RecordId rid
                  Fields = fields }
        { Records = records }
