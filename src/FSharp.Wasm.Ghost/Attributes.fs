namespace FSharp.Wasm.Ghost

open System

/// <summary>Marks a type for ghost-graph tracking and automatic checkpoint serialization.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
type CheckpointAttribute() =
    inherit Attribute()

/// <summary>Marks a function or type as edge-only, excluded from Wasm compilation.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type EdgeOnlyAttribute() =
    inherit Attribute()

/// <summary>Marks a function or type as safe for client-side execution.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type ClientOkAttribute() =
    inherit Attribute()

/// <summary>Marks a function as a Wasm export entry point.</summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type WasmEntryAttribute() =
    inherit Attribute()

/// <summary>Opts a type or member out of ghost-graph tracking.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct ||| AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type GhostIgnoreAttribute() =
    inherit Attribute()
