namespace FSharp.Wasm.Contract

open System

/// <summary>
/// Marks a discriminated union type as a contract API definition.
/// Types annotated with this attribute are used by code generators
/// to produce REST, GraphQL, gRPC, and TypeScript contracts.
/// </summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
type ContractAttribute() =
    inherit Attribute()

/// <summary>
/// Overrides the auto-generated route for a contract endpoint.
/// </summary>
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type RouteAttribute(method: string, path: string) =
    inherit Attribute()

    /// <summary>The HTTP method (GET, POST, PUT, DELETE, PATCH).</summary>
    member _.Method = method

    /// <summary>The URL path pattern (e.g., "/api/users/:id").</summary>
    member _.Path = path

/// <summary>
/// Adds a human-readable description to a contract element
/// for inclusion in generated documentation and schemas.
/// </summary>
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type DescriptionAttribute(text: string) =
    inherit Attribute()

    /// <summary>The description text.</summary>
    member _.Text = text

/// <summary>
/// Marks a contract endpoint or type as deprecated,
/// causing generators to emit deprecation notices in output.
/// </summary>
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type DeprecatedAttribute(reason: string) =
    inherit Attribute()

    /// <summary>The reason for deprecation.</summary>
    member _.Reason = reason
