namespace FSharp.Wasm.Auth

open System

/// <summary>
/// Marks a function as requiring a specific capability or role.
/// Authorization is enforced as a compile-time property in capability-based security.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type RequiresAttribute(capability: string) =
    inherit Attribute()

    /// <summary>Gets the capability required to invoke the annotated function.</summary>
    member _.Capability = capability

/// <summary>
/// Marks a function as requiring any one of the listed capabilities.
/// The caller must possess at least one of the specified capabilities.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type RequiresAnyAttribute([<ParamArray>] capabilities: string array) =
    inherit Attribute()

    /// <summary>Gets the set of capabilities, any one of which satisfies the requirement.</summary>
    member _.Capabilities = capabilities

/// <summary>
/// Marks a function as requiring all of the listed capabilities.
/// The caller must possess every specified capability.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type RequiresAllAttribute([<ParamArray>] capabilities: string array) =
    inherit Attribute()

    /// <summary>Gets the set of capabilities that must all be present.</summary>
    member _.Capabilities = capabilities

/// <summary>
/// Explicitly marks a function as not requiring authentication.
/// Use this to document intentionally public endpoints.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type AnonymousAttribute() =
    inherit Attribute()

/// <summary>
/// Opts out of auth tracing and logging for the annotated function.
/// Use on high-frequency endpoints where auth logging would be excessive.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type NoTraceAttribute() =
    inherit Attribute()
