namespace FSharp.Wasm.Host

open System
open System.Collections.Generic

/// <summary>A key–value pair representing an environment variable.</summary>
type EnvVar =
    { /// <summary>Variable name.</summary>
      Key: string
      /// <summary>Variable value.</summary>
      Value: string }

/// <summary>
/// Abstract host environment providing access to configuration variables.
/// Platform adapters inherit from this class to expose host-specific
/// environment variables (e.g. Cloudflare secrets, Spin config).
/// </summary>
[<AbstractClass>]
type Env() =

    /// <summary>Look up a variable by key. Returns None when the key is absent.</summary>
    abstract member getVar: key: string -> string option

    /// <summary>Return all variables known to this environment.</summary>
    abstract member getAllVars: unit -> EnvVar list

    /// <summary>
    /// Look up a variable by key, throwing <see cref="KeyNotFoundException"/>
    /// when the key is absent.
    /// </summary>
    member this.getVarRequired(key: string) : string =
        match this.getVar key with
        | Some v -> v
        | None -> raise (KeyNotFoundException(sprintf "Required environment variable '%s' is not set" key))

/// <summary>
/// In-memory <see cref="Env"/> backed by a dictionary.
/// Useful for unit testing and local development.
/// </summary>
type MemoryEnv() =
    inherit Env()

    let store = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

    /// <summary>Set or overwrite a variable.</summary>
    member _.Set(key: string, value: string) = store.[key] <- value

    /// <summary>Remove a variable. Returns true if it was present.</summary>
    member _.Remove(key: string) = store.Remove(key)

    override _.getVar(key: string) =
        match store.TryGetValue(key) with
        | true, v -> Some v
        | _ -> None

    override _.getAllVars() =
        [ for kv in store do
              { Key = kv.Key; Value = kv.Value } ]

/// <summary>Identifies the host platform running the Wasm module.</summary>
[<RequireQualifiedAccess>]
type PlatformKind =
    /// <summary>Cloudflare Workers.</summary>
    | Cloudflare
    /// <summary>Fastly Compute.</summary>
    | Fastly
    /// <summary>Fermyon Spin.</summary>
    | Fermyon
    /// <summary>WasmEdge runtime.</summary>
    | WasmEdge
    /// <summary>Node.js with WASI support.</summary>
    | NodeJs
    /// <summary>Browser with WebAssembly.</summary>
    | Browser
    /// <summary>An unrecognised platform.</summary>
    | Unknown of string
