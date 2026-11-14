namespace FSharp.Wasm.Sdk

open System

/// <summary>Deployment target for the Wasm build.</summary>
type WasmTarget =
    /// <summary>Client-side only deployment.</summary>
    | Client
    /// <summary>Edge-side only deployment.</summary>
    | Edge
    /// <summary>Both client and edge deployment with ghost sync.</summary>
    | Both

/// <summary>Target Wasm platform runtime.</summary>
type WasmPlatform =
    /// <summary>Cloudflare Workers.</summary>
    | Cloudflare
    /// <summary>Fastly Compute@Edge.</summary>
    | Fastly
    /// <summary>Fermyon Spin.</summary>
    | Fermyon
    /// <summary>WasmEdge runtime.</summary>
    | WasmEdge
    /// <summary>Generic WASI-compatible runtime.</summary>
    | Generic

/// <summary>Wasm GC feature flags controlling which GC proposals are enabled.</summary>
type WasmGcFlags =
    { /// <summary>Enable the Wasm GC proposal.</summary>
      EnableGc: bool
      /// <summary>Enable typed function references.</summary>
      EnableTypedFunctionRefs: bool
      /// <summary>Enable tail call optimization.</summary>
      EnableTailCall: bool
      /// <summary>Enable bulk memory operations.</summary>
      EnableBulkMemory: bool
      /// <summary>Enable reference types.</summary>
      EnableReferenceTypes: bool }

/// <summary>Project-level configuration for a Wasm build.</summary>
type ProjectConfig =
    { /// <summary>Deployment target.</summary>
      Target: WasmTarget
      /// <summary>Target Wasm platform.</summary>
      Platform: WasmPlatform
      /// <summary>Whether ghost sync is enabled for client/edge communication.</summary>
      GhostSyncEnabled: bool
      /// <summary>Wasm GC feature flags.</summary>
      GcFlags: WasmGcFlags
      /// <summary>Output directory for build artifacts.</summary>
      OutputDir: string
      /// <summary>Source directory containing F# files.</summary>
      SourceDir: string
      /// <summary>Name of the project.</summary>
      ProjectName: string }

/// <summary>SDK-level configuration wrapping project config with tooling options.</summary>
type SdkConfig =
    { /// <summary>Project configuration.</summary>
      Config: ProjectConfig
      /// <summary>Whether to generate a split analysis report.</summary>
      SplitReport: bool
      /// <summary>Enable verbose logging output.</summary>
      Verbose: bool }

/// <summary>Helpers for constructing and parsing project configuration.</summary>
module ProjectConfig =

    /// <summary>Default Wasm GC flags with all features enabled.</summary>
    let defaultGcFlags: WasmGcFlags =
        { EnableGc = true
          EnableTypedFunctionRefs = true
          EnableTailCall = true
          EnableBulkMemory = true
          EnableReferenceTypes = true }

    /// <summary>Default project configuration targeting both client and edge on a generic platform.</summary>
    let defaultConfig: ProjectConfig =
        { Target = Both
          Platform = Generic
          GhostSyncEnabled = true
          GcFlags = defaultGcFlags
          OutputDir = "bin/wasm"
          SourceDir = "src"
          ProjectName = "App" }

    /// <summary>Parse a WasmTarget from a JSON-style string value.</summary>
    let private parseTarget (value: string) : WasmTarget =
        match value.Trim().ToLowerInvariant() with
        | "client" -> Client
        | "edge" -> Edge
        | "both" -> Both
        | _ -> Both

    /// <summary>Parse a WasmPlatform from a JSON-style string value.</summary>
    let private parsePlatform (value: string) : WasmPlatform =
        match value.Trim().ToLowerInvariant() with
        | "cloudflare" -> Cloudflare
        | "fastly" -> Fastly
        | "fermyon" -> Fermyon
        | "wasmedge" -> WasmEdge
        | _ -> Generic

    /// <summary>Extract a string value from a simple JSON key-value pair.</summary>
    let private extractJsonString (json: string) (key: string) : string option =
        let pattern = sprintf "\"%s\"" key
        let idx = json.IndexOf(pattern, StringComparison.OrdinalIgnoreCase)
        if idx < 0 then
            None
        else
            let afterKey = json.Substring(idx + pattern.Length)
            let colonIdx = afterKey.IndexOf(':')
            if colonIdx < 0 then
                None
            else
                let afterColon = afterKey.Substring(colonIdx + 1).TrimStart()
                if afterColon.Length > 0 && afterColon.[0] = '"' then
                    let endQuote = afterColon.IndexOf('"', 1)
                    if endQuote > 0 then
                        Some(afterColon.Substring(1, endQuote - 1))
                    else
                        None
                else
                    // Unquoted value (bool or number) up to comma, brace, or end
                    let mutable endIdx = 0
                    let trimmed = afterColon.TrimStart()
                    while endIdx < trimmed.Length
                          && trimmed.[endIdx] <> ','
                          && trimmed.[endIdx] <> '}'
                          && trimmed.[endIdx] <> ']' do
                        endIdx <- endIdx + 1
                    if endIdx > 0 then
                        Some(trimmed.Substring(0, endIdx).Trim())
                    else
                        None

    /// <summary>Extract a boolean value from a simple JSON key-value pair.</summary>
    let private extractJsonBool (json: string) (key: string) (defaultValue: bool) : bool =
        match extractJsonString json key with
        | Some "true" -> true
        | Some "false" -> false
        | _ -> defaultValue

    /// <summary>Parse a ProjectConfig from a JSON string with basic key-value extraction.</summary>
    let fromJson (json: string) : ProjectConfig =
        let target =
            match extractJsonString json "target" with
            | Some v -> parseTarget v
            | None -> defaultConfig.Target

        let platform =
            match extractJsonString json "platform" with
            | Some v -> parsePlatform v
            | None -> defaultConfig.Platform

        let ghostSync = extractJsonBool json "ghostSyncEnabled" defaultConfig.GhostSyncEnabled
        let outputDir =
            match extractJsonString json "outputDir" with
            | Some v -> v
            | None -> defaultConfig.OutputDir

        let sourceDir =
            match extractJsonString json "sourceDir" with
            | Some v -> v
            | None -> defaultConfig.SourceDir

        let projectName =
            match extractJsonString json "projectName" with
            | Some v -> v
            | None -> defaultConfig.ProjectName

        let gcFlags =
            { EnableGc = extractJsonBool json "enableGc" defaultGcFlags.EnableGc
              EnableTypedFunctionRefs = extractJsonBool json "enableTypedFunctionRefs" defaultGcFlags.EnableTypedFunctionRefs
              EnableTailCall = extractJsonBool json "enableTailCall" defaultGcFlags.EnableTailCall
              EnableBulkMemory = extractJsonBool json "enableBulkMemory" defaultGcFlags.EnableBulkMemory
              EnableReferenceTypes = extractJsonBool json "enableReferenceTypes" defaultGcFlags.EnableReferenceTypes }

        { Target = target
          Platform = platform
          GhostSyncEnabled = ghostSync
          GcFlags = gcFlags
          OutputDir = outputDir
          SourceDir = sourceDir
          ProjectName = projectName }
