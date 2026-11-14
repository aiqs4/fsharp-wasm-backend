namespace FSharp.Wasm.Protocol

/// <summary>
/// Manages monotonic model versioning and patch validation
/// for optimistic concurrency control.
/// </summary>
[<RequireQualifiedAccess>]
module VersionManager =

    /// <summary>Tracks the current and last-acknowledged model versions.</summary>
    type VersionState =
        { /// <summary>Current local model version.</summary>
          CurrentVersion: uint32
          /// <summary>Last version acknowledged by the remote peer.</summary>
          LastAcknowledged: uint32 }

    /// <summary>Create an initial version state starting at version 0.</summary>
    let create () : VersionState =
        { CurrentVersion = 0u
          LastAcknowledged = 0u }

    /// <summary>Increment the current version by one.</summary>
    let increment (state: VersionState) : VersionState =
        { state with
            CurrentVersion = state.CurrentVersion + 1u }

    /// <summary>
    /// Determine whether a full resync is needed.
    /// Returns true when the incoming patch version is more than one
    /// ahead of the local version, indicating missed patches.
    /// </summary>
    let shouldResync (localVersion: uint32) (patchVersion: uint32) : bool =
        patchVersion > localVersion + 1u

    /// <summary>
    /// Validate an incoming patch against the current version state.
    /// Accepts the patch when its version equals CurrentVersion + 1,
    /// advancing the state. Returns an error otherwise.
    /// </summary>
    let validatePatch (state: VersionState) (patch: Patch) : Result<VersionState, string> =
        let (ModelVersion pv) = patch.Version

        if pv = state.CurrentVersion + 1u then
            Ok
                { CurrentVersion = pv
                  LastAcknowledged = state.LastAcknowledged }
        elif pv <= state.CurrentVersion then
            Error(sprintf "Stale patch version %d; current is %d" pv state.CurrentVersion)
        else
            Error(
                sprintf
                    "Patch version %d is too far ahead of current %d; resync required"
                    pv
                    state.CurrentVersion
            )
