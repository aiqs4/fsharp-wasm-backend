namespace FSharp.Wasm.Auth

/// <summary>Describes a named capability that can be granted to an identity.</summary>
type Capability =
    { /// <summary>The unique capability name (e.g. "admin", "read:reports").</summary>
      Name: string
      /// <summary>An optional human-readable description of the capability.</summary>
      Description: string option }

/// <summary>
/// A composable authorization check that can be evaluated against an Identity.
/// Supports role checks, claim checks, custom predicates, and boolean combinators.
/// </summary>
type CapabilityCheck =
    /// <summary>The identity must have the specified role.</summary>
    | HasRole of string
    /// <summary>The identity must have a claim with the given type and value.</summary>
    | HasClaim of claimType: string * claimValue: string
    /// <summary>A custom predicate evaluated against the identity.</summary>
    | Custom of (Identity -> bool)
    /// <summary>All nested checks must pass (logical AND).</summary>
    | All of CapabilityCheck list
    /// <summary>At least one nested check must pass (logical OR).</summary>
    | Any of CapabilityCheck list

/// <summary>The result of an authorization check.</summary>
type AuthorizationResult =
    /// <summary>The identity is authorized.</summary>
    | Authorized
    /// <summary>The identity is not authorized; includes the reason.</summary>
    | Denied of reason: string
    /// <summary>The identity is not authenticated and must authenticate first.</summary>
    | ChallengeRequired

/// <summary>Functions for evaluating capability-based authorization checks.</summary>
[<RequireQualifiedAccess>]
module CapabilityGuard =

    /// <summary>
    /// Evaluates a capability check against an identity.
    /// Returns ChallengeRequired for unauthenticated identities,
    /// Authorized if the check passes, or Denied with a reason.
    /// </summary>
    /// <param name="capability">The capability check to evaluate.</param>
    /// <param name="identity">The identity to evaluate against.</param>
    /// <returns>The authorization result.</returns>
    let rec check (capability: CapabilityCheck) (identity: Identity) : AuthorizationResult =
        if not identity.IsAuthenticated then
            ChallengeRequired
        else
            match capability with
            | HasRole role ->
                if Identity.hasRole role identity then
                    Authorized
                else
                    Denied(sprintf "Missing required role '%s'" role)

            | HasClaim(claimType, claimValue) ->
                if Identity.hasClaim claimType claimValue identity then
                    Authorized
                else
                    Denied(sprintf "Missing required claim '%s' = '%s'" claimType claimValue)

            | Custom predicate ->
                if predicate identity then
                    Authorized
                else
                    Denied "Custom authorization check failed"

            | All checks ->
                let firstFailure =
                    checks
                    |> List.tryPick (fun c ->
                        match check c identity with
                        | Authorized -> None
                        | result -> Some result)

                match firstFailure with
                | Some failure -> failure
                | None -> Authorized

            | Any checks ->
                let anyPassed =
                    checks
                    |> List.exists (fun c ->
                        match check c identity with
                        | Authorized -> true
                        | _ -> false)

                if anyPassed then
                    Authorized
                else
                    Denied "None of the required capabilities were satisfied"

    /// <summary>
    /// Shorthand for checking whether the identity has a specific role.
    /// </summary>
    /// <param name="capability">The role name to require.</param>
    /// <param name="identity">The identity to evaluate.</param>
    /// <returns>The authorization result.</returns>
    let require (capability: string) (identity: Identity) : AuthorizationResult =
        check (HasRole capability) identity

    /// <summary>
    /// Checks whether the identity has any one of the specified roles.
    /// </summary>
    /// <param name="capabilities">The role names, any one of which is sufficient.</param>
    /// <param name="identity">The identity to evaluate.</param>
    /// <returns>The authorization result.</returns>
    let requireAny (capabilities: string list) (identity: Identity) : AuthorizationResult =
        let checks = capabilities |> List.map HasRole
        check (Any checks) identity

    /// <summary>
    /// Checks whether the identity has all of the specified roles.
    /// </summary>
    /// <param name="capabilities">The role names, all of which are required.</param>
    /// <param name="identity">The identity to evaluate.</param>
    /// <returns>The authorization result.</returns>
    let requireAll (capabilities: string list) (identity: Identity) : AuthorizationResult =
        let checks = capabilities |> List.map HasRole
        check (All checks) identity
