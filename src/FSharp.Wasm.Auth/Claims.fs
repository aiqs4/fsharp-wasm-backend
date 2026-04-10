namespace FSharp.Wasm.Auth

open System

/// <summary>Represents a single identity claim as a type-value pair.</summary>
type Claim =
    { /// <summary>The claim type URI or short name (e.g. "role", "email").</summary>
      Type: string
      /// <summary>The claim value.</summary>
      Value: string }

/// <summary>Well-known claim type constants following standard JWT / OpenID Connect conventions.</summary>
[<RequireQualifiedAccess>]
module ClaimTypes =

    /// <summary>Role claim type.</summary>
    [<Literal>]
    let Role = "role"

    /// <summary>Display name claim type.</summary>
    [<Literal>]
    let Name = "name"

    /// <summary>Email address claim type.</summary>
    [<Literal>]
    let Email = "email"

    /// <summary>Subject (unique user identifier) claim type.</summary>
    [<Literal>]
    let Sub = "sub"

    /// <summary>Issuer claim type.</summary>
    [<Literal>]
    let Iss = "iss"

    /// <summary>Audience claim type.</summary>
    [<Literal>]
    let Aud = "aud"

    /// <summary>Expiration time claim type (Unix timestamp).</summary>
    [<Literal>]
    let Exp = "exp"

    /// <summary>Issued-at time claim type (Unix timestamp).</summary>
    [<Literal>]
    let Iat = "iat"

    /// <summary>JWT ID (unique token identifier) claim type.</summary>
    [<Literal>]
    let Jti = "jti"

/// <summary>Represents an authenticated (or anonymous) identity with associated claims.</summary>
type Identity =
    { /// <summary>The subject identifier for the identity.</summary>
      Subject: string
      /// <summary>The list of claims associated with this identity.</summary>
      Claims: Claim list
      /// <summary>Whether this identity has been authenticated.</summary>
      IsAuthenticated: bool
      /// <summary>The authentication scheme used (e.g. "Bearer", "Cookie").</summary>
      AuthScheme: string }

/// <summary>Functions for creating and querying Identity values.</summary>
[<RequireQualifiedAccess>]
module Identity =

    /// <summary>An unauthenticated, anonymous identity with no claims.</summary>
    let anonymous: Identity =
        { Subject = ""
          Claims = []
          IsAuthenticated = false
          AuthScheme = "" }

    /// <summary>Creates an authenticated identity with the given subject, claims, and scheme.</summary>
    /// <param name="subject">The unique subject identifier.</param>
    /// <param name="claims">The claims to associate with the identity.</param>
    /// <param name="scheme">The authentication scheme name.</param>
    /// <returns>An authenticated Identity.</returns>
    let create (subject: string) (claims: Claim list) (scheme: string) : Identity =
        { Subject = subject
          Claims = claims
          IsAuthenticated = true
          AuthScheme = scheme }

    /// <summary>Checks whether the identity possesses the specified role.</summary>
    /// <param name="role">The role name to check.</param>
    /// <param name="identity">The identity to inspect.</param>
    /// <returns>True if the identity has a role claim with the given value.</returns>
    let hasRole (role: string) (identity: Identity) : bool =
        identity.Claims
        |> List.exists (fun c -> c.Type = ClaimTypes.Role && c.Value = role)

    /// <summary>Checks whether the identity has a claim matching the given type and value.</summary>
    /// <param name="claimType">The claim type to match.</param>
    /// <param name="claimValue">The claim value to match.</param>
    /// <param name="identity">The identity to inspect.</param>
    /// <returns>True if a matching claim exists.</returns>
    let hasClaim (claimType: string) (claimValue: string) (identity: Identity) : bool =
        identity.Claims
        |> List.exists (fun c -> c.Type = claimType && c.Value = claimValue)

    /// <summary>Gets the first claim value of the given type, if present.</summary>
    /// <param name="claimType">The claim type to search for.</param>
    /// <param name="identity">The identity to inspect.</param>
    /// <returns>Some value if the claim exists, None otherwise.</returns>
    let getClaim (claimType: string) (identity: Identity) : string option =
        identity.Claims
        |> List.tryFind (fun c -> c.Type = claimType)
        |> Option.map (fun c -> c.Value)

    /// <summary>Gets all role claim values from the identity.</summary>
    /// <param name="identity">The identity to inspect.</param>
    /// <returns>A list of role names.</returns>
    let getRoles (identity: Identity) : string list =
        identity.Claims
        |> List.choose (fun c ->
            if c.Type = ClaimTypes.Role then Some c.Value
            else None)

    /// <summary>
    /// Checks whether the identity's expiration claim indicates the token has expired.
    /// Returns true if an "exp" claim exists and its Unix timestamp is in the past.
    /// Returns false if no "exp" claim is present.
    /// </summary>
    /// <param name="identity">The identity to inspect.</param>
    /// <returns>True if the identity is expired.</returns>
    let isExpired (identity: Identity) : bool =
        match getClaim ClaimTypes.Exp identity with
        | None -> false
        | Some expStr ->
            match Int64.TryParse(expStr) with
            | true, expUnix ->
                let expTime = DateTimeOffset.FromUnixTimeSeconds(expUnix)
                expTime < DateTimeOffset.UtcNow
            | false, _ -> false
