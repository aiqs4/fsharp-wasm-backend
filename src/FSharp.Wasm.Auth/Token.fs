namespace FSharp.Wasm.Auth

open System
open System.Collections.Concurrent
open System.Text

/// <summary>Supported token strategies.</summary>
type TokenStrategy =
    /// <summary>JSON Web Token (RFC 7519).</summary>
    | Jwt
    /// <summary>Platform-Agnostic Security Tokens.</summary>
    | Paseto

/// <summary>Configuration for token validation and generation.</summary>
type TokenConfig =
    { /// <summary>The token strategy to use.</summary>
      Strategy: TokenStrategy
      /// <summary>Optional public key for asymmetric token verification.</summary>
      PublicKey: string option
      /// <summary>Optional secret key for symmetric token signing.</summary>
      SecretKey: string option
      /// <summary>Expected issuer, validated during token verification.</summary>
      Issuer: string option
      /// <summary>Expected audience, validated during token verification.</summary>
      Audience: string option
      /// <summary>Token expiration time in minutes.</summary>
      ExpirationMinutes: int
      /// <summary>Whether to cache validated tokens for performance.</summary>
      CacheTokens: bool }

/// <summary>Decoded token payload with standard claims.</summary>
type TokenPayload =
    { /// <summary>The subject identifier.</summary>
      Subject: string
      /// <summary>Custom claims as key-value pairs.</summary>
      Claims: Map<string, string>
      /// <summary>When the token was issued.</summary>
      IssuedAt: DateTimeOffset
      /// <summary>When the token expires.</summary>
      ExpiresAt: DateTimeOffset
      /// <summary>The token issuer, if specified.</summary>
      Issuer: string option
      /// <summary>Unique token identifier for revocation tracking.</summary>
      TokenId: string }

/// <summary>Interface for token validation and generation.</summary>
type ITokenProvider =
    /// <summary>Validates a token string and returns the decoded payload, or an error message.</summary>
    /// <param name="token">The raw token string.</param>
    /// <returns>Ok with the decoded payload, or Error with a reason.</returns>
    abstract Validate: token: string -> Result<TokenPayload, string>

    /// <summary>Generates a token string from the given payload.</summary>
    /// <param name="payload">The token payload to encode.</param>
    /// <returns>An encoded token string.</returns>
    abstract Generate: payload: TokenPayload -> string

/// <summary>
/// A simple Base64-encoded token provider for testing and development.
/// WARNING: This is NOT cryptographically secure. Do not use in production.
/// Tokens are plain-text Base64 with no signature or encryption.
/// </summary>
type SimpleTokenProvider() =

    let separator = "|"

    let formatClaims (claims: Map<string, string>) =
        claims
        |> Map.toList
        |> List.map (fun (k, v) -> sprintf "%s=%s" k v)
        |> String.concat ","

    let parseClaims (s: string) =
        if String.IsNullOrWhiteSpace(s) then
            Map.empty
        else
            s.Split(',')
            |> Array.choose (fun pair ->
                match pair.Split('=', 2) with
                | [| k; v |] -> Some(k, v)
                | _ -> None)
            |> Map.ofArray

    interface ITokenProvider with

        /// <summary>
        /// Validates a Base64-encoded token. NOT cryptographically secure.
        /// </summary>
        member _.Validate(token: string) =
            try
                let decoded =
                    token
                    |> Convert.FromBase64String
                    |> Encoding.UTF8.GetString

                let parts = decoded.Split(separator, StringSplitOptions.None)

                if parts.Length < 5 then
                    Error "Invalid token format: insufficient segments"
                else
                    let subject = parts.[0]
                    let claims = parseClaims parts.[1]

                    match Int64.TryParse(parts.[2]), Int64.TryParse(parts.[3]) with
                    | (true, issuedAt), (true, expiresAt) ->
                        let expiresAtOffset = DateTimeOffset.FromUnixTimeSeconds(expiresAt)

                        if expiresAtOffset < DateTimeOffset.UtcNow then
                            Error "Token has expired"
                        else
                            let issuer =
                                if String.IsNullOrEmpty(parts.[4]) then None
                                else Some parts.[4]

                            let tokenId =
                                if parts.Length > 5 then parts.[5]
                                else ""

                            Ok
                                { Subject = subject
                                  Claims = claims
                                  IssuedAt = DateTimeOffset.FromUnixTimeSeconds(issuedAt)
                                  ExpiresAt = expiresAtOffset
                                  Issuer = issuer
                                  TokenId = tokenId }
                    | _ -> Error "Invalid token format: cannot parse timestamps"
            with ex ->
                Error(sprintf "Token validation failed: %s" ex.Message)

        /// <summary>
        /// Generates a Base64-encoded token. NOT cryptographically secure.
        /// </summary>
        member _.Generate(payload: TokenPayload) =
            let issuerStr = payload.Issuer |> Option.defaultValue ""

            let raw =
                String.concat
                    separator
                    [ payload.Subject
                      formatClaims payload.Claims
                      string (payload.IssuedAt.ToUnixTimeSeconds())
                      string (payload.ExpiresAt.ToUnixTimeSeconds())
                      issuerStr
                      payload.TokenId ]

            raw |> Encoding.UTF8.GetBytes |> Convert.ToBase64String

/// <summary>
/// Caches validated token payloads to avoid repeated validation of the same token.
/// Expired entries are evicted on access.
/// </summary>
type TokenCache() =

    let cache = ConcurrentDictionary<string, TokenPayload>()

    /// <summary>
    /// Returns a cached payload for the token if available, otherwise validates
    /// and caches the result.
    /// </summary>
    /// <param name="token">The raw token string.</param>
    /// <param name="provider">The token provider to use for validation.</param>
    /// <returns>Ok with the payload, or Error with a reason.</returns>
    member _.GetOrValidate(token: string, provider: ITokenProvider) : Result<TokenPayload, string> =
        match cache.TryGetValue(token) with
        | true, payload when payload.ExpiresAt > DateTimeOffset.UtcNow -> Ok payload
        | true, _ ->
            cache.TryRemove(token) |> ignore
            match provider.Validate(token) with
            | Ok payload ->
                cache.TryAdd(token, payload) |> ignore
                Ok payload
            | Error _ as err -> err
        | false, _ ->
            match provider.Validate(token) with
            | Ok payload ->
                cache.TryAdd(token, payload) |> ignore
                Ok payload
            | Error _ as err -> err

    /// <summary>Removes a specific token from the cache.</summary>
    /// <param name="token">The token string to invalidate.</param>
    member _.Invalidate(token: string) : unit =
        cache.TryRemove(token) |> ignore

    /// <summary>Removes all cached tokens.</summary>
    member _.Clear() : unit = cache.Clear()

    /// <summary>Gets the number of tokens currently in the cache.</summary>
    member _.CacheSize: int = cache.Count
