namespace FSharp.Wasm.Auth

open System
open FSharp.Wasm.Host

/// <summary>Configuration for the authentication middleware.</summary>
type AuthConfig =
    { /// <summary>Token validation and generation settings.</summary>
      TokenConfig: TokenConfig
      /// <summary>Optional default capability check applied to all requests.</summary>
      DefaultPolicy: CapabilityCheck option
      /// <summary>Function to extract a token string from an HTTP request.</summary>
      TokenExtractor: HttpRequest -> string option }

/// <summary>The authentication context built from a validated request.</summary>
type AuthContext =
    { /// <summary>The resolved identity (authenticated or anonymous).</summary>
      Identity: Identity
      /// <summary>The decoded token payload, if a token was present and valid.</summary>
      Token: TokenPayload option
      /// <summary>The original HTTP request.</summary>
      Request: HttpRequest }

/// <summary>Authentication and authorization middleware for Host integration.</summary>
[<RequireQualifiedAccess>]
module Auth =

    let mutable private currentConfig: AuthConfig option = None
    let private tokenCache = TokenCache()
    let private tokenProvider = SimpleTokenProvider() :> ITokenProvider

    /// <summary>Sets the global authentication configuration.</summary>
    /// <param name="config">The auth configuration to use.</param>
    let configure (config: AuthConfig) : unit = currentConfig <- Some config

    /// <summary>
    /// Extracts a Bearer token from the Authorization header of an HTTP request.
    /// Expects the format "Bearer &lt;token&gt;".
    /// </summary>
    /// <param name="request">The HTTP request to inspect.</param>
    /// <returns>Some token string if found, None otherwise.</returns>
    let extractBearerToken (request: HttpRequest) : string option =
        request.Headers
        |> List.tryFind (fun h ->
            String.Equals(h.Name, "Authorization", StringComparison.OrdinalIgnoreCase))
        |> Option.bind (fun h ->
            let value = h.Value.Trim()

            if value.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase) then
                Some(value.Substring(7).Trim())
            else
                None)

    /// <summary>
    /// Extracts a token from a cookie with the specified name.
    /// </summary>
    /// <param name="cookieName">The cookie name to look for.</param>
    /// <param name="request">The HTTP request to inspect.</param>
    /// <returns>Some token string if the cookie is found, None otherwise.</returns>
    let extractCookieToken (cookieName: string) (request: HttpRequest) : string option =
        request.Headers
        |> List.tryFind (fun h ->
            String.Equals(h.Name, "Cookie", StringComparison.OrdinalIgnoreCase))
        |> Option.bind (fun h ->
            h.Value.Split(';')
            |> Array.tryPick (fun segment ->
                let trimmed = segment.Trim()

                match trimmed.Split('=', 2) with
                | [| name; value |] when name.Trim() = cookieName -> Some(value.Trim())
                | _ -> None))

    /// <summary>
    /// Extracts a token from the request, validates it, and builds an AuthContext.
    /// Returns an anonymous context if no token is present.
    /// Returns Error if a token is present but invalid.
    /// </summary>
    /// <param name="request">The HTTP request to verify.</param>
    /// <returns>Ok with the auth context, or Error with a reason.</returns>
    let verify (request: HttpRequest) : Async<Result<AuthContext, string>> =
        async {
            match currentConfig with
            | None ->
                return
                    Ok
                        { Identity = Identity.anonymous
                          Token = None
                          Request = request }
            | Some config ->
                match config.TokenExtractor request with
                | None ->
                    return
                        Ok
                            { Identity = Identity.anonymous
                              Token = None
                              Request = request }
                | Some token ->
                    let validationResult =
                        if config.TokenConfig.CacheTokens then
                            tokenCache.GetOrValidate(token, tokenProvider)
                        else
                            tokenProvider.Validate(token)

                    match validationResult with
                    | Error reason -> return Error reason
                    | Ok payload ->
                        let claims =
                            payload.Claims
                            |> Map.toList
                            |> List.map (fun (k, v) -> { Type = k; Value = v })

                        let identity = Identity.create payload.Subject claims "Bearer"

                        return
                            Ok
                                { Identity = identity
                                  Token = Some payload
                                  Request = request }
        }

    /// <summary>
    /// Middleware-style wrapper that extracts and validates auth before
    /// invoking the handler. Returns 401 if the token is invalid.
    /// </summary>
    /// <param name="handler">The handler to invoke with the auth context.</param>
    /// <param name="request">The incoming HTTP request.</param>
    /// <returns>The HTTP response from the handler, or a 401 response on auth failure.</returns>
    let withAuth
        (handler: AuthContext -> Async<HttpResponse>)
        (request: HttpRequest)
        : Async<HttpResponse> =
        async {
            let! result = verify request

            match result with
            | Ok ctx -> return! handler ctx
            | Error reason ->
                let body =
                    reason
                    |> System.Text.Encoding.UTF8.GetBytes

                return
                    { StatusCode = 401
                      Headers =
                        [ { Name = "WWW-Authenticate"
                            Value = "Bearer" } ]
                      Body = Some body
                      ContentType = Some "text/plain" }
        }
