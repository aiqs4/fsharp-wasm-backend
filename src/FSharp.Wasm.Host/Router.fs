namespace FSharp.Wasm.Host

open System

/// <summary>A registered route mapping an HTTP method and URL pattern to a handler.</summary>
type Route =
    { /// <summary>HTTP method this route matches.</summary>
      Method: HttpMethod
      /// <summary>
      /// URL pattern with optional <c>:param</c> segments
      /// (e.g. <c>/api/users/:id</c>).
      /// </summary>
      Pattern: string
      /// <summary>Async handler invoked when the route matches.</summary>
      Handler: HttpRequest -> Env -> Async<HttpResponse> }

/// <summary>Result of matching a request against the route table.</summary>
type RouteMatch =
    { /// <summary>The matched route.</summary>
      Route: Route
      /// <summary>Path parameters extracted from the URL.</summary>
      PathParams: Map<string, string> }

/// <summary>
/// Simple path-based HTTP router supporting <c>:param</c>-style path parameters.
/// Routes are evaluated in registration order; the first match wins.
/// </summary>
type Router() =

    let mutable registeredRoutes: Route list = []

    // ── Pattern matching engine ──────────────────────────────────────

    /// Split a path into non-empty segments.
    let splitPath (path: string) =
        path.Split('/', StringSplitOptions.RemoveEmptyEntries)

    /// Try to match a request path against a route pattern,
    /// extracting any <c>:param</c> captures.
    let tryMatchPattern (pattern: string) (path: string) : Map<string, string> option =
        let patternParts = splitPath pattern
        let pathParts = splitPath path

        if patternParts.Length <> pathParts.Length then
            None
        else
            let mutable matches = true
            let mutable paramMap = Map.empty<string, string>

            for i in 0 .. patternParts.Length - 1 do
                let pp = patternParts.[i]
                let rp = pathParts.[i]

                if pp.StartsWith(":") then
                    let paramName = pp.Substring(1)
                    paramMap <- paramMap |> Map.add paramName rp
                elif not (String.Equals(pp, rp, StringComparison.OrdinalIgnoreCase)) then
                    matches <- false

            if matches then Some paramMap else None

    /// Extract the path component from a URL (strip query string).
    let extractPath (url: string) =
        match url.IndexOf('?') with
        | -1 -> url
        | idx -> url.Substring(0, idx)

    // ── Route registration ───────────────────────────────────────────

    let addRoute (method': HttpMethod) (pattern: string) (handler: HttpRequest -> Env -> Async<HttpResponse>) =
        let route =
            { Method = method'
              Pattern = pattern
              Handler = handler }

        registeredRoutes <- registeredRoutes @ [ route ]

    /// <summary>Register a GET route.</summary>
    member _.get (pattern: string) (handler: HttpRequest -> Env -> Async<HttpResponse>) = addRoute HttpMethod.GET pattern handler

    /// <summary>Register a POST route.</summary>
    member _.post (pattern: string) (handler: HttpRequest -> Env -> Async<HttpResponse>) = addRoute HttpMethod.POST pattern handler

    /// <summary>Register a PUT route.</summary>
    member _.put (pattern: string) (handler: HttpRequest -> Env -> Async<HttpResponse>) = addRoute HttpMethod.PUT pattern handler

    /// <summary>Register a DELETE route.</summary>
    member _.delete (pattern: string) (handler: HttpRequest -> Env -> Async<HttpResponse>) = addRoute HttpMethod.DELETE pattern handler

    /// <summary>Return all registered routes.</summary>
    member _.routes() : Route list = registeredRoutes

    /// <summary>
    /// Dispatch an incoming request to the first matching route.
    /// Extracted path parameters are merged into the request's
    /// <see cref="HttpRequest.PathParams"/> before the handler is invoked.
    /// Returns a 404 response when no route matches.
    /// </summary>
    member _.dispatch (request: HttpRequest) (env: Env) : Async<HttpResponse> =
        let path = extractPath request.Url

        let matched =
            registeredRoutes
            |> List.tryPick (fun route ->
                if route.Method = request.Method then
                    tryMatchPattern route.Pattern path
                    |> Option.map (fun pathParams -> { Route = route; PathParams = pathParams })
                else
                    None)

        match matched with
        | Some m ->
            let mergedParams =
                Map.fold (fun acc k v -> Map.add k v acc) request.PathParams m.PathParams

            let enrichedRequest =
                { request with
                    PathParams = mergedParams }

            m.Route.Handler enrichedRequest env
        | None -> async { return HttpResponse.notFound }
