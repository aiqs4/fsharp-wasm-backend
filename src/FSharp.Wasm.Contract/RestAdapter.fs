namespace FSharp.Wasm.Contract

open System
open FSharp.Wasm.Host

/// <summary>
/// Adapts ContractSchema endpoints to FSharp.Wasm.Host.Route definitions,
/// bridging the contract layer with the Host router.
/// </summary>
module RestAdapter =

    /// <summary>Converts a contract EndpointMethod to a Host HttpMethod.</summary>
    let private toHttpMethod (m: EndpointMethod) : HttpMethod =
        match m with
        | EndpointMethod.Get -> HttpMethod.GET
        | EndpointMethod.Post -> HttpMethod.POST
        | EndpointMethod.Put -> HttpMethod.PUT
        | EndpointMethod.Delete -> HttpMethod.DELETE
        | EndpointMethod.Patch -> HttpMethod.PATCH

    /// <summary>
    /// Converts an OpenAPI-style path (e.g., "/api/users/{id}") to a
    /// Host-style pattern (e.g., "/api/users/:id").
    /// </summary>
    let private toRoutePattern (path: string) : string =
        System.Text.RegularExpressions.Regex.Replace(path, @"\{(\w+)\}", ":$1")

    /// <summary>
    /// Generates a placeholder handler that returns a JSON response stub.
    /// Real handlers should be wired up by the application.
    /// </summary>
    let private placeholderHandler (endpointName: string) : (HttpRequest -> Env -> Async<HttpResponse>) =
        fun (_req: HttpRequest) (_env: Env) ->
            async {
                return
                    HttpResponse.json (
                        sprintf """{"endpoint":"%s","status":"not_implemented"}"""
                            (endpointName.Replace("\"", "\\\""))
                    )
            }

    /// <summary>
    /// Generates Host Route definitions from a ContractSchema.
    /// Each contract endpoint maps to a Route with method, pattern, and a
    /// placeholder handler. Applications should replace the handlers with
    /// real implementations.
    /// </summary>
    /// <param name="schema">The contract schema to generate routes from.</param>
    /// <returns>A list of Host Route definitions.</returns>
    let generateRoutes (schema: ContractSchema) : Route list =
        schema.Endpoints
        |> List.map (fun endpoint ->
            { Method = toHttpMethod endpoint.Method
              Pattern = toRoutePattern endpoint.Path
              Handler = placeholderHandler endpoint.Name })

    /// <summary>Escapes a string for safe inclusion in F# source code.</summary>
    let private escapeFSharpString (s: string) =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")

    /// <summary>Converts an EndpointMethod to its F# HttpMethod DU case string.</summary>
    let private methodToFSharpCase (m: EndpointMethod) =
        match m with
        | EndpointMethod.Get -> "HttpMethod.GET"
        | EndpointMethod.Post -> "HttpMethod.POST"
        | EndpointMethod.Put -> "HttpMethod.PUT"
        | EndpointMethod.Delete -> "HttpMethod.DELETE"
        | EndpointMethod.Patch -> "HttpMethod.PATCH"

    /// <summary>
    /// Generates an F# source file defining API routes for the given ContractSchema.
    /// The generated file can be included in an F# project to register routes
    /// with a Host Router instance.
    /// </summary>
    /// <param name="schema">The contract schema to generate routes from.</param>
    /// <returns>An F# source code string containing route definitions.</returns>
    let generateRoutesFile (schema: ContractSchema) : string =
        let header =
            "// Auto-generated API routes for " + schema.Name + " v" + schema.Version + "\n"
            + "// Do not edit manually.\n"
            + "module " + schema.Name.Replace(" ", "") + ".ApiRoutes\n"
            + "\n"
            + "open FSharp.Wasm.Host\n"

        let routeDefs =
            schema.Endpoints
            |> List.map (fun endpoint ->
                let docComment =
                    match endpoint.Description with
                    | Some desc ->
                        "/// <summary>" + escapeFSharpString desc + "</summary>\n"
                    | None -> ""

                let deprecatedAttr =
                    if endpoint.IsDeprecated then
                        "[<System.Obsolete>]\n"
                    else
                        ""

                let jsonBody =
                    sprintf "{\\\"endpoint\\\":\\\"%s\\\",\\\"status\\\":\\\"not_implemented\\\"}"
                        (escapeFSharpString endpoint.Name)

                docComment + deprecatedAttr
                + "let " + endpoint.Name + "Handler (req: HttpRequest) (env: Env) : Async<HttpResponse> =\n"
                + "    async {\n"
                + "        // TODO: Implement " + endpoint.Name + " handler\n"
                + "        return HttpResponse.json \"" + jsonBody + "\"\n"
                + "    }\n")
            |> String.concat "\n"

        let registerFn =
            if schema.Endpoints.IsEmpty then
                "/// <summary>Register all API routes with the given router.</summary>\n"
                + "let registerRoutes (router: Router) =\n"
                + "    ()\n"
            else
                let registrations =
                    schema.Endpoints
                    |> List.map (fun endpoint ->
                        let methodCall =
                            match endpoint.Method with
                            | EndpointMethod.Get -> "get"
                            | EndpointMethod.Post -> "post"
                            | EndpointMethod.Put -> "put"
                            | EndpointMethod.Delete -> "delete"
                            | EndpointMethod.Patch -> "post"

                        let pattern = toRoutePattern endpoint.Path

                        "    router." + methodCall + " \"" + escapeFSharpString pattern + "\" " + endpoint.Name + "Handler")
                    |> String.concat "\n"

                "/// <summary>Register all API routes with the given router.</summary>\n"
                + "let registerRoutes (router: Router) =\n"
                + registrations + "\n"

        header + "\n" + routeDefs + registerFn
