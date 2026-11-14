namespace FSharp.Wasm.Contract

open System

/// <summary>
/// Generates TypeScript type declarations and fetch-based runtime clients
/// from a ContractSchema. Output has no external dependencies.
/// </summary>
module TypeScriptGenerator =

    /// <summary>Converts a name to PascalCase for TypeScript interface names.</summary>
    let private toPascalCase (s: string) =
        if String.IsNullOrEmpty s then s
        else String.Concat(Char.ToUpperInvariant s.[0], s.Substring 1)

    /// <summary>Converts a name to camelCase for TypeScript property/method names.</summary>
    let private toCamelCase (s: string) =
        if String.IsNullOrEmpty s then s
        else String.Concat(Char.ToLowerInvariant s.[0], s.Substring 1)

    /// <summary>Converts an EndpointMethod to its uppercase HTTP verb string.</summary>
    let private methodToString (m: EndpointMethod) =
        match m with
        | EndpointMethod.Get -> "GET"
        | EndpointMethod.Post -> "POST"
        | EndpointMethod.Put -> "PUT"
        | EndpointMethod.Delete -> "DELETE"
        | EndpointMethod.Patch -> "PATCH"

    /// <summary>
    /// Converts a SchemaType to its TypeScript type representation.
    /// </summary>
    /// <param name="schemaType">The schema type to convert.</param>
    /// <returns>A TypeScript type string.</returns>
    let rec schemaTypeToTs (schemaType: SchemaType) : string =
        match schemaType with
        | SchemaType.SString -> "string"
        | SchemaType.SInt32 -> "number"
        | SchemaType.SInt64 -> "number"
        | SchemaType.SFloat -> "number"
        | SchemaType.SDouble -> "number"
        | SchemaType.SBool -> "boolean"
        | SchemaType.SBytes -> "Uint8Array"
        | SchemaType.SArray elementType ->
            sprintf "%s[]" (schemaTypeToTs elementType)
        | SchemaType.SOption innerType ->
            sprintf "%s | null" (schemaTypeToTs innerType)
        | SchemaType.SRecord(name, _) -> toPascalCase name
        | SchemaType.SUnion(name, _) -> toPascalCase name
        | SchemaType.SRef refName -> toPascalCase refName

    /// <summary>Generates a TypeScript interface from a record schema type.</summary>
    let private generateInterface (name: string) (fields: (string * SchemaType) list) =
        let props =
            fields
            |> List.map (fun (fieldName, fieldType) ->
                sprintf "  %s: %s;" (toCamelCase fieldName) (schemaTypeToTs fieldType))
            |> String.concat "\n"

        sprintf "export interface %s {\n%s\n}" (toPascalCase name) props

    /// <summary>Generates a TypeScript discriminated union type.</summary>
    let private generateUnion (name: string) (cases: (string * SchemaType option) list) =
        let variants =
            cases
            |> List.map (fun (caseName, payload) ->
                match payload with
                | None ->
                    sprintf "  | { tag: '%s' }" caseName
                | Some payloadType ->
                    sprintf "  | { tag: '%s'; value: %s }" caseName (schemaTypeToTs payloadType))
            |> String.concat "\n"

        sprintf "export type %s =\n%s;" (toPascalCase name) variants

    /// <summary>Generates a type definition from a named schema type.</summary>
    let private generateTypeDecl (name: string) (schemaType: SchemaType) =
        match schemaType with
        | SchemaType.SRecord(_, fields) -> generateInterface name fields
        | SchemaType.SUnion(_, cases) -> generateUnion name cases
        | other -> sprintf "export type %s = %s;" (toPascalCase name) (schemaTypeToTs other)

    /// <summary>Collects inline type definitions from endpoints not in the Types map.</summary>
    let private collectInlineTypes (schema: ContractSchema) =
        let collect (st: SchemaType) =
            match st with
            | SchemaType.SRecord(name, fields) when not (schema.Types.ContainsKey name) ->
                [ name, generateInterface name fields ]
            | SchemaType.SUnion(name, cases) when not (schema.Types.ContainsKey name) ->
                [ name, generateUnion name cases ]
            | _ -> []

        schema.Endpoints
        |> List.collect (fun e ->
            let respTypes = collect e.ResponseBody
            let reqTypes = match e.RequestBody with Some b -> collect b | None -> []
            respTypes @ reqTypes)
        |> List.distinctBy fst

    /// <summary>
    /// Generates TypeScript type declaration file content (index.d.ts)
    /// from a ContractSchema.
    /// </summary>
    /// <param name="schema">The contract schema to generate from.</param>
    /// <returns>A TypeScript declaration file string.</returns>
    let generate (schema: ContractSchema) : string =
        let header =
            sprintf "// Auto-generated TypeScript declarations for %s v%s\n// Do not edit manually.\n"
                schema.Name schema.Version

        let typeDecls =
            schema.Types
            |> Map.toList
            |> List.map (fun (name, st) -> generateTypeDecl name st)

        let inlineDecls =
            collectInlineTypes schema
            |> List.map snd

        let allDecls = typeDecls @ inlineDecls

        let methodDecls =
            schema.Endpoints
            |> List.map (fun e ->
                let paramParts =
                    e.Params
                    |> List.filter (fun p -> p.Location <> ParamLocation.Body)
                    |> List.map (fun p ->
                        let tsType = schemaTypeToTs p.Type
                        let optional = if p.Required then "" else "?"
                        sprintf "%s%s: %s" (toCamelCase p.Name) optional tsType)

                let bodyParam =
                    match e.RequestBody with
                    | Some bodyType -> [ sprintf "body: %s" (schemaTypeToTs bodyType) ]
                    | None -> []

                let allParams = paramParts @ bodyParam
                let paramsStr = String.concat ", " allParams
                let returnType = schemaTypeToTs e.ResponseBody

                let deprecatedDoc =
                    if e.IsDeprecated then "/** @deprecated */\n" else ""

                let descDoc =
                    match e.Description with
                    | Some desc -> sprintf "/** %s */\n" desc
                    | None -> ""

                let doc = deprecatedDoc + descDoc

                sprintf "%sexport declare function %s(%s): Promise<%s>;"
                    doc (toCamelCase e.Name) paramsStr returnType)

        let sections =
            [ header ]
            @ (if allDecls.IsEmpty then [] else [ String.concat "\n\n" allDecls ])
            @ (if methodDecls.IsEmpty then [] else [ String.concat "\n\n" methodDecls ])

        String.concat "\n\n" sections + "\n"

    /// <summary>
    /// Generates TypeScript runtime implementation file content (index.ts)
    /// with fetch-based API client. Has no external dependencies.
    /// </summary>
    /// <param name="schema">The contract schema to generate from.</param>
    /// <returns>A TypeScript source file string.</returns>
    let generateRuntime (schema: ContractSchema) : string =
        let header =
            sprintf "// Auto-generated TypeScript client for %s v%s\n// Do not edit manually.\n"
                schema.Name schema.Version

        let typeDecls =
            schema.Types
            |> Map.toList
            |> List.map (fun (name, st) -> generateTypeDecl name st)

        let inlineDecls =
            collectInlineTypes schema
            |> List.map snd

        let allDecls = typeDecls @ inlineDecls

        let clientConfig =
            """export interface ClientConfig {
  baseUrl: string;
  headers?: Record<string, string>;
  fetch?: typeof fetch;
}

let _config: ClientConfig = { baseUrl: '' };

export function configure(config: ClientConfig): void {
  _config = config;
}

async function request<T>(method: string, path: string, body?: unknown): Promise<T> {
  const fetchFn = _config.fetch ?? fetch;
  const url = `${_config.baseUrl}${path}`;
  const headers: Record<string, string> = {
    'Content-Type': 'application/json',
    ...(_config.headers ?? {}),
  };
  const response = await fetchFn(url, {
    method,
    headers,
    body: body !== undefined ? JSON.stringify(body) : undefined,
  });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }
  return response.json() as Promise<T>;
}"""

        let methodImpls =
            schema.Endpoints
            |> List.map (fun e ->
                let httpMethod = methodToString e.Method

                let pathParams =
                    e.Params
                    |> List.filter (fun p -> p.Location = ParamLocation.Path)

                let queryParams =
                    e.Params
                    |> List.filter (fun p -> p.Location = ParamLocation.Query)

                let paramParts =
                    e.Params
                    |> List.filter (fun p -> p.Location <> ParamLocation.Body)
                    |> List.map (fun p ->
                        let tsType = schemaTypeToTs p.Type
                        let optional = if p.Required then "" else "?"
                        sprintf "%s%s: %s" (toCamelCase p.Name) optional tsType)

                let bodyParam =
                    match e.RequestBody with
                    | Some bodyType -> [ sprintf "body: %s" (schemaTypeToTs bodyType) ]
                    | None -> []

                let allParams = paramParts @ bodyParam
                let paramsStr = String.concat ", " allParams
                let returnType = schemaTypeToTs e.ResponseBody

                // Build path with interpolation for path params
                let pathExpr =
                    let mutable p = e.Path
                    for param in pathParams do
                        p <- p.Replace(sprintf "{%s}" param.Name, sprintf "${%s}" (toCamelCase param.Name))
                    p

                // Build query string
                let queryBuild =
                    if queryParams.IsEmpty then
                        ""
                    else
                        let parts =
                            queryParams
                            |> List.map (fun p ->
                                let name = toCamelCase p.Name
                                if p.Required then
                                    sprintf "`%s=${encodeURIComponent(String(%s))}`" p.Name name
                                else
                                    sprintf "...(%s !== undefined ? [`%s=${encodeURIComponent(String(%s))}`] : [])" name p.Name name)
                            |> String.concat ", "

                        sprintf "\n  const queryParts = [%s];\n  const queryString = queryParts.length > 0 ? `?${queryParts.join('&')}` : '';" parts

                let urlSuffix =
                    if queryParams.IsEmpty then "" else " + queryString"

                let bodyArg =
                    match e.RequestBody with
                    | Some _ -> ", body"
                    | None -> ""

                let deprecatedComment =
                    if e.IsDeprecated then "/** @deprecated */\n" else ""

                sprintf "%sexport async function %s(%s): Promise<%s> {%s\n  return request<%s>('%s', `%s`%s%s);\n}"
                    deprecatedComment
                    (toCamelCase e.Name)
                    paramsStr
                    returnType
                    queryBuild
                    returnType
                    httpMethod
                    pathExpr
                    urlSuffix
                    bodyArg)

        let sections =
            [ header ]
            @ (if allDecls.IsEmpty then [] else [ String.concat "\n\n" allDecls ])
            @ [ clientConfig ]
            @ (if methodImpls.IsEmpty then [] else [ String.concat "\n\n" methodImpls ])

        String.concat "\n\n" sections + "\n"
