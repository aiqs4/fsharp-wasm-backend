namespace FSharp.Wasm.Contract

open System

/// <summary>
/// Generates OpenAPI 3.1 JSON specifications from a ContractSchema.
/// </summary>
module OpenApiGenerator =

    /// <summary>Escapes a string for safe inclusion in JSON.</summary>
    let private escapeJson (s: string) =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")

    /// <summary>Wraps a value in JSON quotes.</summary>
    let private jsonStr (s: string) = "\"" + escapeJson s + "\""

    /// <summary>Converts an EndpointMethod to its lowercase HTTP verb string.</summary>
    let private methodToString (m: EndpointMethod) =
        match m with
        | EndpointMethod.Get -> "get"
        | EndpointMethod.Post -> "post"
        | EndpointMethod.Put -> "put"
        | EndpointMethod.Delete -> "delete"
        | EndpointMethod.Patch -> "patch"

    /// <summary>Joins lines with newline separators.</summary>
    let private lines (xs: string list) = String.concat "\n" xs

    /// <summary>
    /// Converts a SchemaType to its OpenAPI JSON Schema representation.
    /// </summary>
    /// <param name="schemaType">The schema type to convert.</param>
    /// <returns>A JSON string representing the OpenAPI schema.</returns>
    let rec schemaTypeToOpenApi (schemaType: SchemaType) : string =
        match schemaType with
        | SchemaType.SString ->
            "{ \"type\": \"string\" }"
        | SchemaType.SInt32 ->
            "{ \"type\": \"integer\", \"format\": \"int32\" }"
        | SchemaType.SInt64 ->
            "{ \"type\": \"integer\", \"format\": \"int64\" }"
        | SchemaType.SFloat ->
            "{ \"type\": \"number\", \"format\": \"float\" }"
        | SchemaType.SDouble ->
            "{ \"type\": \"number\", \"format\": \"double\" }"
        | SchemaType.SBool ->
            "{ \"type\": \"boolean\" }"
        | SchemaType.SBytes ->
            "{ \"type\": \"string\", \"format\": \"byte\" }"
        | SchemaType.SArray elementType ->
            let itemSchema = schemaTypeToOpenApi elementType
            "{ \"type\": \"array\", \"items\": " + itemSchema + " }"
        | SchemaType.SOption innerType ->
            let innerSchema = schemaTypeToOpenApi innerType
            "{ \"oneOf\": [" + innerSchema + ", { \"type\": \"null\" }] }"
        | SchemaType.SRecord(_name, fields) ->
            let props =
                fields
                |> List.map (fun (fieldName, fieldType) ->
                    "        " + jsonStr fieldName + ": " + schemaTypeToOpenApi fieldType)
                |> String.concat ",\n"

            let required =
                fields
                |> List.map (fun (fieldName, _) -> jsonStr fieldName)
                |> String.concat ", "

            lines [
                "{"
                "      \"type\": \"object\","
                "      \"properties\": {"
                props
                "      },"
                "      \"required\": [" + required + "]"
                "    }"
            ]
        | SchemaType.SUnion(_name, cases) ->
            let variants =
                cases
                |> List.map (fun (caseName, payload) ->
                    match payload with
                    | None ->
                        lines [
                            "      {"
                            "        \"type\": \"object\","
                            "        \"properties\": { \"tag\": { \"type\": \"string\", \"const\": " + jsonStr caseName + " } },"
                            "        \"required\": [\"tag\"]"
                            "      }"
                        ]
                    | Some payloadType ->
                        lines [
                            "      {"
                            "        \"type\": \"object\","
                            "        \"properties\": {"
                            "          \"tag\": { \"type\": \"string\", \"const\": " + jsonStr caseName + " },"
                            "          \"value\": " + schemaTypeToOpenApi payloadType
                            "        },"
                            "        \"required\": [\"tag\", \"value\"]"
                            "      }"
                        ])
                |> String.concat ",\n"

            lines [
                "{"
                "      \"oneOf\": ["
                variants
                "      ],"
                "      \"discriminator\": { \"propertyName\": \"tag\" }"
                "    }"
            ]
        | SchemaType.SRef refName ->
            "{ \"$ref\": \"#/components/schemas/" + escapeJson refName + "\" }"

    /// <summary>Converts a ParamLocation to the OpenAPI "in" string.</summary>
    let private paramLocationToString (loc: ParamLocation) =
        match loc with
        | ParamLocation.Path -> "path"
        | ParamLocation.Query -> "query"
        | ParamLocation.Header -> "header"
        | ParamLocation.Body -> "body"

    /// <summary>
    /// Converts an Endpoint to an OpenAPI path item JSON fragment.
    /// </summary>
    /// <param name="endpoint">The endpoint to convert.</param>
    /// <returns>A JSON string representing the path item operation.</returns>
    let endpointToPathItem (endpoint: Endpoint) : string =
        let method = methodToString endpoint.Method

        let description =
            match endpoint.Description with
            | Some desc ->
                ",\n        \"description\": " + jsonStr desc
            | None -> ""

        let deprecated =
            if endpoint.IsDeprecated then
                ",\n        \"deprecated\": true"
            else
                ""

        let nonBodyParams =
            endpoint.Params
            |> List.filter (fun p -> p.Location <> ParamLocation.Body)

        let parameters =
            if nonBodyParams.IsEmpty then
                ""
            else
                let paramItems =
                    nonBodyParams
                    |> List.map (fun p ->
                        let requiredStr = if p.Required then "true" else "false"
                        lines [
                            "          {"
                            "            \"name\": " + jsonStr p.Name + ","
                            "            \"in\": \"" + paramLocationToString p.Location + "\","
                            "            \"required\": " + requiredStr + ","
                            "            \"schema\": " + schemaTypeToOpenApi p.Type
                            "          }"
                        ])
                    |> String.concat ",\n"

                lines [
                    ","
                    "        \"parameters\": ["
                    paramItems
                    "        ]"
                ]

        let requestBody =
            match endpoint.RequestBody with
            | Some bodyType ->
                lines [
                    ","
                    "        \"requestBody\": {"
                    "          \"required\": true,"
                    "          \"content\": {"
                    "            \"application/json\": {"
                    "              \"schema\": " + schemaTypeToOpenApi bodyType
                    "            }"
                    "          }"
                    "        }"
                ]
            | None -> ""

        let responseSchema = schemaTypeToOpenApi endpoint.ResponseBody

        lines [
            "      \"" + escapeJson method + "\": {"
            "        \"operationId\": " + jsonStr endpoint.Name
                + description + deprecated + parameters + requestBody + ","
            "        \"responses\": {"
            "          \"200\": {"
            "            \"description\": \"Successful response\","
            "            \"content\": {"
            "              \"application/json\": {"
            "                \"schema\": " + responseSchema
            "              }"
            "            }"
            "          }"
            "        }"
            "      }"
        ]

    /// <summary>
    /// Generates a complete OpenAPI 3.1 JSON specification from a ContractSchema.
    /// </summary>
    /// <param name="schema">The contract schema to generate from.</param>
    /// <returns>A valid OpenAPI 3.1 JSON string.</returns>
    let generate (schema: ContractSchema) : string =
        let pathGroups =
            schema.Endpoints
            |> List.groupBy (fun e -> e.Path)

        let paths =
            pathGroups
            |> List.map (fun (path, endpoints) ->
                let operations =
                    endpoints
                    |> List.map endpointToPathItem
                    |> String.concat ",\n"

                lines [
                    "    " + jsonStr path + ": {"
                    operations
                    "    }"
                ])
            |> String.concat ",\n"

        let schemas =
            if schema.Types.IsEmpty then
                ""
            else
                let typeEntries =
                    schema.Types
                    |> Map.toList
                    |> List.map (fun (name, schemaType) ->
                        "      " + jsonStr name + ": " + schemaTypeToOpenApi schemaType)
                    |> String.concat ",\n"

                lines [
                    ","
                    "    \"components\": {"
                    "      \"schemas\": {"
                    typeEntries
                    "      }"
                    "    }"
                ]

        lines [
            "{"
            "  \"openapi\": \"3.1.0\","
            "  \"info\": {"
            "    \"title\": " + jsonStr schema.Name + ","
            "    \"version\": " + jsonStr schema.Version
            "  },"
            "  \"paths\": {"
            paths
            "  }" + schemas
            "}"
        ]
