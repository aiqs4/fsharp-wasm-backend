namespace FSharp.Wasm.Contract

open System

/// <summary>
/// Generates GraphQL Schema Definition Language (SDL) from a ContractSchema.
/// GET endpoints become Query fields; POST/PUT/DELETE/PATCH become Mutation fields.
/// </summary>
module GraphQlGenerator =

    /// <summary>Converts a name to PascalCase for GraphQL type names.</summary>
    let private toPascalCase (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            String.Concat(Char.ToUpperInvariant s.[0], s.Substring 1)

    /// <summary>Converts a name to camelCase for GraphQL field names.</summary>
    let private toCamelCase (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            String.Concat(Char.ToLowerInvariant s.[0], s.Substring 1)

    /// <summary>
    /// Converts a SchemaType to its GraphQL type representation.
    /// </summary>
    /// <param name="schemaType">The schema type to convert.</param>
    /// <returns>A GraphQL type string.</returns>
    let rec schemaTypeToGraphQl (schemaType: SchemaType) : string =
        match schemaType with
        | SchemaType.SString -> "String!"
        | SchemaType.SInt32 -> "Int!"
        | SchemaType.SInt64 -> "Int!"
        | SchemaType.SFloat -> "Float!"
        | SchemaType.SDouble -> "Float!"
        | SchemaType.SBool -> "Boolean!"
        | SchemaType.SBytes -> "String!"
        | SchemaType.SArray elementType ->
            sprintf "[%s]!" (schemaTypeToGraphQl elementType)
        | SchemaType.SOption innerType ->
            let inner = schemaTypeToGraphQl innerType
            // Remove trailing '!' to make nullable
            if inner.EndsWith "!" then inner.Substring(0, inner.Length - 1) else inner
        | SchemaType.SRecord(name, _) -> sprintf "%s!" (toPascalCase name)
        | SchemaType.SUnion(name, _) -> sprintf "%s!" (toPascalCase name)
        | SchemaType.SRef refName -> sprintf "%s!" (toPascalCase refName)

    /// <summary>Generates a GraphQL type definition for a record.</summary>
    let private generateRecordType (name: string) (fields: (string * SchemaType) list) =
        let fieldDefs =
            fields
            |> List.map (fun (fieldName, fieldType) ->
                sprintf "  %s: %s" (toCamelCase fieldName) (schemaTypeToGraphQl fieldType))
            |> String.concat "\n"

        sprintf "type %s {\n%s\n}" (toPascalCase name) fieldDefs

    /// <summary>Generates a GraphQL union type definition.</summary>
    let private generateUnionType (name: string) (cases: (string * SchemaType option) list) =
        // For unions with payloads, generate wrapper types and a union
        let wrapperTypes =
            cases
            |> List.map (fun (caseName, payload) ->
                match payload with
                | None ->
                    sprintf "type %s%s {\n  _tag: String!\n}" (toPascalCase name) (toPascalCase caseName)
                | Some payloadType ->
                    sprintf "type %s%s {\n  _tag: String!\n  value: %s\n}"
                        (toPascalCase name)
                        (toPascalCase caseName)
                        (schemaTypeToGraphQl payloadType))
            |> String.concat "\n\n"

        let memberTypes =
            cases
            |> List.map (fun (caseName, _) ->
                sprintf "%s%s" (toPascalCase name) (toPascalCase caseName))
            |> String.concat " | "

        sprintf "%s\n\nunion %s = %s" wrapperTypes (toPascalCase name) memberTypes

    /// <summary>Generates a type definition from a SchemaType.</summary>
    let private generateTypeDefinition (name: string) (schemaType: SchemaType) =
        match schemaType with
        | SchemaType.SRecord(_, fields) -> generateRecordType name fields
        | SchemaType.SUnion(_, cases) -> generateUnionType name cases
        | _ -> sprintf "scalar %s" (toPascalCase name)

    /// <summary>
    /// Converts an Endpoint to a GraphQL field definition string.
    /// </summary>
    /// <param name="endpoint">The endpoint to convert.</param>
    /// <returns>A GraphQL field definition string.</returns>
    let endpointToField (endpoint: Endpoint) : string =
        let fieldName = toCamelCase endpoint.Name
        let returnType = schemaTypeToGraphQl endpoint.ResponseBody

        let args =
            let allParams =
                endpoint.Params
                |> List.filter (fun p -> p.Location <> ParamLocation.Body)

            let bodyArg =
                match endpoint.RequestBody with
                | Some bodyType ->
                    [ sprintf "input: %s" (schemaTypeToGraphQl bodyType) ]
                | None -> []

            let paramArgs =
                allParams
                |> List.map (fun p ->
                    let graphQlType = schemaTypeToGraphQl p.Type
                    let argType =
                        if p.Required then graphQlType
                        else
                            // Make nullable if not required
                            if graphQlType.EndsWith "!" then
                                graphQlType.Substring(0, graphQlType.Length - 1)
                            else
                                graphQlType

                    sprintf "%s: %s" (toCamelCase p.Name) argType)

            paramArgs @ bodyArg

        let argsStr =
            if args.IsEmpty then ""
            else sprintf "(%s)" (String.concat ", " args)

        let description =
            match endpoint.Description with
            | Some desc -> sprintf "  \"\"\"%s\"\"\"\n" desc
            | None -> ""

        let deprecated =
            if endpoint.IsDeprecated then " @deprecated"
            else ""

        sprintf "%s  %s%s: %s%s" description fieldName argsStr returnType deprecated

    /// <summary>
    /// Generates a complete GraphQL SDL schema from a ContractSchema.
    /// GET endpoints become Query fields; POST/PUT/DELETE/PATCH become Mutation fields.
    /// </summary>
    /// <param name="schema">The contract schema to generate from.</param>
    /// <returns>A valid GraphQL SDL string.</returns>
    let generate (schema: ContractSchema) : string =
        // Generate type definitions
        let typeDefs =
            schema.Types
            |> Map.toList
            |> List.map (fun (name, schemaType) -> generateTypeDefinition name schemaType)

        // Inline record/union types from endpoints that aren't in Types map
        let inlineTypes =
            let collectTypes (st: SchemaType) =
                match st with
                | SchemaType.SRecord(name, fields) when not (schema.Types.ContainsKey name) ->
                    [ generateRecordType name fields ]
                | SchemaType.SUnion(name, cases) when not (schema.Types.ContainsKey name) ->
                    [ generateUnionType name cases ]
                | _ -> []

            schema.Endpoints
            |> List.collect (fun e ->
                let responseTypes = collectTypes e.ResponseBody

                let requestTypes =
                    match e.RequestBody with
                    | Some body -> collectTypes body
                    | None -> []

                responseTypes @ requestTypes)
            |> List.distinct

        // Split endpoints into queries and mutations
        let queries =
            schema.Endpoints
            |> List.filter (fun e -> e.Method = EndpointMethod.Get)

        let mutations =
            schema.Endpoints
            |> List.filter (fun e -> e.Method <> EndpointMethod.Get)

        let allTypeDefs = typeDefs @ inlineTypes

        let typeSection =
            if allTypeDefs.IsEmpty then ""
            else (String.concat "\n\n" allTypeDefs) + "\n\n"

        let querySection =
            if queries.IsEmpty then
                ""
            else
                let fields =
                    queries
                    |> List.map endpointToField
                    |> String.concat "\n"

                sprintf "type Query {\n%s\n}\n" fields

        let mutationSection =
            if mutations.IsEmpty then
                ""
            else
                let fields =
                    mutations
                    |> List.map endpointToField
                    |> String.concat "\n"

                sprintf "\ntype Mutation {\n%s\n}\n" fields

        let schemaDirective =
            let hasQuery = not queries.IsEmpty
            let hasMutation = not mutations.IsEmpty

            match hasQuery, hasMutation with
            | true, true ->
                "\nschema {\n  query: Query\n  mutation: Mutation\n}\n"
            | true, false ->
                "\nschema {\n  query: Query\n}\n"
            | false, true ->
                "\nschema {\n  mutation: Mutation\n}\n"
            | false, false -> ""

        (typeSection + querySection + mutationSection + schemaDirective).Trim() + "\n"
