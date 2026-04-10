namespace FSharp.Wasm.Contract

open System

/// <summary>
/// Generates Protocol Buffers (proto3) definitions and gRPC service
/// declarations from a ContractSchema.
/// </summary>
module ProtobufGenerator =

    /// <summary>Converts a name to PascalCase for Protobuf message/service names.</summary>
    let private toPascalCase (s: string) =
        if String.IsNullOrEmpty s then s
        else String.Concat(Char.ToUpperInvariant s.[0], s.Substring 1)

    /// <summary>Converts a name to snake_case for Protobuf field names.</summary>
    let private toSnakeCase (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            s
            |> Seq.mapi (fun i c ->
                if Char.IsUpper c && i > 0 then
                    sprintf "_%c" (Char.ToLowerInvariant c)
                else
                    string (Char.ToLowerInvariant c))
            |> String.concat ""

    /// <summary>
    /// Converts a SchemaType to its Protobuf type representation.
    /// </summary>
    /// <param name="schemaType">The schema type to convert.</param>
    /// <returns>A Protobuf type string.</returns>
    let rec schemaTypeToProto (schemaType: SchemaType) : string =
        match schemaType with
        | SchemaType.SString -> "string"
        | SchemaType.SInt32 -> "int32"
        | SchemaType.SInt64 -> "int64"
        | SchemaType.SFloat -> "float"
        | SchemaType.SDouble -> "double"
        | SchemaType.SBool -> "bool"
        | SchemaType.SBytes -> "bytes"
        | SchemaType.SArray elementType ->
            sprintf "repeated %s" (schemaTypeToProto elementType)
        | SchemaType.SOption innerType ->
            sprintf "optional %s" (schemaTypeToProto innerType)
        | SchemaType.SRecord(name, _) -> toPascalCase name
        | SchemaType.SUnion(name, _) -> toPascalCase name
        | SchemaType.SRef refName -> toPascalCase refName

    /// <summary>Generates a Protobuf message definition from a record.</summary>
    let private generateMessage (name: string) (fields: (string * SchemaType) list) =
        let fieldDefs =
            fields
            |> List.mapi (fun i (fieldName, fieldType) ->
                let protoType = schemaTypeToProto fieldType

                match fieldType with
                | SchemaType.SArray elementType ->
                    sprintf "  repeated %s %s = %d;" (schemaTypeToProto elementType) (toSnakeCase fieldName) (i + 1)
                | SchemaType.SOption innerType ->
                    sprintf "  optional %s %s = %d;" (schemaTypeToProto innerType) (toSnakeCase fieldName) (i + 1)
                | _ ->
                    sprintf "  %s %s = %d;" protoType (toSnakeCase fieldName) (i + 1))
            |> String.concat "\n"

        sprintf "message %s {\n%s\n}" (toPascalCase name) fieldDefs

    /// <summary>Generates a Protobuf message with oneof for a union type.</summary>
    let private generateUnionMessage (name: string) (cases: (string * SchemaType option) list) =
        let caseFields =
            cases
            |> List.mapi (fun i (caseName, payload) ->
                match payload with
                | None ->
                    sprintf "    bool %s = %d;" (toSnakeCase caseName) (i + 1)
                | Some payloadType ->
                    sprintf "    %s %s = %d;" (schemaTypeToProto payloadType) (toSnakeCase caseName) (i + 1))
            |> String.concat "\n"

        sprintf "message %s {\n  oneof value {\n%s\n  }\n}" (toPascalCase name) caseFields

    /// <summary>Generates a message definition from a named schema type.</summary>
    let private generateTypeMessage (name: string) (schemaType: SchemaType) =
        match schemaType with
        | SchemaType.SRecord(_, fields) -> generateMessage name fields
        | SchemaType.SUnion(_, cases) -> generateUnionMessage name cases
        | _ -> sprintf "message %s {\n  %s value = 1;\n}" (toPascalCase name) (schemaTypeToProto schemaType)

    /// <summary>Generates request/response wrapper messages for an endpoint.</summary>
    let private generateEndpointMessages (endpoint: Endpoint) =
        let requestFields =
            let paramFields =
                endpoint.Params
                |> List.mapi (fun i p ->
                    let protoType = schemaTypeToProto p.Type

                    match p.Type with
                    | SchemaType.SArray elementType ->
                        sprintf "  repeated %s %s = %d;"
                            (schemaTypeToProto elementType) (toSnakeCase p.Name) (i + 1)
                    | SchemaType.SOption innerType ->
                        sprintf "  optional %s %s = %d;"
                            (schemaTypeToProto innerType) (toSnakeCase p.Name) (i + 1)
                    | _ ->
                        sprintf "  %s %s = %d;" protoType (toSnakeCase p.Name) (i + 1))

            let bodyField =
                match endpoint.RequestBody with
                | Some bodyType ->
                    let nextIdx = endpoint.Params.Length + 1
                    [ sprintf "  %s body = %d;" (schemaTypeToProto bodyType) nextIdx ]
                | None -> []

            paramFields @ bodyField

        let requestMsg =
            if requestFields.IsEmpty then
                sprintf "message %sRequest {}" (toPascalCase endpoint.Name)
            else
                sprintf "message %sRequest {\n%s\n}"
                    (toPascalCase endpoint.Name) (String.concat "\n" requestFields)

        let responseMsg =
            sprintf "message %sResponse {\n  %s result = 1;\n}"
                (toPascalCase endpoint.Name) (schemaTypeToProto endpoint.ResponseBody)

        requestMsg, responseMsg

    /// <summary>
    /// Generates a complete proto3 file from a ContractSchema,
    /// including message definitions and a gRPC service.
    /// </summary>
    /// <param name="schema">The contract schema to generate from.</param>
    /// <returns>A valid proto3 file string.</returns>
    let generate (schema: ContractSchema) : string =
        let header =
            sprintf """syntax = "proto3";

package %s;

option csharp_namespace = "FSharp.Wasm.Contract.%s";
"""
                (schema.Name.ToLowerInvariant().Replace(" ", "_"))
                (toPascalCase (schema.Name.Replace(" ", "")))

        // Generate type messages
        let typeMessages =
            schema.Types
            |> Map.toList
            |> List.map (fun (name, st) -> generateTypeMessage name st)

        // Generate endpoint request/response messages
        let endpointMessages =
            schema.Endpoints
            |> List.map (fun e ->
                let req, resp = generateEndpointMessages e
                sprintf "%s\n\n%s" req resp)

        // Generate service
        let serviceRpcs =
            schema.Endpoints
            |> List.map (fun e ->
                let streamPrefix =
                    if e.IsStreaming then "stream " else ""

                let deprecatedComment =
                    if e.IsDeprecated then
                        sprintf "  // Deprecated\n"
                    else
                        ""

                sprintf "%s  rpc %s (%sRequest) returns (%s%sResponse);"
                    deprecatedComment
                    (toPascalCase e.Name)
                    (toPascalCase e.Name)
                    streamPrefix
                    (toPascalCase e.Name))
            |> String.concat "\n"

        let service =
            if schema.Endpoints.IsEmpty then
                ""
            else
                sprintf "service %sService {\n%s\n}\n"
                    (toPascalCase (schema.Name.Replace(" ", "")))
                    serviceRpcs

        let allMessages =
            typeMessages @ endpointMessages
            |> String.concat "\n\n"

        sprintf "%s%s\n\n%s" header allMessages service
