namespace FSharp.Wasm.Contract

/// <summary>
/// Represents a type in the contract schema, used by code generators
/// to produce language-specific type representations.
/// </summary>
[<RequireQualifiedAccess>]
type SchemaType =
    /// <summary>A UTF-8 string value.</summary>
    | SString
    /// <summary>A 32-bit signed integer.</summary>
    | SInt32
    /// <summary>A 64-bit signed integer.</summary>
    | SInt64
    /// <summary>A 32-bit floating-point number.</summary>
    | SFloat
    /// <summary>A 64-bit floating-point number.</summary>
    | SDouble
    /// <summary>A boolean value.</summary>
    | SBool
    /// <summary>A raw byte array.</summary>
    | SBytes
    /// <summary>A homogeneous array of elements.</summary>
    | SArray of SchemaType
    /// <summary>An optional (nullable) value.</summary>
    | SOption of SchemaType
    /// <summary>A named record with typed fields.</summary>
    | SRecord of name: string * fields: (string * SchemaType) list
    /// <summary>A tagged union with named cases.</summary>
    | SUnion of name: string * cases: (string * SchemaType option) list
    /// <summary>A reference to a named type defined elsewhere in the schema.</summary>
    | SRef of name: string

/// <summary>
/// The HTTP method for a contract endpoint.
/// </summary>
[<RequireQualifiedAccess>]
type EndpointMethod =
    | Get
    | Post
    | Put
    | Delete
    | Patch

/// <summary>
/// Where a parameter is located in the HTTP request.
/// </summary>
[<RequireQualifiedAccess>]
type ParamLocation =
    | Path
    | Query
    | Header
    | Body

/// <summary>
/// Describes a single parameter of a contract endpoint.
/// </summary>
type EndpointParam =
    { /// <summary>Parameter name.</summary>
      Name: string
      /// <summary>Parameter type.</summary>
      Type: SchemaType
      /// <summary>Where the parameter appears in the request.</summary>
      Location: ParamLocation
      /// <summary>Whether the parameter is required.</summary>
      Required: bool }

/// <summary>
/// Describes a single endpoint in the contract schema.
/// </summary>
type Endpoint =
    { /// <summary>Endpoint name used for code generation identifiers.</summary>
      Name: string
      /// <summary>HTTP method.</summary>
      Method: EndpointMethod
      /// <summary>URL path pattern (e.g., "/api/users/{id}").</summary>
      Path: string
      /// <summary>Parameters extracted from path, query, headers, or body.</summary>
      Params: EndpointParam list
      /// <summary>Optional request body schema.</summary>
      RequestBody: SchemaType option
      /// <summary>Response body schema.</summary>
      ResponseBody: SchemaType
      /// <summary>Human-readable description for documentation.</summary>
      Description: string option
      /// <summary>Whether this endpoint uses streaming responses.</summary>
      IsStreaming: bool
      /// <summary>Whether this endpoint is deprecated.</summary>
      IsDeprecated: bool }

/// <summary>
/// A complete contract schema describing an API surface,
/// including all endpoints and shared type definitions.
/// </summary>
type ContractSchema =
    { /// <summary>API name.</summary>
      Name: string
      /// <summary>API version string.</summary>
      Version: string
      /// <summary>All endpoints in this contract.</summary>
      Endpoints: Endpoint list
      /// <summary>Named type definitions referenced by endpoints.</summary>
      Types: Map<string, SchemaType> }
