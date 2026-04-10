namespace FSharp.Wasm.Host

open System

/// <summary>HTTP methods supported by the host runtime.</summary>
[<RequireQualifiedAccess>]
type HttpMethod =
    /// <summary>HTTP GET.</summary>
    | GET
    /// <summary>HTTP POST.</summary>
    | POST
    /// <summary>HTTP PUT.</summary>
    | PUT
    /// <summary>HTTP DELETE.</summary>
    | DELETE
    /// <summary>HTTP PATCH.</summary>
    | PATCH
    /// <summary>HTTP HEAD.</summary>
    | HEAD
    /// <summary>HTTP OPTIONS.</summary>
    | OPTIONS
    /// <summary>A non-standard HTTP method.</summary>
    | Custom of string

/// <summary>A single HTTP header (name–value pair).</summary>
type HttpHeader =
    { /// <summary>Header name (e.g. "Content-Type").</summary>
      Name: string
      /// <summary>Header value.</summary>
      Value: string }

/// <summary>An incoming HTTP request from the host environment.</summary>
type HttpRequest =
    { /// <summary>HTTP method of the request.</summary>
      Method: HttpMethod
      /// <summary>Full URL or path of the request.</summary>
      Url: string
      /// <summary>HTTP headers attached to the request.</summary>
      Headers: HttpHeader list
      /// <summary>Optional raw body bytes.</summary>
      Body: byte array option
      /// <summary>Parsed query-string parameters.</summary>
      QueryParams: Map<string, string>
      /// <summary>Path parameters extracted by routing.</summary>
      PathParams: Map<string, string> }

/// <summary>An outgoing HTTP response to return to the host environment.</summary>
type HttpResponse =
    { /// <summary>HTTP status code (e.g. 200, 404).</summary>
      StatusCode: int
      /// <summary>HTTP response headers.</summary>
      Headers: HttpHeader list
      /// <summary>Optional raw body bytes.</summary>
      Body: byte array option
      /// <summary>Content-Type header value, if set.</summary>
      ContentType: string option }

/// <summary>Convenience helpers for building common HTTP responses.</summary>
[<RequireQualifiedAccess>]
module HttpResponse =

    /// <summary>Create a 200 OK response with no body.</summary>
    let ok: HttpResponse =
        { StatusCode = 200
          Headers = []
          Body = None
          ContentType = None }

    /// <summary>Create a 200 JSON response with the given UTF-8 body.</summary>
    let json (body: string) : HttpResponse =
        { StatusCode = 200
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes(body))
          ContentType = Some "application/json" }

    /// <summary>Create a 200 plain-text response.</summary>
    let text (body: string) : HttpResponse =
        { StatusCode = 200
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes(body))
          ContentType = Some "text/plain" }

    /// <summary>Create a 200 HTML response.</summary>
    let html (body: string) : HttpResponse =
        { StatusCode = 200
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes(body))
          ContentType = Some "text/html" }

    /// <summary>Create a 404 Not Found response.</summary>
    let notFound: HttpResponse =
        { StatusCode = 404
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes("Not Found"))
          ContentType = Some "text/plain" }

    /// <summary>Create a 400 Bad Request response.</summary>
    let badRequest: HttpResponse =
        { StatusCode = 400
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes("Bad Request"))
          ContentType = Some "text/plain" }

    /// <summary>Create a 401 Unauthorized response.</summary>
    let unauthorized: HttpResponse =
        { StatusCode = 401
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes("Unauthorized"))
          ContentType = Some "text/plain" }

    /// <summary>Create a 500 Internal Server Error response.</summary>
    let internalError: HttpResponse =
        { StatusCode = 500
          Headers = []
          Body = Some(Text.Encoding.UTF8.GetBytes("Internal Server Error"))
          ContentType = Some "text/plain" }

    /// <summary>Create a 302 redirect response to the given URL.</summary>
    let redirect (url: string) : HttpResponse =
        { StatusCode = 302
          Headers = [ { Name = "Location"; Value = url } ]
          Body = None
          ContentType = None }

    /// <summary>Add a single header to an existing response.</summary>
    let withHeader (name: string) (value: string) (response: HttpResponse) : HttpResponse =
        { response with
            Headers = response.Headers @ [ { Name = name; Value = value } ] }

    /// <summary>Add multiple headers to an existing response.</summary>
    let withHeaders (headers: HttpHeader list) (response: HttpResponse) : HttpResponse =
        { response with
            Headers = response.Headers @ headers }
