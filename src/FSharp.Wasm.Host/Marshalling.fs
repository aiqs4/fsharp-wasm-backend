namespace FSharp.Wasm.Host

open System
open System.IO
open System.Text

/// <summary>
/// Fixed-size header describing the binary layout of a serialised HTTP request
/// in shared linear memory.
/// </summary>
type RequestHeader =
    { /// <summary>HTTP method encoded as a single byte (see <see cref="Marshal.methodToByte"/>).</summary>
      Method: byte
      /// <summary>Pointer (offset) to the URL string in the buffer.</summary>
      UrlPtr: int
      /// <summary>Length of the URL string in bytes.</summary>
      UrlLen: int
      /// <summary>Pointer (offset) to the serialised header block.</summary>
      HeadersPtr: int
      /// <summary>Pointer (offset) to the body bytes (0 when absent).</summary>
      BodyPtr: int
      /// <summary>Length of the body in bytes (0 when absent).</summary>
      BodyLen: int }

/// <summary>
/// Marshalling helpers for converting between <see cref="HttpRequest"/>/<see cref="HttpResponse"/>
/// and a compact binary format suitable for shared Wasm linear memory.
/// </summary>
[<RequireQualifiedAccess>]
module Marshal =

    // ── Method byte mapping ──────────────────────────────────────────

    /// <summary>Encode an <see cref="HttpMethod"/> as a single tag byte.</summary>
    let methodToByte (m: HttpMethod) : byte =
        match m with
        | HttpMethod.GET -> 0uy
        | HttpMethod.POST -> 1uy
        | HttpMethod.PUT -> 2uy
        | HttpMethod.DELETE -> 3uy
        | HttpMethod.PATCH -> 4uy
        | HttpMethod.HEAD -> 5uy
        | HttpMethod.OPTIONS -> 6uy
        | HttpMethod.Custom _ -> 7uy

    /// <summary>Decode a tag byte into an <see cref="HttpMethod"/>.</summary>
    let byteToMethod (b: byte) : HttpMethod =
        match b with
        | 0uy -> HttpMethod.GET
        | 1uy -> HttpMethod.POST
        | 2uy -> HttpMethod.PUT
        | 3uy -> HttpMethod.DELETE
        | 4uy -> HttpMethod.PATCH
        | 5uy -> HttpMethod.HEAD
        | 6uy -> HttpMethod.OPTIONS
        | 7uy -> HttpMethod.Custom ""
        | v -> failwithf "Unknown HttpMethod byte: 0x%02X" v

    // ── Internal helpers ─────────────────────────────────────────────

    /// Write a length-prefixed UTF-8 string.
    let private writeString (w: BinaryWriter) (s: string) =
        let bytes = Encoding.UTF8.GetBytes(s)
        w.Write(int32 bytes.Length)
        w.Write(bytes)

    /// Read a length-prefixed UTF-8 string.
    let private readString (r: BinaryReader) : string =
        let len = r.ReadInt32()
        let bytes = r.ReadBytes(len)
        Encoding.UTF8.GetString(bytes)

    /// Write a length-prefixed map.
    let private writeMap (w: BinaryWriter) (m: Map<string, string>) =
        w.Write(int32 m.Count)

        for kv in m do
            writeString w kv.Key
            writeString w kv.Value

    /// Read a length-prefixed map.
    let private readMap (r: BinaryReader) : Map<string, string> =
        let count = r.ReadInt32()

        [ for _ in 1..count do
              let k = readString r
              let v = readString r
              yield k, v ]
        |> Map.ofList

    // ── Request encode / decode ──────────────────────────────────────

    /// <summary>
    /// Serialise an <see cref="HttpRequest"/> into the shared-memory binary layout.
    /// Layout: MethodByte | URL(len-prefixed) | HeaderCount(i32) | Headers(name+value)…
    ///         | HasBody(byte) | Body(len-prefixed)? | QueryParams(map) | PathParams(map)
    ///         | CustomMethod(len-prefixed)?
    /// </summary>
    let encodeRequest (req: HttpRequest) : byte array =
        use ms = new MemoryStream()
        use w = new BinaryWriter(ms)

        // Method
        w.Write(methodToByte req.Method)

        // Custom method string (written only for Custom)
        match req.Method with
        | HttpMethod.Custom name -> writeString w name
        | _ -> ()

        // URL
        writeString w req.Url

        // Headers
        w.Write(int32 req.Headers.Length)

        for h in req.Headers do
            writeString w h.Name
            writeString w h.Value

        // Body
        match req.Body with
        | Some body ->
            w.Write(1uy)
            w.Write(int32 body.Length)
            w.Write(body)
        | None -> w.Write(0uy)

        // Query params & path params
        writeMap w req.QueryParams
        writeMap w req.PathParams

        w.Flush()
        ms.ToArray()

    /// <summary>
    /// Deserialise a byte array produced by <see cref="encodeRequest"/> back into
    /// an <see cref="HttpRequest"/>.
    /// </summary>
    let decodeRequest (data: byte array) : HttpRequest =
        use ms = new MemoryStream(data)
        use r = new BinaryReader(ms)

        let methodByte = r.ReadByte()
        let baseMethod = byteToMethod methodByte

        let method' =
            match baseMethod with
            | HttpMethod.Custom _ -> HttpMethod.Custom(readString r)
            | m -> m

        let url = readString r

        let headerCount = r.ReadInt32()

        let headers =
            [ for _ in 1..headerCount do
                  let n = readString r
                  let v = readString r
                  { Name = n; Value = v } ]

        let hasBody = r.ReadByte()

        let body =
            if hasBody = 1uy then
                let len = r.ReadInt32()
                Some(r.ReadBytes(len))
            else
                None

        let queryParams = readMap r
        let pathParams = readMap r

        { Method = method'
          Url = url
          Headers = headers
          Body = body
          QueryParams = queryParams
          PathParams = pathParams }

    // ── Response encode / decode ─────────────────────────────────────

    /// <summary>
    /// Serialise an <see cref="HttpResponse"/> into a compact binary format.
    /// Layout: StatusCode(i32) | HeaderCount(i32) | Headers… | HasBody(byte)
    ///         | Body(len-prefixed)? | HasContentType(byte) | ContentType(len-prefixed)?
    /// </summary>
    let encodeResponse (resp: HttpResponse) : byte array =
        use ms = new MemoryStream()
        use w = new BinaryWriter(ms)

        w.Write(int32 resp.StatusCode)

        w.Write(int32 resp.Headers.Length)

        for h in resp.Headers do
            writeString w h.Name
            writeString w h.Value

        match resp.Body with
        | Some body ->
            w.Write(1uy)
            w.Write(int32 body.Length)
            w.Write(body)
        | None -> w.Write(0uy)

        match resp.ContentType with
        | Some ct ->
            w.Write(1uy)
            writeString w ct
        | None -> w.Write(0uy)

        w.Flush()
        ms.ToArray()

    /// <summary>
    /// Deserialise a byte array produced by <see cref="encodeResponse"/> back into
    /// an <see cref="HttpResponse"/>.
    /// </summary>
    let decodeResponse (data: byte array) : HttpResponse =
        use ms = new MemoryStream(data)
        use r = new BinaryReader(ms)

        let statusCode = r.ReadInt32()

        let headerCount = r.ReadInt32()

        let headers =
            [ for _ in 1..headerCount do
                  let n = readString r
                  let v = readString r
                  { Name = n; Value = v } ]

        let hasBody = r.ReadByte()

        let body =
            if hasBody = 1uy then
                let len = r.ReadInt32()
                Some(r.ReadBytes(len))
            else
                None

        let hasCt = r.ReadByte()

        let contentType =
            if hasCt = 1uy then
                Some(readString r)
            else
                None

        { StatusCode = statusCode
          Headers = headers
          Body = body
          ContentType = contentType }
