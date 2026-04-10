module FSharp.Wasm.Tests.HostTests

open Xunit
open FSharp.Wasm.Host

// ── HttpResponse helpers ──────────────────────────────────────────────────────

[<Fact>]
let ``HttpResponse ok returns 200`` () =
    Assert.Equal(200, HttpResponse.ok.StatusCode)

[<Fact>]
let ``HttpResponse notFound returns 404`` () =
    Assert.Equal(404, HttpResponse.notFound.StatusCode)

[<Fact>]
let ``HttpResponse json sets application/json ContentType`` () =
    let resp = HttpResponse.json "{}"
    Assert.Equal(Some "application/json", resp.ContentType)

[<Fact>]
let ``HttpResponse text sets text/plain ContentType`` () =
    let resp = HttpResponse.text "hello"
    Assert.Equal(Some "text/plain", resp.ContentType)

[<Fact>]
let ``HttpResponse withHeader adds header`` () =
    let resp = HttpResponse.ok |> HttpResponse.withHeader "X-Custom" "value"
    Assert.Contains(resp.Headers, fun h -> h.Name = "X-Custom" && h.Value = "value")

[<Fact>]
let ``HttpResponse badRequest returns 400`` () =
    Assert.Equal(400, HttpResponse.badRequest.StatusCode)

[<Fact>]
let ``HttpResponse unauthorized returns 401`` () =
    Assert.Equal(401, HttpResponse.unauthorized.StatusCode)

[<Fact>]
let ``HttpResponse internalError returns 500`` () =
    Assert.Equal(500, HttpResponse.internalError.StatusCode)

[<Fact>]
let ``HttpResponse redirect returns 302 with Location header`` () =
    let resp = HttpResponse.redirect "/home"
    Assert.Equal(302, resp.StatusCode)
    Assert.Contains(resp.Headers, fun h -> h.Name = "Location" && h.Value = "/home")

// ── Marshalling ───────────────────────────────────────────────────────────────

[<Fact>]
let ``Marshal encodeRequest and decodeRequest round-trip`` () =
    let req =
        { Method = HttpMethod.POST
          Url = "/api/users"
          Headers = [ { Name = "Content-Type"; Value = "application/json" } ]
          Body = Some [| 0x01uy; 0x02uy |]
          QueryParams = Map.ofList [ "page", "1" ]
          PathParams = Map.empty }
    let bytes = Marshal.encodeRequest req
    let decoded = Marshal.decodeRequest bytes
    Assert.Equal(req.Url, decoded.Url)
    Assert.Equal(req.Method, decoded.Method)

[<Fact>]
let ``Marshal methodToByte and byteToMethod round-trip`` () =
    for m in [ HttpMethod.GET; HttpMethod.POST; HttpMethod.PUT; HttpMethod.DELETE; HttpMethod.PATCH ] do
        let b = Marshal.methodToByte m
        Assert.Equal(m, Marshal.byteToMethod b)

// ── Router ───────────────────────────────────────────────────────────────────

let private makeEnv () = MemoryEnv() :> Env

[<Fact>]
let ``Router dispatches GET request to handler`` () =
    let router = Router()
    let mutable called = false
    router.get "/hello" (fun _req _env ->
        called <- true
        async { return HttpResponse.ok })
    let req =
        { Method = HttpMethod.GET
          Url = "/hello"
          Headers = []
          Body = None
          QueryParams = Map.empty
          PathParams = Map.empty }
    router.dispatch req (makeEnv()) |> Async.RunSynchronously |> ignore
    Assert.True(called)

[<Fact>]
let ``Router extracts path parameters`` () =
    let router = Router()
    let mutable capturedId = ""
    router.get "/users/:id" (fun req _env ->
        capturedId <- req.PathParams |> Map.tryFind "id" |> Option.defaultValue ""
        async { return HttpResponse.ok })
    let req =
        { Method = HttpMethod.GET
          Url = "/users/42"
          Headers = []
          Body = None
          QueryParams = Map.empty
          PathParams = Map.empty }
    router.dispatch req (makeEnv()) |> Async.RunSynchronously |> ignore
    Assert.Equal("42", capturedId)

[<Fact>]
let ``Router returns 404 when no route matches`` () =
    let router = Router()
    let req =
        { Method = HttpMethod.GET
          Url = "/unknown"
          Headers = []
          Body = None
          QueryParams = Map.empty
          PathParams = Map.empty }
    let resp = router.dispatch req (makeEnv()) |> Async.RunSynchronously
    Assert.Equal(404, resp.StatusCode)

[<Fact>]
let ``Router registers multiple methods`` () =
    let router = Router()
    router.get "/x" (fun _ _ -> async { return HttpResponse.ok })
    router.post "/x" (fun _ _ -> async { return HttpResponse.ok })
    router.put "/x" (fun _ _ -> async { return HttpResponse.ok })
    router.delete "/x" (fun _ _ -> async { return HttpResponse.ok })
    Assert.Equal(4, router.routes().Length)

// ── MemoryEnv ────────────────────────────────────────────────────────────────

[<Fact>]
let ``MemoryEnv getVar returns value after Set`` () =
    let env = MemoryEnv()
    env.Set("KEY", "value")
    Assert.Equal(Some "value", env.getVar "KEY")

[<Fact>]
let ``MemoryEnv getVar returns None for missing key`` () =
    let env = MemoryEnv()
    Assert.Equal(None, env.getVar "MISSING")

[<Fact>]
let ``MemoryEnv getVarRequired throws for missing key`` () =
    let env = MemoryEnv()
    Assert.Throws<System.Collections.Generic.KeyNotFoundException>(
        fun () -> env.getVarRequired "MISSING" |> ignore)

// ── Platform shim generation ─────────────────────────────────────────────────

[<Fact>]
let ``CloudflareAdapter generates non-empty shim with fetch`` () =
    let adapter = CloudflareAdapter() :> IPlatformAdapter
    let shim = adapter.GenerateShim()
    Assert.NotEmpty(shim)
    Assert.Contains("fetch", shim)

[<Fact>]
let ``CloudflareAdapter platform kind is Cloudflare`` () =
    let adapter = CloudflareAdapter() :> IPlatformAdapter
    Assert.Equal(PlatformKind.Cloudflare, adapter.Platform)

