module FSharp.Wasm.Tests.AuthTests

open Xunit
open FSharp.Wasm.Auth
open FSharp.Wasm.Protocol

// ── Identity ────────────────────────────────────────────────────────────────

[<Fact>]
let ``Identity anonymous is not authenticated`` () =
    Assert.False(Identity.anonymous.IsAuthenticated)

[<Fact>]
let ``Identity create is authenticated`` () =
    let id = Identity.create "sub123" [] "jwt"
    Assert.True(id.IsAuthenticated)
    Assert.Equal("sub123", id.Subject)

[<Fact>]
let ``Identity hasRole returns true when role present`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "admin" } ] "jwt"
    Assert.True(Identity.hasRole "admin" id)

[<Fact>]
let ``Identity hasRole returns false when role absent`` () =
    let id = Identity.create "sub" [] "jwt"
    Assert.False(Identity.hasRole "admin" id)

[<Fact>]
let ``Identity getRoles returns all roles`` () =
    let id =
        Identity.create "sub"
            [ { Type = ClaimTypes.Role; Value = "admin" }
              { Type = ClaimTypes.Role; Value = "editor" } ]
            "jwt"
    let roles = Identity.getRoles id
    Assert.Contains("admin", roles)
    Assert.Contains("editor", roles)

[<Fact>]
let ``Identity hasClaim returns true for matching claim`` () =
    let id = Identity.create "sub" [ { Type = "email"; Value = "a@b.com" } ] "jwt"
    Assert.True(Identity.hasClaim "email" "a@b.com" id)

[<Fact>]
let ``Identity hasClaim returns false for non-matching claim`` () =
    let id = Identity.create "sub" [] "jwt"
    Assert.False(Identity.hasClaim "email" "a@b.com" id)

// ── CapabilityGuard ──────────────────────────────────────────────────────────

[<Fact>]
let ``CapabilityGuard check unauthenticated identity returns ChallengeRequired`` () =
    let result = CapabilityGuard.check (HasRole "admin") Identity.anonymous
    Assert.Equal(ChallengeRequired, result)

[<Fact>]
let ``CapabilityGuard check HasRole authorized when role present`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "admin" } ] "jwt"
    Assert.Equal(Authorized, CapabilityGuard.check (HasRole "admin") id)

[<Fact>]
let ``CapabilityGuard check HasRole denied when role absent`` () =
    let id = Identity.create "sub" [] "jwt"
    match CapabilityGuard.check (HasRole "admin") id with
    | Denied _ -> ()
    | other -> Assert.True(false, sprintf "Expected Denied but got %A" other)

[<Fact>]
let ``CapabilityGuard require shorthand works`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "editor" } ] "jwt"
    Assert.Equal(Authorized, CapabilityGuard.require "editor" id)

[<Fact>]
let ``CapabilityGuard requireAny authorized when any role present`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "editor" } ] "jwt"
    Assert.Equal(Authorized, CapabilityGuard.requireAny [ "admin"; "editor" ] id)

[<Fact>]
let ``CapabilityGuard requireAny denied when no roles present`` () =
    let id = Identity.create "sub" [] "jwt"
    match CapabilityGuard.requireAny [ "admin"; "editor" ] id with
    | Denied _ -> ()
    | other -> Assert.True(false, sprintf "Expected Denied but got %A" other)

[<Fact>]
let ``CapabilityGuard requireAll authorized when all roles present`` () =
    let id =
        Identity.create "sub"
            [ { Type = ClaimTypes.Role; Value = "admin" }
              { Type = ClaimTypes.Role; Value = "editor" } ]
            "jwt"
    Assert.Equal(Authorized, CapabilityGuard.requireAll [ "admin"; "editor" ] id)

[<Fact>]
let ``CapabilityGuard requireAll denied when one role missing`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "admin" } ] "jwt"
    match CapabilityGuard.requireAll [ "admin"; "editor" ] id with
    | Denied _ -> ()
    | other -> Assert.True(false, sprintf "Expected Denied but got %A" other)

[<Fact>]
let ``CapabilityGuard All combinator passes when all pass`` () =
    let id =
        Identity.create "sub"
            [ { Type = ClaimTypes.Role; Value = "r1" }
              { Type = ClaimTypes.Role; Value = "r2" } ]
            "jwt"
    let check = All [ HasRole "r1"; HasRole "r2" ]
    Assert.Equal(Authorized, CapabilityGuard.check check id)

[<Fact>]
let ``CapabilityGuard Any combinator passes when at least one passes`` () =
    let id = Identity.create "sub" [ { Type = ClaimTypes.Role; Value = "r2" } ] "jwt"
    let check = Any [ HasRole "r1"; HasRole "r2" ]
    Assert.Equal(Authorized, CapabilityGuard.check check id)

[<Fact>]
let ``CapabilityGuard Custom predicate works`` () =
    let id = Identity.create "alice" [] "jwt"
    let check = Custom(fun i -> i.Subject = "alice")
    Assert.Equal(Authorized, CapabilityGuard.check check id)

// ── PatchFilter ───────────────────────────────────────────────────────────────

[<Fact>]
let ``PatchFilter filter removes ops that fail predicate`` () =
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops =
            [ PatchOp.DeleteNode(NodeId 1)
              PatchOp.DeleteNode(NodeId 2) ] }
    let filtered = PatchFilter.filter (fun op ->
        match op with
        | PatchOp.DeleteNode(NodeId id) -> id = 1
        | _ -> true) patch
    Assert.Single(filtered.Ops) |> ignore

[<Fact>]
let ``PatchFilter filterByNodeIds keeps only allowed nodes`` () =
    let patch =
        { Magic = GhostPatch.MagicByte
          Version = ModelVersion 0u
          TransactionId = TransactionId 0u
          Ops =
            [ PatchOp.DeleteNode(NodeId 1)
              PatchOp.DeleteNode(NodeId 2)
              PatchOp.DeleteNode(NodeId 3) ] }
    let filtered = PatchFilter.filterByNodeIds (Set.ofList [ 1; 3 ]) patch
    Assert.Equal(2, filtered.Ops.Length)
