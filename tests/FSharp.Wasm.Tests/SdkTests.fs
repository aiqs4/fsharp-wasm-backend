module FSharp.Wasm.Tests.SdkTests

open Xunit
open FSharp.Wasm.Protocol
open FSharp.Wasm.Sdk

// ── PurityAnalyzer ────────────────────────────────────────────────────────────

[<Fact>]
let ``PurityAnalyzer pure function stays pure`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "pureAdd"
          Module = "App"
          Purity = Pure
          TaintSources = Set.empty
          CalledFunctions = Set.empty
          IsHigherOrder = false }
    let result = analyzer.analyze()
    Assert.Equal(Pure, result.["pureAdd"])

[<Fact>]
let ``PurityAnalyzer tainted function becomes EdgeOnly`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "saveData"
          Module = "App"
          Purity = Pure
          TaintSources = Set.singleton Database
          CalledFunctions = Set.empty
          IsHigherOrder = false }
    let result = analyzer.analyze()
    Assert.Equal(EdgeOnly, result.["saveData"])

[<Fact>]
let ``PurityAnalyzer taint propagates through call chain`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "dbSave"
          Module = "App"
          Purity = Pure
          TaintSources = Set.singleton Database
          CalledFunctions = Set.empty
          IsHigherOrder = false }
    analyzer.addFunction
        { Name = "saveUser"
          Module = "App"
          Purity = Pure
          TaintSources = Set.empty
          CalledFunctions = Set.singleton "dbSave"
          IsHigherOrder = false }
    let result = analyzer.analyze()
    Assert.Equal(EdgeOnly, result.["saveUser"])

[<Fact>]
let ``PurityAnalyzer higher-order function becomes EdgeOnly conservatively`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "applyFn"
          Module = "App"
          Purity = Pure
          TaintSources = Set.empty
          CalledFunctions = Set.empty
          IsHigherOrder = true }
    let result = analyzer.analyze()
    Assert.Equal(EdgeOnly, result.["applyFn"])

[<Fact>]
let ``PurityAnalyzer addTaintedSource adds taint`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "fn"
          Module = "App"
          Purity = Pure
          TaintSources = Set.empty
          CalledFunctions = Set.empty
          IsHigherOrder = false }
    analyzer.addTaintedSource("fn", Secrets)
    let result = analyzer.analyze()
    Assert.Equal(EdgeOnly, result.["fn"])

[<Fact>]
let ``PurityAnalyzer getReport returns updated purity`` () =
    let analyzer = PurityAnalyzer()
    analyzer.addFunction
        { Name = "fn"
          Module = "App"
          Purity = Pure
          TaintSources = Set.singleton Network
          CalledFunctions = Set.empty
          IsHigherOrder = false }
    let report = analyzer.getReport()
    let fn = report |> List.find (fun f -> f.Name = "fn")
    Assert.Equal(EdgeOnly, fn.Purity)

// ── FieldMapGenerator ─────────────────────────────────────────────────────────

[<Fact>]
let ``FieldMapGenerator hashFieldName returns consistent hash`` () =
    let h1 = FieldMapGenerator.hashFieldName "Score"
    let h2 = FieldMapGenerator.hashFieldName "Score"
    Assert.Equal(h1, h2)

[<Fact>]
let ``FieldMapGenerator hashFieldName different names produce different hashes`` () =
    let h1 = FieldMapGenerator.hashFieldName "Name"
    let h2 = FieldMapGenerator.hashFieldName "Score"
    Assert.NotEqual(h1, h2)

[<Fact>]
let ``FieldMapGenerator generateFieldMap creates FieldMap`` () =
    let records : FieldMapGenerator.RecordDefinition list =
        [ { Name = "User"
            RecordId = 1us
            Fields = [ ("Name", 0uy, WasmType.RefString); ("Score", 1uy, WasmType.I32) ] } ]
    let fm = FieldMapGenerator.generateFieldMap records
    Assert.Equal(1, fm.Records.Length)
    Assert.Equal(RecordId 1us, fm.Records.[0].RecordId)
    Assert.Equal(2, fm.Records.[0].Fields.Length)

[<Fact>]
let ``FieldMapGenerator encodeFieldMap and decodeFieldMap round-trip`` () =
    let records : FieldMapGenerator.RecordDefinition list =
        [ { Name = "Point"
            RecordId = 2us
            Fields = [ ("X", 0uy, WasmType.F64); ("Y", 1uy, WasmType.F64) ] } ]
    let fm = FieldMapGenerator.generateFieldMap records
    let bytes = FieldMapGenerator.encodeFieldMap fm
    let decoded = FieldMapGenerator.decodeFieldMap bytes
    Assert.Equal(1, decoded.Records.Length)
    Assert.Equal(RecordId 2us, decoded.Records.[0].RecordId)
    Assert.Equal(2, decoded.Records.[0].Fields.Length)

// ── WitGenerator ─────────────────────────────────────────────────────────────

[<Fact>]
let ``WitGenerator generates valid WIT output`` () =
    let world : WitWorld =
        { Name = "my-app"
          Imports =
            [ { Name = "types"
                Functions =
                    [ { Name = "get-score"
                        Params = [ ("id", WitType.WitU32) ]
                        Results = [ WitType.WitS32 ] } ] } ]
          Exports =
            [ { Name = "handler"
                Functions =
                    [ { Name = "handle"
                        Params = [ ("req", WitType.WitString) ]
                        Results = [ WitType.WitString ] } ] } ] }
    let wit = WitGenerator.generateWit world
    Assert.Contains("world my-app", wit)
    Assert.Contains("import", wit)
    Assert.Contains("export", wit)
    Assert.Contains("get-score", wit)
    Assert.Contains("handle", wit)

[<Fact>]
let ``WitGenerator witTypeToString converts primitive types`` () =
    Assert.Equal("u32", WitGenerator.witTypeToString WitType.WitU32)
    Assert.Equal("string", WitGenerator.witTypeToString WitType.WitString)
    Assert.Equal("bool", WitGenerator.witTypeToString WitType.WitBool)
    Assert.Equal("f64", WitGenerator.witTypeToString WitType.WitF64)

// ── ProjectConfig ─────────────────────────────────────────────────────────────

[<Fact>]
let ``ProjectConfig defaultConfig has expected values`` () =
    let cfg = ProjectConfig.defaultConfig
    Assert.Equal(WasmTarget.Both, cfg.Target)

[<Fact>]
let ``ProjectConfig defaultGcFlags enables all flags`` () =
    let flags = ProjectConfig.defaultGcFlags
    Assert.True(flags.EnableGc)
    Assert.True(flags.EnableTailCall)
    Assert.True(flags.EnableBulkMemory)
    Assert.True(flags.EnableReferenceTypes)
