namespace FSharp.Wasm.Sdk

open System.Collections.Generic

/// <summary>Purity classification for a function in the Wasm build.</summary>
type PurityTag =
    /// <summary>Function is pure with no side effects.</summary>
    | Pure
    /// <summary>Function must run on the edge due to taint sources.</summary>
    | EdgeOnly
    /// <summary>Function is safe to run on the client.</summary>
    | ClientOk

/// <summary>Sources of impurity that taint a function.</summary>
type TaintSource =
    /// <summary>Database access.</summary>
    | Database
    /// <summary>Secret or credential access.</summary>
    | Secrets
    /// <summary>File system access.</summary>
    | FileSystem
    /// <summary>Environment variable access.</summary>
    | Environment
    /// <summary>Cloud service access.</summary>
    | Cloud
    /// <summary>Network I/O access.</summary>
    | Network

/// <summary>Metadata about a function for purity analysis.</summary>
type FunctionInfo =
    { /// <summary>Fully qualified function name.</summary>
      Name: string
      /// <summary>Module containing the function.</summary>
      Module: string
      /// <summary>Declared purity tag before analysis.</summary>
      Purity: PurityTag
      /// <summary>Set of taint sources affecting this function.</summary>
      TaintSources: Set<TaintSource>
      /// <summary>Names of functions called by this function.</summary>
      CalledFunctions: Set<string>
      /// <summary>Whether this function accepts or returns higher-order values.</summary>
      IsHigherOrder: bool }

/// <summary>Static purity/taint analyzer that propagates taints through the call graph
/// and conservatively resolves purity for all registered functions.</summary>
type PurityAnalyzer() =
    let functions = Dictionary<string, FunctionInfo>()

    /// <summary>Register a function for purity analysis.</summary>
    /// <param name="info">Function metadata.</param>
    member _.addFunction(info: FunctionInfo) : unit =
        functions.[info.Name] <- info

    /// <summary>Add a taint source to an already registered function.</summary>
    /// <param name="functionName">Name of the function to taint.</param>
    /// <param name="source">The taint source to add.</param>
    member _.addTaintedSource(functionName: string, source: TaintSource) : unit =
        match functions.TryGetValue(functionName) with
        | true, info ->
            functions.[functionName] <-
                { info with TaintSources = info.TaintSources |> Set.add source }
        | false, _ -> ()

    /// <summary>Propagate taints through the call graph and resolve purity for all functions.
    /// Uses a fixed-point iteration to ensure transitive taint propagation.</summary>
    /// <returns>Map from function name to resolved purity tag.</returns>
    member _.analyze() : Map<string, PurityTag> =
        // Phase 1: propagate taint sources transitively through the call graph
        let taintMap = Dictionary<string, Set<TaintSource>>()
        for kvp in functions do
            taintMap.[kvp.Key] <- kvp.Value.TaintSources

        let mutable changed = true
        while changed do
            changed <- false
            for kvp in functions do
                let caller = kvp.Value
                let mutable callerTaints = taintMap.[caller.Name]
                for callee in caller.CalledFunctions do
                    match taintMap.TryGetValue(callee) with
                    | true, calleeTaints ->
                        let merged = Set.union callerTaints calleeTaints
                        if merged.Count > callerTaints.Count then
                            callerTaints <- merged
                            changed <- true
                    | false, _ -> ()
                taintMap.[caller.Name] <- callerTaints

        // Phase 2: resolve purity based on propagated taints and higher-order status
        let result = Dictionary<string, PurityTag>()
        for kvp in functions do
            let info = kvp.Value
            let taints = taintMap.[kvp.Key]
            let resolvedPurity =
                if not (Set.isEmpty taints) then
                    EdgeOnly
                elif info.IsHigherOrder then
                    // Conservative: higher-order functions may be passed across boundaries
                    EdgeOnly
                else
                    info.Purity
            result.[kvp.Key] <- resolvedPurity

        result |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

    /// <summary>Return all registered functions with their computed purity after analysis.</summary>
    /// <returns>List of function info records with updated purity tags.</returns>
    member this.getReport() : FunctionInfo list =
        let purityMap = this.analyze()
        functions.Values
        |> Seq.map (fun info ->
            match Map.tryFind info.Name purityMap with
            | Some resolved -> { info with Purity = resolved }
            | None -> info)
        |> Seq.toList
