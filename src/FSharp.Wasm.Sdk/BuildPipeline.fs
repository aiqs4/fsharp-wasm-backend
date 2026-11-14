namespace FSharp.Wasm.Sdk

open System.Collections.Generic

/// <summary>Stages of the Wasm build pipeline.</summary>
type BuildStep =
    /// <summary>Collect all F# types from the source project.</summary>
    | TypeCollection
    /// <summary>Run purity/taint analysis on collected types.</summary>
    | PurityAnalysis
    /// <summary>Emit client/edge split artifacts.</summary>
    | SplitEmission
    /// <summary>Generate WIT interface files.</summary>
    | WitGeneration
    /// <summary>Emit ghost sync contract artifacts.</summary>
    | ContractEmission
    /// <summary>Inject runtime shims for host interop.</summary>
    | ShimInjection

/// <summary>Result of executing the build pipeline.</summary>
type BuildResult =
    /// <summary>All steps succeeded with the listed output files.</summary>
    | Success of outputs: string list
    /// <summary>A step failed with an error message.</summary>
    | Failure of step: BuildStep * error: string

/// <summary>An artifact produced by a build step.</summary>
type BuildArtifact =
    { /// <summary>Output file name.</summary>
      FileName: string
      /// <summary>Binary content of the artifact.</summary>
      Content: byte array
      /// <summary>Type descriptor for the artifact (e.g., "wit", "wasm", "json").</summary>
      ArtifactType: string }

/// <summary>Orchestrates the Wasm build pipeline, executing registered steps in order
/// and short-circuiting on the first failure.</summary>
type BuildPipeline(config: SdkConfig) =
    let steps = List<BuildStep * (unit -> Result<BuildArtifact list, string>)>()

    /// <summary>Gets the SDK configuration used by this pipeline.</summary>
    member _.SdkConfig = config

    /// <summary>Register a build step with its execution function.</summary>
    /// <param name="step">The build step identifier.</param>
    /// <param name="execute">Function that produces artifacts or an error.</param>
    member _.addStep (step: BuildStep) (execute: unit -> Result<BuildArtifact list, string>) : unit =
        steps.Add((step, execute))

    /// <summary>Execute all registered steps in order.
    /// Short-circuits on the first failure, returning all accumulated artifacts on success.</summary>
    /// <returns>Ok with all artifacts from all steps, or Error with the failure message.</returns>
    member _.run() : Result<BuildArtifact list, string> =
        let mutable allArtifacts = []
        let mutable error: string option = None
        let mutable failedStep: BuildStep option = None
        let mutable i = 0
        while i < steps.Count && Option.isNone error do
            let step, execute = steps.[i]
            match execute() with
            | Ok artifacts ->
                allArtifacts <- allArtifacts @ artifacts
            | Error msg ->
                error <- Some msg
                failedStep <- Some step
            i <- i + 1
        match error, failedStep with
        | Some msg, Some step ->
            Error(sprintf "Build failed at step %A: %s" step msg)
        | _ ->
            Ok allArtifacts

    /// <summary>Get the ordered list of registered build steps.</summary>
    /// <returns>List of build step identifiers in execution order.</returns>
    member _.getSteps() : BuildStep list =
        steps |> Seq.map fst |> Seq.toList
