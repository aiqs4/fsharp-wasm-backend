namespace FSharp.Wasm.Dev

open System
open System.Text

/// Options for the build CLI command.
type BuildOptions =
    { /// Target platform (e.g., "wasm", "native").
      Target: string option
      /// Platform specifier (e.g., "browser", "node").
      Platform: string option
      /// Output directory path.
      OutputDir: string option
      /// Whether to enable verbose output.
      Verbose: bool }

/// Options for the split CLI command.
type SplitOptions =
    { /// If true, only check without writing output.
      Check: bool
      /// Optional output file for the split report.
      OutputFile: string option }

/// Options for the deploy CLI command.
type DeployOptions =
    { /// Target platform for deployment.
      Platform: string
      /// If true, simulate deployment without executing.
      DryRun: bool }

/// Options for the dev CLI command.
type DevOptions =
    { /// Optional port override for the dev server.
      Port: int option
      /// Whether to enable file watching.
      Watch: bool
      /// Whether to enable the ghost inspector.
      Inspector: bool }

/// Options for the bench CLI command.
type BenchOptions =
    { /// Optional number of iterations.
      Iterations: int option
      /// Optional name filter for benchmarks.
      Filter: string option }

/// CLI command discriminated union.
type CliCommand =
    | Build of BuildOptions
    | Split of SplitOptions
    | Bundle
    | Deploy of DeployOptions
    | Dev of DevOptions
    | Bench of BenchOptions

/// Parses CLI arguments into CliCommand values.
module CliParser =

    let private tryParseInt (s: string) =
        match Int32.TryParse(s) with
        | true, v -> Some v
        | _ -> None

    let private consumeFlag (args: string list) (flag: string) =
        args |> List.exists ((=) flag),
        args |> List.filter ((<>) flag)

    let private consumeOption (args: string list) (flag: string) =
        match args |> List.tryFindIndex ((=) flag) with
        | Some idx when idx + 1 < args.Length ->
            Some args.[idx + 1],
            args |> List.indexed |> List.filter (fun (i, _) -> i <> idx && i <> idx + 1) |> List.map snd
        | _ -> None, args

    let private parseBuild (args: string list) =
        let target, args = consumeOption args "--target"
        let platform, args = consumeOption args "--platform"
        let outputDir, args = consumeOption args "--output"
        let verbose, _ = consumeFlag args "--verbose"
        Result.Ok (Build { Target = target; Platform = platform; OutputDir = outputDir; Verbose = verbose })

    let private parseSplit (args: string list) =
        let check, args = consumeFlag args "--check"
        let outputFile, _ = consumeOption args "--output"
        Result.Ok (Split { Check = check; OutputFile = outputFile })

    let private parseDeploy (args: string list) =
        let platform, args = consumeOption args "--platform"
        let dryRun, _ = consumeFlag args "--dry-run"
        match platform with
        | Some p -> Result.Ok (Deploy { Platform = p; DryRun = dryRun })
        | None -> Result.Error "deploy requires --platform <platform>"

    let private parseDev (args: string list) =
        let portStr, args = consumeOption args "--port"
        let port = portStr |> Option.bind tryParseInt
        let watch, args = consumeFlag args "--watch"
        let inspector, _ = consumeFlag args "--inspector"
        Result.Ok (Dev { Port = port; Watch = watch; Inspector = inspector })

    let private parseBench (args: string list) =
        let iterStr, args = consumeOption args "--iterations"
        let iterations = iterStr |> Option.bind tryParseInt
        let filter, _ = consumeOption args "--filter"
        Result.Ok (Bench { Iterations = iterations; Filter = filter })

    /// Generate help text for all commands.
    let showHelp () : string =
        let sb = StringBuilder()
        sb.AppendLine("FSharp.Wasm CLI") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("Usage: fsharp-wasm <command> [options]") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("Commands:") |> ignore
        sb.AppendLine("  build    Build the project for WebAssembly") |> ignore
        sb.AppendLine("           --target <target>    Target platform") |> ignore
        sb.AppendLine("           --platform <plat>    Platform specifier") |> ignore
        sb.AppendLine("           --output <dir>       Output directory") |> ignore
        sb.AppendLine("           --verbose            Enable verbose output") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("  split    Analyze client/edge split") |> ignore
        sb.AppendLine("           --check              Check only, don't write") |> ignore
        sb.AppendLine("           --output <file>      Output report file") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("  bundle   Bundle the application") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("  deploy   Deploy the application") |> ignore
        sb.AppendLine("           --platform <plat>    Target platform (required)") |> ignore
        sb.AppendLine("           --dry-run            Simulate deployment") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("  dev      Start the development server") |> ignore
        sb.AppendLine("           --port <port>        Server port") |> ignore
        sb.AppendLine("           --watch              Enable file watching") |> ignore
        sb.AppendLine("           --inspector          Enable ghost inspector") |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine("  bench    Run benchmarks") |> ignore
        sb.AppendLine("           --iterations <n>     Number of iterations") |> ignore
        sb.AppendLine("           --filter <name>      Filter benchmarks by name") |> ignore
        sb.ToString()

    /// Parse CLI arguments into a CliCommand.
    let parse (args: string array) : Result<CliCommand, string> =
        let argList = args |> Array.toList
        match argList with
        | [] -> Result.Error "No command specified. Use --help for usage."
        | cmd :: rest ->
            match cmd.ToLowerInvariant() with
            | "build" -> parseBuild rest
            | "split" -> parseSplit rest
            | "bundle" -> Result.Ok Bundle
            | "deploy" -> parseDeploy rest
            | "dev" -> parseDev rest
            | "bench" -> parseBench rest
            | "--help" | "-h" | "help" -> Result.Error (showHelp ())
            | unknown -> Result.Error (sprintf "Unknown command '%s'. Use --help for usage." unknown)
