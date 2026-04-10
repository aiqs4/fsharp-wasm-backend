namespace FSharp.Wasm.Dev

open System
open System.Diagnostics

/// Configuration for the local development server.
type DevServerConfig =
    { /// Port to listen on.
      Port: int
      /// Directories to watch for file changes.
      WatchPaths: string list
      /// Whether to automatically reload on file changes.
      AutoReload: bool
      /// Whether the ghost inspector endpoint is enabled.
      InspectorEnabled: bool
      /// Port for the ghost inspector.
      InspectorPort: int
      /// Whether to emit verbose logging.
      Verbose: bool }

/// State of the development server.
type DevServerState =
    | Starting
    | Running
    | Reloading
    | Stopped
    | Error of string

/// Local development server with file watching, auto-reload, and inspector support.
type DevServer(config: DevServerConfig) =
    let mutable state = Stopped
    let mutable startTime = DateTimeOffset.UtcNow
    let mutable reloadCount = 0
    let watcher = FileWatcher(config.WatchPaths, [ ".fs"; ".fsx"; ".fsproj" ])

    let log msg =
        if config.Verbose then
            printfn "[DevServer] %s" msg

    /// Start the development server.
    member _.start() : Async<unit> =
        async {
            state <- Starting
            startTime <- DateTimeOffset.UtcNow
            log (sprintf "Starting dev server on port %d..." config.Port)

            if config.AutoReload then
                watcher.start (fun change ->
                    log (sprintf "File changed: %s (%A)" change.Path change.Kind))

            if config.InspectorEnabled then
                log (sprintf "Ghost inspector enabled on port %d" config.InspectorPort)

            state <- Running
            log "Dev server is running."
        }

    /// Stop the development server.
    member _.stop() : unit =
        log "Stopping dev server..."
        watcher.stop ()
        state <- Stopped
        log "Dev server stopped."

    /// Trigger a rebuild and reload.
    member _.reload() : Async<unit> =
        async {
            state <- Reloading
            reloadCount <- reloadCount + 1
            log (sprintf "Reload #%d triggered..." reloadCount)

            // Simulate rebuild delay
            do! Async.Sleep 100

            state <- Running
            log "Reload complete."
        }

    /// Current state of the server.
    member _.State : DevServerState = state

    /// Time elapsed since the server started.
    member _.Uptime : TimeSpan =
        match state with
        | Running | Reloading -> DateTimeOffset.UtcNow - startTime
        | _ -> TimeSpan.Zero

    /// Number of reloads performed since start.
    member _.ReloadCount : int = reloadCount
