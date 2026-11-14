namespace FSharp.Wasm.Dev

open System
open System.IO
open System.Collections.Generic

/// Kind of file system change detected.
type FileChangeKind =
    | Created
    | Modified
    | Deleted
    | Renamed

/// Represents a single file change event.
type FileChange =
    { /// Full path to the changed file.
      Path: string
      /// Kind of change that occurred.
      Kind: FileChangeKind
      /// When the change was detected.
      Timestamp: DateTimeOffset }

/// Watches file system paths for changes and invokes callbacks on changes.
type FileWatcher(watchPaths: string list, extensions: string list) =
    let watchers = ResizeArray<FileSystemWatcher>()
    let recentChanges = LinkedList<FileChange>()
    let lockObj = obj ()
    let mutable watching = false
    let maxRecentChanges = 1000

    let addChange (change: FileChange) =
        lock lockObj (fun () ->
            recentChanges.AddLast(change) |> ignore
            if recentChanges.Count > maxRecentChanges then
                recentChanges.RemoveFirst())

    let matchesExtension (path: string) =
        if extensions.IsEmpty then true
        else extensions |> List.exists (fun ext -> path.EndsWith(ext, StringComparison.OrdinalIgnoreCase))

    /// Starts watching for file changes, invoking callback on each change.
    member _.start(callback: FileChange -> unit) =
        if not watching then
            for watchPath in watchPaths do
                if Directory.Exists(watchPath) then
                    let watcher = new FileSystemWatcher(watchPath)
                    watcher.IncludeSubdirectories <- true
                    watcher.EnableRaisingEvents <- true

                    let handle kind (e: FileSystemEventArgs) =
                        if matchesExtension e.FullPath then
                            let change = { Path = e.FullPath; Kind = kind; Timestamp = DateTimeOffset.UtcNow }
                            addChange change
                            callback change

                    watcher.Created.Add(handle Created)
                    watcher.Changed.Add(handle Modified)
                    watcher.Deleted.Add(handle Deleted)
                    watcher.Renamed.Add(fun e -> handle Renamed e)
                    watchers.Add(watcher)

            watching <- true

    /// Stops watching for file changes and disposes watchers.
    member _.stop() =
        for w in watchers do
            w.EnableRaisingEvents <- false
            w.Dispose()
        watchers.Clear()
        watching <- false

    /// Returns the most recent file changes, up to count.
    member _.getRecentChanges(count: int) : FileChange list =
        lock lockObj (fun () ->
            recentChanges
            |> Seq.toList
            |> List.rev
            |> List.truncate count)

    /// Whether the watcher is currently active.
    member _.IsWatching = watching
