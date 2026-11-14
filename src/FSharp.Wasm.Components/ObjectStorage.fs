namespace FSharp.Wasm.Components

open System
open System.Collections.Concurrent

/// <summary>Metadata about a stored object.</summary>
type ObjectInfo =
    { /// <summary>The object key (path).</summary>
      Key: string
      /// <summary>Size of the object in bytes.</summary>
      Size: int64
      /// <summary>MIME content type of the object.</summary>
      ContentType: string
      /// <summary>Timestamp of the last modification.</summary>
      LastModified: DateTimeOffset
      /// <summary>Optional entity tag for conditional operations.</summary>
      ETag: string option }

/// <summary>
/// R2/S3-compatible object storage interface for binary data with
/// content type metadata.
/// </summary>
type IObjectStorage =
    /// <summary>Store an object with the given key, data, and content type.</summary>
    /// <param name="key">The object key.</param>
    /// <param name="data">The binary data to store.</param>
    /// <param name="contentType">The MIME content type.</param>
    abstract member Put : key: string -> data: byte array -> contentType: string -> Async<unit>

    /// <summary>Retrieve an object by key, returning data and content type.</summary>
    /// <param name="key">The object key.</param>
    /// <returns>A tuple of (data, contentType), or None if not found.</returns>
    abstract member Get : key: string -> Async<(byte array * string) option>

    /// <summary>Delete an object by key.</summary>
    /// <param name="key">The object key.</param>
    /// <returns>True if the object existed and was deleted.</returns>
    abstract member Delete : key: string -> Async<bool>

    /// <summary>List objects matching the given prefix.</summary>
    /// <param name="prefix">The key prefix to filter by.</param>
    /// <returns>A list of <see cref="ObjectInfo"/> for matching objects.</returns>
    abstract member List : prefix: string -> Async<ObjectInfo list>

    /// <summary>Get metadata for an object without retrieving its data.</summary>
    /// <param name="key">The object key.</param>
    /// <returns>The <see cref="ObjectInfo"/> if found, or None.</returns>
    abstract member Head : key: string -> Async<ObjectInfo option>

/// <summary>
/// In-memory implementation of <see cref="IObjectStorage"/> for testing.
/// Stores objects as byte arrays with content type metadata.
/// </summary>
type MemoryObjectStorage() =
    let store = ConcurrentDictionary<string, byte array * string * DateTimeOffset>()

    let toObjectInfo (key: string) (data: byte array, contentType: string, lastModified: DateTimeOffset) =
        { Key = key
          Size = int64 data.Length
          ContentType = contentType
          LastModified = lastModified
          ETag = None }

    interface IObjectStorage with
        member _.Put (key: string) (data: byte array) (contentType: string) =
            async { store.[key] <- (data, contentType, DateTimeOffset.UtcNow) }

        member _.Get(key: string) =
            async {
                match store.TryGetValue(key) with
                | true, (data, contentType, _) -> return Some(data, contentType)
                | _ -> return None
            }

        member _.Delete(key: string) =
            async {
                let mutable removed = Unchecked.defaultof<byte array * string * DateTimeOffset>
                return store.TryRemove(key, &removed)
            }

        member _.List(prefix: string) =
            async {
                return
                    store
                    |> Seq.filter (fun kvp -> kvp.Key.StartsWith(prefix))
                    |> Seq.map (fun kvp -> toObjectInfo kvp.Key kvp.Value)
                    |> Seq.sortBy (fun info -> info.Key)
                    |> Seq.toList
            }

        member _.Head(key: string) =
            async {
                match store.TryGetValue(key) with
                | true, value -> return Some(toObjectInfo key value)
                | _ -> return None
            }
