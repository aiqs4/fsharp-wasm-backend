namespace FSharp.Wasm.Ghost

open System.Threading
open FSharp.Wasm.Protocol

/// <summary>Synchronization strategy for ghost-graph state exchange.</summary>
type SyncMode =
    /// <summary>Optimistic: apply locally first, reconcile on conflict.</summary>
    | Optimistic
    /// <summary>Pessimistic: wait for server acknowledgement before applying.</summary>
    | Pessimistic
    /// <summary>Eventual consistency: fire-and-forget with background reconciliation.</summary>
    | EventualConsistency

/// <summary>Configuration for the ghost-sync protocol.</summary>
type SyncConfig =
    { /// <summary>Synchronization strategy.</summary>
      Mode: SyncMode
      /// <summary>Transport channel for patch exchange.</summary>
      Transport: ITransport
      /// <summary>Number of retry attempts on transient failure.</summary>
      RetryCount: int
      /// <summary>Timeout in milliseconds for send/receive operations.</summary>
      TimeoutMs: int }

/// <summary>
/// Integrates the <see cref="ShadowManager"/> with the GGBP transport layer,
/// providing send/receive, acknowledgement, rollback, and continuous sync capabilities.
/// </summary>
type GhostSync(manager: ShadowManager, config: SyncConfig) =

    let mutable pendingTransactions = Map.empty<uint32, Patch>

    /// <summary>
    /// Collect the current dirty patch from the manager, send it via transport,
    /// and return the transaction identifier.
    /// </summary>
    /// <returns>The transaction ID assigned to this patch.</returns>
    member _.sendPatch() : Async<uint32> =
        async {
            let txId = manager.beginTransaction()
            let patch = manager.currentDirtyPatch()

            let outgoing =
                { patch with
                    TransactionId = TransactionId txId }

            pendingTransactions <- pendingTransactions |> Map.add txId outgoing
            manager.commit txId

            let mutable sent = false
            let mutable attempt = 0

            while not sent && attempt < config.RetryCount do
                try
                    do! config.Transport.Send outgoing
                    sent <- true
                with _ ->
                    attempt <- attempt + 1

                    if attempt < config.RetryCount then
                        do! Async.Sleep 100

            if not sent then
                do! config.Transport.Send outgoing

            return txId
        }

    /// <summary>
    /// Receive one patch from the transport and apply it to the manager.
    /// </summary>
    member _.receivePatch() : Async<unit> =
        async {
            use cts = new CancellationTokenSource(config.TimeoutMs)
            let! patchOpt = config.Transport.Receive cts.Token

            match patchOpt with
            | Some patch -> manager.applyPatch patch
            | None -> ()
        }

    /// <summary>
    /// Handle a positive acknowledgement for a previously sent transaction.
    /// Removes the transaction from pending state.
    /// </summary>
    /// <param name="transactionId">The acknowledged transaction.</param>
    member _.handleAck(transactionId: uint32) =
        pendingTransactions <- pendingTransactions |> Map.remove transactionId

    /// <summary>
    /// Handle a negative acknowledgement by applying the rollback patch
    /// and removing the transaction from pending state.
    /// </summary>
    /// <param name="transactionId">The rejected transaction.</param>
    /// <param name="rollbackPatch">The patch that restores pre-transaction state.</param>
    member _.handleNack (transactionId: uint32) (rollbackPatch: Patch) =
        manager.rollback transactionId rollbackPatch
        pendingTransactions <- pendingTransactions |> Map.remove transactionId

    /// <summary>
    /// Begin a continuous receive loop that applies incoming patches.
    /// Runs until the provided cancellation token is triggered or the transport closes.
    /// </summary>
    member _.startSync() : Async<unit> =
        async {
            let! ct = Async.CancellationToken
            let cts = CancellationTokenSource.CreateLinkedTokenSource(ct)

            while not cts.Token.IsCancellationRequested do
                try
                    let! patchOpt = config.Transport.Receive cts.Token

                    match patchOpt with
                    | Some patch -> manager.applyPatch patch
                    | None -> cts.Cancel()
                with
                | :? System.OperationCanceledException -> cts.Cancel()
                | _ -> do! Async.Sleep 100
        }
