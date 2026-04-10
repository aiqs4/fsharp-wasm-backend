namespace FSharp.Wasm.Components

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

/// <summary>Configuration for LLM inference requests.</summary>
type LlmConfig =
    { /// <summary>The model identifier (e.g., "gpt-4", "llama-3").</summary>
      Model: string
      /// <summary>Maximum number of tokens to generate.</summary>
      MaxTokens: int
      /// <summary>Sampling temperature (0.0–2.0).</summary>
      Temperature: float
      /// <summary>Optional nucleus sampling parameter.</summary>
      TopP: float option }

/// <summary>A single message in an LLM conversation.</summary>
type LlmMessage =
    { /// <summary>The role of the message author (e.g., "system", "user", "assistant").</summary>
      Role: string
      /// <summary>The message content.</summary>
      Content: string }

/// <summary>A completed LLM response.</summary>
type LlmResponse =
    { /// <summary>The generated text content.</summary>
      Content: string
      /// <summary>The reason generation stopped (e.g., "stop", "length").</summary>
      FinishReason: string option
      /// <summary>Total tokens consumed by the request.</summary>
      TokensUsed: int }

/// <summary>
/// Interface for LLM providers supporting completion and streaming.
/// </summary>
type ILlmProvider =
    /// <summary>Generate a complete response from the given messages and config.</summary>
    /// <param name="messages">The conversation messages.</param>
    /// <param name="config">The inference configuration.</param>
    /// <returns>The completed response.</returns>
    abstract member Complete : messages: LlmMessage list -> config: LlmConfig -> Async<LlmResponse>

    /// <summary>Stream a response token-by-token from the given messages and config.</summary>
    /// <param name="messages">The conversation messages.</param>
    /// <param name="config">The inference configuration.</param>
    /// <returns>An async enumerable of generated text chunks.</returns>
    abstract member StreamComplete : messages: LlmMessage list -> config: LlmConfig -> IAsyncEnumerable<string>

/// <summary>
/// Mock implementation of <see cref="ILlmProvider"/> for testing.
/// Returns canned responses based on the last message content.
/// </summary>
type MockLlmProvider() =

    let mutable cannedResponse = "This is a mock LLM response."

    /// <summary>Set the canned response text returned by this mock.</summary>
    /// <param name="response">The response text to return.</param>
    member _.SetResponse(response: string) = cannedResponse <- response

    interface ILlmProvider with
        member _.Complete (messages: LlmMessage list) (_config: LlmConfig) =
            async {
                let content =
                    match messages with
                    | [] -> cannedResponse
                    | _ -> sprintf "Response to: %s" (List.last messages).Content

                return
                    { Content = content
                      FinishReason = Some "stop"
                      TokensUsed = content.Length / 4 }
            }

        member _.StreamComplete (messages: LlmMessage list) (_config: LlmConfig) =
            let content =
                match messages with
                | [] -> cannedResponse
                | _ -> sprintf "Response to: %s" (List.last messages).Content

            let words = content.Split(' ')

            { new IAsyncEnumerable<string> with
                member _.GetAsyncEnumerator(cancellationToken: CancellationToken) =
                    let mutable index = -1

                    { new IAsyncEnumerator<string> with
                        member _.Current =
                            if index = 0 then words.[index]
                            else " " + words.[index]

                        member _.MoveNextAsync() =
                            if cancellationToken.IsCancellationRequested then
                                ValueTask<bool>(false)
                            else
                                index <- index + 1
                                ValueTask<bool>(index < words.Length)

                        member _.DisposeAsync() = ValueTask() } }

/// <summary>
/// Convenience module for LLM operations.
/// Delegates to a configurable <see cref="ILlmProvider"/> instance.
/// </summary>
[<RequireQualifiedAccess>]
module Llm =

    let mutable private provider: ILlmProvider = MockLlmProvider() :> ILlmProvider

    let mutable private defaultConfig =
        { Model = "default"
          MaxTokens = 1024
          Temperature = 0.7
          TopP = None }

    /// <summary>Configure the backing LLM provider.</summary>
    /// <param name="llmProvider">The <see cref="ILlmProvider"/> implementation to use.</param>
    let configure (llmProvider: ILlmProvider) = provider <- llmProvider

    /// <summary>Set the default LLM configuration.</summary>
    /// <param name="config">The default configuration to use.</param>
    let setDefaultConfig (config: LlmConfig) = defaultConfig <- config

    /// <summary>Generate a complete response from a simple prompt string.</summary>
    /// <param name="prompt">The user prompt.</param>
    /// <returns>The generated text content.</returns>
    let complete (prompt: string) : Async<string> =
        async {
            let messages = [ { Role = "user"; Content = prompt } ]
            let! response = provider.Complete messages defaultConfig
            return response.Content
        }

    /// <summary>Stream a response token-by-token from a simple prompt string.</summary>
    /// <param name="prompt">The user prompt.</param>
    /// <returns>An async enumerable of generated text chunks.</returns>
    let streamComplete (prompt: string) : IAsyncEnumerable<string> =
        let messages = [ { Role = "user"; Content = prompt } ]
        provider.StreamComplete messages defaultConfig
