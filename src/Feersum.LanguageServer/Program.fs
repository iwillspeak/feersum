module Feersum.LanguageServer.Program

open System
open Microsoft.Extensions.DependencyInjection
open OmniSharp.Extensions.JsonRpc
open OmniSharp.Extensions.LanguageServer.Server

[<EntryPoint>]
let main _ =
    let server =
        LanguageServer
            .From(fun (options: LanguageServerOptions) ->
                options
                    .WithInput(Console.OpenStandardInput())
                    .WithOutput(Console.OpenStandardOutput())
                    .WithServices(fun services -> services.AddSingleton<DocumentStore>() |> ignore)
                    .AddHandler<FeersumTextDocumentSyncHandler>(JsonRpcHandlerOptions())
                    .AddHandler<FeersumSemanticTokensHandler>(JsonRpcHandlerOptions())
                |> ignore)
            .GetAwaiter()
            .GetResult()

    server.WaitForExit.GetAwaiter().GetResult()
    0
