module Feersum.LanguageServer.Program

open System
open Ionide.LanguageServerProtocol

[<EntryPoint>]
let main _ =

    let input = Console.OpenStandardInput()
    let output = Console.OpenStandardOutput()

    let requestHandlings = Server.defaultRequestHandlings ()

    Server.start
        requestHandlings
        input
        output
        (fun (nots, reqs) -> new FeersumClient(nots, reqs))
        (fun client -> new FeersumServer(client))
        Server.defaultRpc
    |> (int)
