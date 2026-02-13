module AiClient

open System.IO
open Xunit
open Microsoft.Extensions.Configuration
open Ifai.Ai
open FsUnit.Xunit

[<Fact>]
let ``AiClient should successfully send a message with default configuration`` () =
    task {
        let config = 
            ConfigurationBuilder()
                .SetBasePath(Directory.GetCurrentDirectory())
                .AddJsonFile("appsettings.json", optional = false)
                .Build()

        let client = AiClient.ForOllama(config)
        
        // We just want to see if it succeeds, contents don't matter.
        let! result = client.PromptAsync("Hello, this is a test message. Respond saying only 'test'")
        
        result |> should equal "test"
    }
