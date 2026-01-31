module Ifai.Library.Loop
 
type Msg =
    /// <summary>
    /// Game messages are related to the game's internal logic, like movement
    /// </summary>
    | Game of GameMsg
    /// <summary>
    /// System messages are related to the internal working of the engine, like saving or exiting the game
    /// </summary>
    | System of SystemMsg
    /// <summary>
    /// Sent if the user entered new input
    /// </summary>
    | ParsedInput of ParsedInput
    /// <summary>
    /// Sent if the given input was not understood or did not match the context
    /// </summary>
    | InvalidInput 
       

// A cmd MAY produce a message 
type Cmd<'msg> =
    | Nothing
    | Effect of Effects.Effect
    | Batch of Cmd<'msg> list
    


let runEffect (requestTermination: unit -> unit) world (effect: Effects.Effect) : Msg option =
    match effect with                            
    | Effects.Effect.Print text ->
        printfn $"%s{text}"
        None
    | Effects.ExitGame ->
        do requestTermination ()
        None
    | Effects.SaveGame filename ->
        match saveWorld None (System.Text.Json.JsonSerializerOptions()) world filename with
        | Success -> System (SystemMsg.SaveWorld SaveWorldMsg.SavedSuccessfully) |> Some
        | Failure reason -> System (SystemMsg.SaveWorld (reason |> SaveWorldMsg.SaveFailed)) |> Some
        | FileAlreadyExists -> System (SystemMsg.SaveWorld (SaveWorldMsg.ConfirmOverwrite filename)) |> Some


/// <summary>
/// Handles commands and enqueues any resulting <see cref="Msg"/> into the mailbox
/// </summary>
/// <param name="requestTermination">Function to call that will trigger the application exit</param>
/// <param name="world">Current state of the world</param>
/// <param name="cmd">Command to evaluate</param>
let rec runCmd (requestTermination: unit -> unit) world (cmd: Cmd<Msg>) : Msg list =
    let runEffect = runEffect requestTermination world
    match cmd with
    | Nothing -> []
    | Effect effects -> effects |> runEffect |> Option.map List.singleton |> Option.defaultValue []
    | Cmd.Batch commands -> commands |> List.collect (runCmd requestTermination world)


let update (world: Worlds.World) (message: Msg) : Worlds.World * Cmd<Msg> =
    match world.Mode, message with
    | _, Game gameMessage -> world |> updateViaGameMessage gameMessage
    | _, System systemMessage -> world |> updateViaSystemMessage systemMessage
    | GameMode.Exploring, ParsedInput input ->
        failwith "not implemented"
    | GameMode.AwaitingOverrideSaveFile saveFile, input ->
        failwith "not implemented"
    | _, InvalidInput ->
        world, Effect (Print "Invalid input")


let run (initialWorld: World) =
    let cts = new CancellationTokenSource ()
    let agent = MailboxProcessor<Msg>.Start(fun inbox ->
        let rec loop (currentWorld: World) = async {
            let! msg = inbox.Receive ()
            
            let newWorld, cmd = msg |> update currentWorld

            let newMessages = cmd |> (runCmd (fun () -> cts.Cancel()) newWorld)
            do newMessages |> List.iter inbox.Post
                       
            // sync subscriptions here!
            // if the game mode changes we need different subscriptions
            // e.g. for mapping input to intent
            if cts.Token.IsCancellationRequested then
                printfn "Shutting down, goodbye"
                return ()
            else 
                return! loop newWorld 
        }
        loop initialWorld
    )
    
     
    // change this into a dynamic subscription management so that one can add timers and other funny stuff :)
    let builtIns = [ "exit"; "quit"; "north"; "n"; "south"; "s"; "east"; "e"; "west"; "w"; "wait" ]
    let startInputSubscription () =
        async {
            (* The basic parser should not in any way know of the game or system messages.
               it should only prepare the raw input so that the game/system logic does not need to care about
               trimming/splitting the input *)
            while not cts.Token.IsCancellationRequested do
                let! input = AsyncConsoleReader.AsyncConsole.ReadLineAsync(cts.Token).AsTask() |> Async.AwaitTask
                let msg = input |> (parseInput builtIns) |> ParsedInput
                do msg |> agent.Post
        } |> Async.Start
        
    startInputSubscription ()
    cts.Token.WaitHandle.WaitOne () |> ignore
    printfn "Goodbye :)"