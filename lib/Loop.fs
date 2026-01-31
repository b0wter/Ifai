module Ifai.Lib.Loop

open System
open System.Threading

type GameMode =
    | Exploring of Exploring.ExploringState
    | Loading of unit
    | Saving of Saving.SavingState


type Model = {
    World: World
    GameMode: GameMode list
    Language: Language
}


type Msg =
    | Exploring of Exploring.ExploringMsg
    | Saving of Saving.SavingMsg
    | RawInput of string
    | ChangeRoom of RoomId


type Effect =
    | Quit
    | Print of Text


type Cmd =
    | Nothing
    | Parse of (unit -> Msg) // Commands need to be closed, meaning that they cannot take arguments
    | ExploringCmd of Exploring.ExploringCmd
    | SavingCmd of Saving.SavingCmd
    | Batch of Cmd list
    | Effect of Effect


let rec runCmd (terminate: unit -> unit) (cmd: Cmd) : Msg list =
    match cmd with
    | Nothing -> []
    | ExploringCmd cmd -> cmd |> Exploring.runCmd |> List.map Msg.Exploring
    | SavingCmd cmd -> cmd |> Saving.runCmd |> List.map Msg.Saving
    | Batch batch -> batch |> List.collect (runCmd terminate)
    | Parse preconfiguredParser -> [ preconfiguredParser () ]
    | Quit ->
        do terminate ()
        []


let update (model: Model) (msg: Msg) : Model * Cmd =
    let w = model.World
    match model.GameMode, msg with
    | _, Msg.RawInput i ->
        match i |> BuiltIns.globalParser model.Language with
        // We can extract globals right here and turn them into global commands
        | Some (BuiltIns.GlobalBuiltIn.Save filename) -> { model with GameMode = (GameMode.Saving { Saving.SavingState.Filename = filename }) :: model.GameMode }, Cmd.Nothing
        | Some (BuiltIns.GlobalBuiltIn.Load _) -> model, Cmd.Nothing
        | Some BuiltIns.GlobalBuiltIn.Quit -> model, Cmd.Quit
        // Other input is delegated to the current game mode
        | None ->
            let parser =
                match model.GameMode.Head with
                | GameMode.Exploring _ -> (Exploring.parser model.Language) >> Exploring.ExploringMsg.UserInput >> Msg.Exploring
                | GameMode.Saving _ -> (Saving.parser model.Language) >> Saving.SavingMsg.UserInput >> Msg.Saving
                | GameMode.Loading _ -> failwith "RawInput -> No global match -> LoadingMode: not implemented"
            let command = Cmd.Parse (fun () -> i |> parser)
            model, command
    | GameMode.Exploring e :: rest, Msg.Exploring exploringMsg ->
        let updatedWord, updatedState, newCmd = Exploring.updateExploring w e exploringMsg
        { model with World = updatedWord; GameMode = GameMode.Exploring updatedState :: rest }, newCmd |> Cmd.ExploringCmd
    | GameMode.Exploring _ :: _, _ ->
        failwith $"Invalid combination, received {msg} while in Exploring mode"
    | GameMode.Saving s :: rest, Msg.Saving savingMsg ->
        let updatedWorld, updatedState, newCmd = Saving.updateSaving w s savingMsg
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, newCmd |> Cmd.SavingCmd
    | GameMode.Saving _ :: _, _ ->
        failwith $"Invalid combination, received {msg} while in Saving mode"
    | otherMode, msg -> failwith $"Combination of {otherMode} and {msg} not yet implemented"


let run (model: Model) =
    let cts = new CancellationTokenSource()
    let terminate = fun () -> cts.Cancel()
    let agent = MailboxProcessor<Msg>.Start(fun inbox ->
        let rec loop (currentModel: Model) = async {
            try
                let! msg = inbox.Receive()
                let newModel, cmd = update currentModel msg
                let newMsg = cmd |> (runCmd terminate)
                do newMsg |> List.iter (fun msg -> msg |> inbox.Post)
                
                if cts.Token.IsCancellationRequested then
                    do printfn "Shutting down"
                    return ()
                else
                    return! loop newModel
            with
            | exn ->
                do printfn $"Error while running command: %s{exn.Message}"
                do Console.ForegroundColor <- ConsoleColor.Yellow
                do printfn $"%s{exn.StackTrace}"
                do cts.Cancel ()
                return ()
        }
        loop model
    )
    
    let startInputSubscription () =
        async {
            (* The basic parser should not in any way know of the game or system messages.
               it should only prepare the raw input so that the game/system logic does not need to care about
               trimming/splitting the input *)
            while not cts.Token.IsCancellationRequested do
                let! input = AsyncConsoleReader.AsyncConsole.ReadLineAsync(cts.Token).AsTask() |> Async.AwaitTask
                do input.Trim() |> RawInput |> agent.Post
        } |> Async.Start
    
    startInputSubscription ()
    do cts.Token.WaitHandle.WaitOne() |> ignore
    