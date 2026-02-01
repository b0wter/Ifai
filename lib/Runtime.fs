module Ifai.Lib.Runtime

open System
open System.Threading

type GameMode =
    | EnteringRoom of EnteringRoom.EnteringRoomState
    | Exploring of Exploring.ExploringState
    | LeavingRoom of LeavingRoom.LeavingRoomState
    | Loading of Loading.LoadingState
    | Saving of Saving.SavingState


type Model = {
    World: World
    GameMode: GameMode list
    Language: Language
}


type Event =
    | Exploring of Exploring.ExploringEvent
    | Saving of Saving.SavingEvent
    | RawInput of string
    | ChangeRoom of RoomId


type Parsing =
    | ParseExploringInput of Language * string
    | ParseSavingInput of Language * string
    | ParseLoadingInput of Language * string


type Dispatch =
    | LoadingCmd
    | SavingCmd of Saving.SavingCmd
    | ExploringCmd of Exploring.ExploringCmd
    | EnteringRoomCmd
    | LeavingRoomCmd


/// The global Cmd is split because there are three distinct responsibilities (batching, parsing
type Cmd =
    | Nothing
    /// Relays commands to more state-specific game modes
    | Dispatch of Dispatch
    /// Triggers the parsing process which turns a string into something known (that is game-mode-specific)
    | Parsing of Parsing
    /// Side effect that may result in exceptions, e.g. IO operations
    | Effect of RuntimeAction

    /// Aggregated commands
    | Batch of Cmd list
    

/// <summary>
/// Runs the state-specific parser in the input
/// </summary>
/// <remarks>
/// This function always returns a single `Msg`
/// </remarks>
let runParser (preConfiguredParser: Parsing) : Event =
    match preConfiguredParser with
    | ParseExploringInput (language, input) ->
        input
        |> Exploring.parser language
        |> Exploring.ExploringEvent.UserInput
        |> Event.Exploring
    | ParseSavingInput (language, input) ->
        input
        |> Saving.parser language
        |> Saving.SavingEvent.UserInput
        |> Event.Saving
    | ParseLoadingInput (language, input) ->
        failwith "Parsing loading input has not yet been implemented"


let runEffect (terminate: unit -> unit) (effect: RuntimeAction) : Event option =
    match effect with
    | Print text ->
        failwith "printing text not yet implemented"
    | Quit ->
        do terminate ()
        None


let runDispatch (dispatch: Dispatch) : Event list =
    match dispatch with
    | LoadingCmd -> failwith "todo"
    | SavingCmd cmd -> cmd |> Saving.runCmd |> List.map Event.Saving
    | ExploringCmd cmd -> cmd |> Exploring.runCmd |> List.map Event.Exploring
    | EnteringRoomCmd -> failwith "todo"
    | LeavingRoomCmd -> failwith "todo"


let rec runCmd (terminate: unit -> unit) (cmd: Cmd) : Event list =
    match cmd with
    | Nothing -> []
    | Parsing p -> p |> runParser |> List.singleton
    | Batch batch -> batch |> List.collect (runCmd terminate)
    | Effect e -> e |> runEffect terminate |> Option.map List.singleton |> Option.defaultValue [] //failwith "runCmd was called with a 'Cmd Effect' which should be handled by the global loop"
    | Dispatch dispatch -> dispatch |> runDispatch


let (|InExploring|_|) = function
    | GameMode.Exploring s :: _ -> Some s
    | _ -> None


let (|InSaving|_|) = function
    | GameMode.Saving s :: _ -> Some s
    | _ -> None


let (|InLoading|_|) = function
    | GameMode.Loading s :: _ -> Some s
    | _ -> None


let (|InEnteringRoom|_|) = function
    | GameMode.EnteringRoom s :: _ -> Some s
    | _ -> None


let (|InLeavingRoom|_|) = function
    | GameMode.LeavingRoom s :: _ -> Some s
    | _ -> None


let update (model: Model) (msg: Event) : Model * Cmd =
    let w = model.World
    match model.GameMode, msg with
    | _, Event.RawInput i ->
        match i |> BuiltIns.globalParser model.Language with
        // We can extract globals right here and turn them into global commands
        | Some (BuiltIns.GlobalBuiltIn.Save filename) -> { model with GameMode = (GameMode.Saving { Saving.SavingState.Filename = filename }) :: model.GameMode }, Cmd.Nothing
        | Some (BuiltIns.GlobalBuiltIn.Load _) -> model, Cmd.Nothing
        | Some BuiltIns.GlobalBuiltIn.Quit -> model, Cmd.Effect RuntimeAction.Quit
        // Other input is delegated to the current game mode
        | None ->
            let parser =
                match model.GameMode.Head with
                | GameMode.Exploring _ -> Parsing.ParseExploringInput (model.Language, i)
                | GameMode.Saving _    -> Parsing.ParseSavingInput (model.Language, i)
                | GameMode.Loading _   -> Parsing.ParseLoadingInput (model.Language, i)
                | EnteringRoom enteringRoomState -> failwith "todo"
                | LeavingRoom leavingRoomState -> failwith "todo"

            model, Cmd.Parsing parser
    | GameMode.Exploring state :: rest, Event.Exploring msg ->
        let updatedWord, updatedState, newCmd = Exploring.updateExploring w state msg
        { model with World = updatedWord; GameMode = GameMode.Exploring updatedState :: rest }, newCmd |> Dispatch.ExploringCmd |> Cmd.Dispatch
    | InExploring _, _ ->
        failwith $"Invalid combination, received {msg} while in Exploring mode"
    | GameMode.Saving state :: rest, Event.Saving msg ->
        let updatedWorld, updatedState, newCmd = Saving.updateSaving w state msg
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, newCmd |> Dispatch.SavingCmd |> Cmd.Dispatch
    | InSaving _, _ ->
        failwith $"Invalid combination, received {msg} while in Saving mode"
    | otherMode, msg -> failwith $"Combination of {otherMode} and {msg} not yet implemented"


let run (model: Model) =
    let cts = new CancellationTokenSource()
    let terminate = fun () -> cts.Cancel()
    let agent = MailboxProcessor<Event>.Start(fun inbox ->
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
    