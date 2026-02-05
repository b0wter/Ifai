module Ifai.Lib.Runtime

open System
open System.Threading
open Ifai.Lib.Modes


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
    TextResources: TextResources
}


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


type Event =
    | Exploring of Exploring.ExploringEvent
    | Saving of Saving.SavingEvent
    | EnteringRoom of EnteringRoom.EnteringRoomEvent
    | LeavingRoom of LeavingRoom.LeavingRoomEvent
    | RawInput of string
    | FileWrittenSuccessfully
    | FileWriteFailed of reason:string
    | FileAlreadyExists of filename:string


let update (model: Model) (event: Event) : Model * RuntimeAction * RenderAction * ModeTransition =
    let w = model.World
    match model.GameMode, event with
    | _, Event.RawInput i ->
        match i |> BuiltIns.globalParser model.Language with
        // We can extract globals right here and turn them into global commands
        | Some (BuiltIns.GlobalBuiltIn.Save filename) -> model, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.StartSaving { Filename = filename }
        | Some (BuiltIns.GlobalBuiltIn.Load _) -> model, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.Nothing
        | Some BuiltIns.GlobalBuiltIn.Quit -> model, RuntimeAction.Quit, RenderAction.Nothing, ModeTransition.Nothing
        // Other input is delegated to the current game mode
        | None ->
            let parser =
                match model.GameMode.Head with
                | GameMode.Exploring _    -> Parsing.ParseExploringInput (model.Language, i)
                | GameMode.Saving _       -> Parsing.ParseSavingInput (model.Language, i)
                | GameMode.Loading _      -> Parsing.ParseLoadingInput (model.Language, i)
                | GameMode.EnteringRoom _ -> Parsing.ParseEnteringRoomInput (model.Language, i)
                | GameMode.LeavingRoom _  -> Parsing.ParseLeavingRoomInput (model.Language, i)
            model, RuntimeAction.Parsing parser, RenderAction.Nothing, ModeTransition.Nothing
    | GameMode.Exploring state :: rest, Event.Exploring msg ->
        let updatedWord, updatedState, runtimeAction, renderAction, modeTransition = Exploring.updateExploring w state msg
        { model with World = updatedWord; GameMode = GameMode.Exploring updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | InExploring _, _ ->
        failwith $"Invalid combination, received {event} while in Exploring mode"
        
    | GameMode.Saving state :: rest, Event.FileWrittenSuccessfully ->
        let updatedWorld, updatedState, runtimeAction, renderAction, modeTransition = Saving.updateSaving w state Saving.SavingEvent.IoSuccess
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | GameMode.Saving state :: rest, Event.FileWriteFailed error ->
        let updatedWorld, updatedState, runtimeAction, renderAction, modeTransition = Saving.updateSaving w state (error |> Saving.SavingEvent.IoFailure)
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | GameMode.Saving state :: rest, Event.FileAlreadyExists f ->
        let updatedWorld, updatedState, runtimeAction, renderAction, modeTransition = Saving.updateSaving w state (f |> Saving.SavingEvent.FileAlreadyExists)
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | GameMode.Saving state :: rest, Event.Saving msg ->
        let updatedWorld, updatedState, runtimeAction, renderAction, modeTransition = Saving.updateSaving w state msg
        { model with World = updatedWorld; GameMode = GameMode.Saving updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | InSaving _, _ ->
        failwith $"Invalid combination, received {event} while in Saving mode"

    | GameMode.LeavingRoom state :: rest, Event.LeavingRoom event ->
        let updatedWorld, updatedState, runtimeAction, renderAction, modeTransition = LeavingRoom.update w state event
        { model with World = updatedWorld; GameMode = GameMode.LeavingRoom updatedState :: rest }, runtimeAction, renderAction, modeTransition
    | InLeavingRoom _, _ ->
        failwith $"Invalid combination, received {event} while in LeavingRoom mode"
        
    | GameMode.EnteringRoom state :: rest, Event.EnteringRoom event ->
        let updatedWorld, updatedState, runtimeAction, renderAction, ModeTransition = EnteringRoom.update w state event
        { model with World = updatedWorld; GameMode = GameMode.EnteringRoom updatedState :: rest }, runtimeAction, renderAction, ModeTransition
    | InEnteringRoom _, _ ->
        failwith $"Invalid combination, received {event} while in EnteringRoom mode"

    | otherMode, msg -> failwith $"Combination of {otherMode} and {msg} not yet implemented"


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
    | ParseLeavingRoomInput (language, input) ->
        input
        |> LeavingRoom.parser language
        |> LeavingRoom.LeavingRoomEvent.UserInput
        |> Event.LeavingRoom
    | ParseEnteringRoomInput (language, input) ->
        input
        |> EnteringRoom.parser language
        |> EnteringRoom.EnteringRoomEvent.UserInput
        |> Event.EnteringRoom
    | _ -> failwith "Invalid parser"


let runAction (writeToFile: string -> bool -> string -> WriteFileResult) (serializer: obj -> Result<string, string>) (terminate: unit -> unit) (action: RuntimeAction) : Event option =
    match action with
    | Quit ->
        do terminate ()
        None
    | RuntimeAction.Nothing -> None
    | Parsing p -> p |> runParser |> Some
    | WriteFile (filename, allowOverwrite, content) ->
        content
        |> (writeToFile filename allowOverwrite) 
        |> (function
            | WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | WriteFileResult.Failure error -> Event.FileWriteFailed error
            | WriteFileResult.AlreadyExists filename -> Event.FileAlreadyExists filename)
        |> Some
    | ReadFile filename -> failwith "todo: not yet implemented"
    | SerializeAndWriteToFile (filename, allowOverwrite, obj) ->
        obj
        |> serializer
        |> Result.map (writeToFile filename allowOverwrite)
        |> (function
            | Ok WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | Ok (WriteFileResult.Failure err) -> err |> Event.FileWriteFailed
            | Ok (WriteFileResult.AlreadyExists f) -> f |> Event.FileAlreadyExists
            | Error err -> err |> Event.FileWriteFailed)
        |> Some
        
    // The SaveGame case is special since it requires a current instance of the model. That model is only known to the
    // runtime and that's why the `SaveGame` action is replaced with a `SerializeAndWriteToFile` action with the world
    // as parameter
    | SaveGame _ -> failwith "SaveGame must be resolved by the runtime and cannot be run as an action"


let printDebug (model: Model) (event: Event) (action: RuntimeAction) (transition: ModeTransition) : unit =
    do Console.ForegroundColor <- ConsoleColor.Yellow
    do Console.Write($"{model.GameMode.Head.GetType().Name}")
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Green
    do Console.Write(event)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Blue
    do Console.Write(action)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Magenta
    do Console.WriteLine(transition)
    do Console.ResetColor ()
    
    
let rec render (textResources: TextResources) (language: Language) (action: RenderAction) =
    // TODO: since there is no scripting or DSL for the game logic there is no sense in having parameterized texts
    let clear = fun () -> Console.Clear ()
    let render (text: Text.DisplayableText) =
        fun () ->
            do Console.ResetColor ()
            do match text.NarrativeStyle with
               | Regular -> ()
               | Emphasized -> Console.ForegroundColor <- ConsoleColor.Magenta
               | Hint -> Console.ForegroundColor <- ConsoleColor.Gray
               | Dialogue -> Console.ForegroundColor <- ConsoleColor.Cyan
               | System -> Console.ForegroundColor <- ConsoleColor.Red
            do text.Text |> Console.WriteLine

    let formatter (text: Text) =
        text
        |> Text.toDisplayable textResources language None
        |> function Ok t -> t | Error t -> t

    let display = formatter >> render
    
    let actions =
        match action with
        | Render text -> [text |> display]
        | RenderMany texts -> texts |> List.map display
        | Clear -> [clear]
        | ClearAndRender text -> [clear; text |> display]
        | ClearAndRenderMany texts -> clear :: (texts |> List.map display)
        | RenderAction.Nothing -> [fun () -> ()]
        | FallbackRender s ->
            let asDisplayable = { Text.DisplayableText.Text = s; Text.DisplayableText.NarrativeStyle = NarrativeStyle.Regular }
            [ asDisplayable |> render ]

    do actions |> List.iter (fun x -> x ())


/// <summary>
/// Pushes of pops a new game mode onto the game mode stack for the given model
/// </summary>
/// <returns>
/// Returns `Some` if the model was changed and `None` otherwise
/// </returns>
let runTransition (transition: ModeTransition) (model: Model) : Model * RuntimeAction * RenderAction =
    match transition with
    | Nothing ->
        model,
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | Finished ->
        { model with GameMode = model.GameMode.Tail },
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | StartExploring ->
        { model with GameMode = ({ Exploring.Foo = 42 } |> GameMode.Exploring) :: model.GameMode },
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | StartSaving savingModeParameters ->
        let state, action, render = savingModeParameters |> Saving.init
        { model with GameMode = (state |> GameMode.Saving) :: model.GameMode },
        action,
        render
    | StartLoading loadingModeParameters ->
        let state = loadingModeParameters |> Loading.init
        { model with GameMode = (state |> GameMode.Loading) :: model.GameMode },
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | StartEnteringRoom enteringRoomParameters ->
        let updatedWorld, state, action, render = enteringRoomParameters |> EnteringRoom.init model.World
        { model with World = updatedWorld; GameMode = (state |> GameMode.EnteringRoom) :: model.GameMode },
        action,
        render
    | StartLeavingRoom leavingModeParameters ->
        { model with GameMode = (leavingModeParameters |> LeavingRoom.init |> GameMode.LeavingRoom) :: model.GameMode },
        RuntimeAction.Nothing,
        RenderAction.Nothing


let run (fileWriter: string -> bool -> string -> WriteFileResult) (serializer: obj -> Result<string, string>) (model: Model) =
    let cts = new CancellationTokenSource()
    let terminate = fun () -> cts.Cancel()
    let runAction = runAction fileWriter serializer terminate
    let mutable pendingTransition : ModeTransition option = None

    let agent = MailboxProcessor<Event>.Start(fun inbox ->
        let rec loop (currentModel: Model) = async {
            try
                (*
                    To transition between states and not lose any messages, the transition works as follows:
                    - we store the pending transition in a mutable variable.
                    - we "drain" the current mailbox, this should be no problem since messages are almost exclusively
                      created through user input
                    - once the mailbox is empty and we cannot dequeue any event, we apply the transition
                *)
                let! maybeEvent = inbox.TryReceive(100)
                match maybeEvent with
                | Some event ->
                    let newModel, runtimeAction, renderAction, transition = update currentModel event
                    
                    do printDebug newModel event runtimeAction transition
                    
                    do renderAction |> render model.TextResources model.Language

                    do
                       // Since the SaveGame action requires the current `model` it can only be truly constructed in
                       // the runtime. 
                       (match runtimeAction with
                        | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, newModel)
                        | other -> other)
                        |> runAction
                        |> Option.iter inbox.Post
                    
                    if not transition.IsNothing then pendingTransition <- Some transition
                    
                    if cts.Token.IsCancellationRequested then
                        do printfn "Shutting down"
                        return ()
                    else
                        return! loop newModel
                | None ->
                    match pendingTransition with
                    | Some t ->
                        pendingTransition <- None
                        let newModel, runtimeAction, renderAction = currentModel |> runTransition t
                        
                        do (match runtimeAction with
                            | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, newModel)
                            | other -> other)
                            |> runAction
                            |> Option.iter inbox.Post
                        
                        do renderAction |> render model.TextResources model.Language
                        return! loop newModel
                    | None ->
                        return! loop currentModel
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
    