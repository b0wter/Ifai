module Ifai.Lib.Runtime

open System
open System.Threading
open Ifai.Lib.Modes


let (|InExploring|_|) = function
    | GameMode.Exploring s :: _ -> Some s
    | _ -> None


let (|InSaving|_|) = function
    | GameMode.Saving s :: _ -> Some s
    | _ -> None


let (|InLoading|_|) = function
    | GameMode.Loading s :: _ -> Some s
    | _ -> None


let (|InTransitioning|_|) = function
    | GameMode.Transitioning s :: _ -> Some s
    | _ -> None


let mapStepResultToGlobalResult<'state, 'event> (modeMap: 'state -> GameMode) (eventMap: 'event -> Event) (model: Model) (r: StepResult<'state, 'event>) : GlobalResult =
    let newGameMode = (r.State |> modeMap) :: model.GameMode.Tail
    GlobalResult.init { model with GameMode = newGameMode; World = r.World }
    |> GlobalResult.withRuntime (r.Runtime |> RuntimeAction.map eventMap)
    |> GlobalResult.withRender r.Render
    |> GlobalResult.withTransition r.Transition


let update (model: Model) (event: Event) : GlobalResult =
    let w = model.World
    match model.GameMode, event with
    | _, Event.RawInput i ->
        match i |> BuiltIns.globalParser model.Language with
        // We can extract globals right here and turn them into global commands
        | Some (BuiltIns.GlobalBuiltIn.Save filename) ->
            GlobalResult.init model
            |> GlobalResult.withTransition (ModeTransition.StartSaving { Filename = filename })
        | Some (BuiltIns.GlobalBuiltIn.Load filename) -> 
            GlobalResult.init model
            |> GlobalResult.withTransition (ModeTransition.StartLoading { Filename = filename })
        | Some BuiltIns.GlobalBuiltIn.Quit ->
            GlobalResult.init model
            |> GlobalResult.withRuntime RuntimeAction.Quit
 
        // Other input is delegated to the current game mode
        | None ->
            let parser =
                match model.GameMode.Head with
                | GameMode.Exploring _     -> Parsing.ParseExploringInput (model.Language, i)
                | GameMode.Saving _        -> Parsing.ParseSavingInput    (model.Language, i)
                | GameMode.Loading _       -> Parsing.ParseLoadingInput   (model.Language, i)
                | GameMode.Transitioning _ -> Parsing.ParseTransitioningInput (model.Language, i)

            GlobalResult.init model
            |> GlobalResult.withRuntime (RuntimeAction.Parsing parser)

    | InExploring state, Event.Exploring event ->
        Exploring.update (StepInput.init w state event)
        |> mapStepResultToGlobalResult GameMode.Exploring Event.Exploring model
    | InExploring _, _ ->
        // Fail fast while we're still developing!
        failwith $"Invalid combination, received {event} while in Exploring mode"
        
    | InSaving state, Event.FileWrittenSuccessfully ->
        Saving.update (StepInput.init w state Saving.SavingEvent.IoSuccess)
        |> mapStepResultToGlobalResult GameMode.Saving Event.Saving model
    | InSaving state, Event.FileWriteFailed error ->
        Saving.update (StepInput.init w state (error |> Saving.SavingEvent.IoFailure))
        |> mapStepResultToGlobalResult GameMode.Saving Event.Saving model
    | InSaving state, Event.FileAlreadyExists f ->
        Saving.update (StepInput.init w state (f |> Saving.SavingEvent.FileAlreadyExists))
        |> mapStepResultToGlobalResult GameMode.Saving Event.Saving model
    | InSaving state, Event.Saving event ->
        Saving.update (StepInput.init w state event)
        |> mapStepResultToGlobalResult GameMode.Saving Event.Saving model
    | InSaving _, _ ->
        failwith $"Invalid combination, received {event} while in Saving mode"

    | InTransitioning state, Event.Transitioning event ->
        Transitioning.update (StepInput.init w state event)
        |> mapStepResultToGlobalResult GameMode.Transitioning Event.Transitioning model
    | InTransitioning _, _ ->
        failwith $"Invalid combination, received {event} while in Transitioning mode"
        
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
    | ParseLoadingInput _ -> failwith "todo"
    | ParseTransitioningInput (language, input) ->
        input
        |> Transitioning.parser language
        |> Transitioning.TransitioningEvent.UserInput
        |> Event.Transitioning


let runAction
        (fileIo: IFileIO)
        (terminate: unit -> unit)
        (action: RuntimeAction<Event>) : Event option =
    match action with
    | Quit ->
        do terminate ()
        None
    | RuntimeAction.Nothing -> None
    | Parsing p -> p |> runParser |> Some
    | WriteFile (filename, allowOverwrite, content) ->
        content
        |> (fileIo.WriteFile filename allowOverwrite) 
        |> (function
            | WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | WriteFileResult.Failure error -> Event.FileWriteFailed error
            | WriteFileResult.AlreadyExists filename -> Event.FileAlreadyExists filename)
        |> Some
    | ReadFile _ -> failwith "todo: not yet implemented"
    | SerializeAndWriteToFile (filename, allowOverwrite, obj) ->
        obj
        |> fileIo.Serialize
        |> Result.map (fileIo.WriteFile filename allowOverwrite)
        |> (function
            | Ok WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | Ok (WriteFileResult.Failure err) -> err |> Event.FileWriteFailed
            | Ok (WriteFileResult.AlreadyExists f) -> f |> Event.FileAlreadyExists
            | Error err -> err |> Event.FileWriteFailed)
        |> Some
    | OfEvent e -> Some e
    // The SaveGame case is special since it requires a current instance of the model. That model is only known to the
    // runtime and that's why the `SaveGame` action is replaced with a `SerializeAndWriteToFile` action with the world
    // as parameter
    | SaveGame _ -> failwith "SaveGame must be resolved by the runtime and cannot be run as an action"
    
    
let rec render (textResources: TextResources) (language: Language) (action: RenderAction) : EngineMessage option =
    // TODO: since there is no scripting or DSL for the game logic there is no sense in having parameterized texts
    let clear = EngineMessage.ClearScreen

    let mapStyle (a: NarrativeStyle) : NarrativeStyleInfo =
        match a with
        | Regular -> NarrativeStyleInfo.Regular
        | Emphasized -> NarrativeStyleInfo.Emphasized
        | Hint -> NarrativeStyleInfo.Hint
        | Dialogue -> NarrativeStyleInfo.Dialogue
        | System -> NarrativeStyleInfo.System

    let render (text: Text.DisplayableText) =
            EngineMessage.NewHistoryItem (text.Text, text.NarrativeStyle |> mapStyle)

    let formatter (text: Text) =
        text
        |> Text.toDisplayable textResources language None
        |> function Ok t -> t | Error t -> t

    let display = formatter >> render
    
    let rec asFunction (renderable: RenderAction) : EngineMessage list =
        match renderable with
        | RenderAction.Nothing -> []
        | RenderAction.Clear -> [clear]
        | RenderAction.Text text -> [text |> display]
        | RenderAction.Fallback s -> 
            let asDisplayable = { Text.DisplayableText.Text = s; Text.DisplayableText.NarrativeStyle = NarrativeStyle.Regular }
            [ asDisplayable |> render ]
        | RenderAction.Batch batch -> batch |> List.collect asFunction

    match action |> asFunction with
    | [] -> None
    | [single] -> Some single
    | many -> many |> Array.ofList |> EngineMessage.Batch |> Some


/// <summary>
/// Pushes of pops a new game mode onto the game mode stack for the given model
/// </summary>
/// <returns>
/// Returns `Some` if the model was changed and `None` otherwise
/// </returns>
let runTransition (transition: ModeTransition) (model: Model) : Model * RuntimeAction<Event> * RenderAction =
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
        action |> RuntimeAction.map Event.Saving,
        render
    | StartLoading loadingModeParameters ->
        let state = loadingModeParameters |> Loading.init
        { model with GameMode = (state |> GameMode.Loading) :: model.GameMode },
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | StartTransition transitionParameters ->
        let result = transitionParameters |> Transitioning.activate
        { model with GameMode = (result.State |> GameMode.Transitioning) :: model.GameMode },
        result.Runtime |> RuntimeAction.map Event.Transitioning,
        result.Render


let constructGameStateInfo (model: Model) : GameStateInfo =
    let asDisplayable t =
        match Text.toDisplayable model.TextResources model.Language None t with
        | Ok d -> d
        | Error e -> e
        
    let constructConnectionInfo (c: Connection<RoomId>) : ConnectionInfo =
        let name = c.Exit |> Exit.asText |> asDisplayable
        let visibility = c.Visibility |> VisibilityInfo.fromVisibility
        { ConnectionInfo.Visibility = visibility
          ConnectionInfo.Description = c.Description |> Option.map asDisplayable
          ConnectionInfo.Name = name }

    let constructRoomInfo (room: Room) : RoomInfo =
        { RoomInfo.Name = room.Name |> asDisplayable
          RoomInfo.Description = room.Description |> asDisplayable
          RoomInfo.Exits = Array.empty
          RoomInfo.Items = Array.empty
          RoomInfo.Characters = Array.empty }

    let room = model.World |> World.currentRoom |> constructRoomInfo
    let player = { PlayerInfo.Inventory = Array.empty }
    { GameStateInfo.Player = player; GameStateInfo.Room = room }
    

let run (fileIo: IFileIO) (model: Model) : Engine =
    let cts = new CancellationTokenSource()
    let terminate = fun () -> cts.Cancel()
    let runAction = runAction fileIo terminate
    let engineMessageEvent = Event<EngineMessageInfo>()
    let sendEngineMessage (message: EngineMessage) = message |> EngineMessageInfo.FromEngineMessage |> engineMessageEvent.Trigger

    let agent = MailboxProcessor<Event>.Start(fun inbox ->
        let rec loop (currentModel: Model) (pendingTransition: ModeTransition option) = async {
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
                    let result = update currentModel event
                    
                    sendEngineMessage (EngineMessage.UpdatedGameState (constructGameStateInfo result.Model))
                    
                    do sendEngineMessage (EngineMessage.DebugOutputResult (result, event))
                    
                    match result.Render |> render model.TextResources model.Language with
                    | Some renderable -> renderable |> sendEngineMessage
                    | _ -> ()

                    do
                       // Since the SaveGame action requires the current `model` it can only be truly constructed in
                       // the runtime. 
                       (match result.Runtime with
                        | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, result.Model)
                        | other -> other)
                        |> runAction
                        |> Option.iter inbox.Post
                    
                    if cts.Token.IsCancellationRequested then
                        do printfn "Shutting down"
                        return ()
                    else
                        match result.Transition, pendingTransition with
                        | ModeTransition.Nothing, None ->
                            return! loop result.Model None
                        | otherTransition, None ->
                            return! loop result.Model (Some otherTransition)
                        | otherTransition, Some pending ->
                            do
                                (EngineMessage.DebugOutputMessage $"Received transition to %A{otherTransition} while having pending transition to %A{pending}, ignoring new transition and keep pending transition")
                                |> sendEngineMessage
                            return! loop result.Model pendingTransition
                | None ->
                    match pendingTransition with
                    | Some t ->
                        let newModel, runtimeAction, renderAction = currentModel |> runTransition t
                        
                        do (match runtimeAction with
                            | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, newModel)
                            | other -> other)
                            |> runAction
                            |> Option.iter inbox.Post
                        
                        match renderAction |> render model.TextResources model.Language with
                        | Some renderable -> renderable |> sendEngineMessage
                        | None -> ()

                        return! loop newModel None
                    | None ->
                        return! loop currentModel None
            with
            | exn ->
                do printfn $"Error while running command: %s{exn.Message}"
                do Console.ForegroundColor <- ConsoleColor.Yellow
                do printfn $"%s{exn.StackTrace}"
                do cts.Cancel ()
                return ()
        }
        loop model None
    )
    
    let inputPort =
        { new IEngineInput with
            member _.Send command =
                if not cts.IsCancellationRequested then
                    match command with
                    | EngineCommand.UserInput input -> input |> Event.RawInput |> agent.Post }
    
    
    {
        Input = inputPort
        Output = engineMessageEvent.Publish
        CancellationTokenSource = cts
    }
    
    (*
    let startInputSubscription () =
        async {
            (* The basic parser should not in any way know of the game or system messages.
               it should only prepare the raw input so that the game/system logic does not need to care about
               trimming/splitting the input *)
            while not cts.Token.IsCancellationRequested do
                let! input = externalInput.ReadInput(cts.Token)
                do input.Trim() |> RawInput |> agent.Post
        } |> Async.Start
    startInputSubscription ()
    *)
    
    //do cts.Token.WaitHandle.WaitOne() |> ignore
