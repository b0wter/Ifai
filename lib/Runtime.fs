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


let mapStepResultToGlobalResult<'state> (modeMap: 'state -> GameMode) (model: Model) (r: StepResult<'state>) : GlobalResult =
    let newGameMode = (r.State |> modeMap) :: model.GameMode.Tail
    GlobalResult.init { model with GameMode = newGameMode; World = r.World }
    |> GlobalResult.withRuntime r.Runtime
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
        |> mapStepResultToGlobalResult GameMode.Exploring model
    | InExploring _, _ ->
        // Fail fast while we're still developing!
        failwith $"Invalid combination, received {event} while in Exploring mode"
        
    | InSaving state, Event.FileWrittenSuccessfully ->
        Saving.update (StepInput.init w state Saving.SavingEvent.IoSuccess)
        |> mapStepResultToGlobalResult GameMode.Saving model
    | InSaving state, Event.FileWriteFailed error ->
        Saving.update (StepInput.init w state (error |> Saving.SavingEvent.IoFailure))
        |> mapStepResultToGlobalResult GameMode.Saving model
    | InSaving state, Event.FileAlreadyExists f ->
        Saving.update (StepInput.init w state (f |> Saving.SavingEvent.FileAlreadyExists))
        |> mapStepResultToGlobalResult GameMode.Saving model
    | InSaving state, Event.Saving event ->
        Saving.update (StepInput.init w state event)
        |> mapStepResultToGlobalResult GameMode.Saving model
    | InSaving _, _ ->
        failwith $"Invalid combination, received {event} while in Saving mode"

    | InTransitioning state, Event.Transitioning event ->
        Transitioning.update (StepInput.init w state event)
        |> mapStepResultToGlobalResult GameMode.Transitioning model
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
        (writeToFile: string -> bool -> string -> WriteFileResult)
        (serializer: obj -> Result<string, string>)
        (terminate: unit -> unit)
        (action: RuntimeAction) : Event option =
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
    | ReadFile _ -> failwith "todo: not yet implemented"
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


let printDebug (result: GlobalResult) (event: Event) =
    do Console.ForegroundColor <- ConsoleColor.Yellow
    do Console.Write($"{result.Model.GameMode.Head.GetType().Name}")
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Green
    do Console.Write(event)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Blue
    do Console.Write(result.Runtime)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Magenta
    do Console.WriteLine(result.Transition)
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
    
    let rec asFunction (renderable: RenderAction) =
        match renderable with
        | RenderAction.Nothing -> [fun _ -> ()]
        | Clear -> [clear]
        | Text text -> [text |> display]
        | Fallback s -> 
            let asDisplayable = { Text.DisplayableText.Text = s; Text.DisplayableText.NarrativeStyle = NarrativeStyle.Regular }
            [ asDisplayable |> render ]
        | Batch batch -> batch |> List.collect asFunction

    do action |> asFunction |> List.iter (fun x -> x ())


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
    | StartTransition transitionParameters ->
        let state, action, render = transitionParameters |> Transitioning.activate
        { model with GameMode = (state |> GameMode.Transitioning) :: model.GameMode },
        action,
        render


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
                    let result = update currentModel event
                    
                    do printDebug result event
                    
                    do result.Render |> render model.TextResources model.Language

                    do
                       // Since the SaveGame action requires the current `model` it can only be truly constructed in
                       // the runtime. 
                       (match result.Runtime with
                        | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, result.Model)
                        | other -> other)
                        |> runAction
                        |> Option.iter inbox.Post
                    
                    if not result.Transition.IsNothing then pendingTransition <- Some result.Transition
                    
                    if cts.Token.IsCancellationRequested then
                        do printfn "Shutting down"
                        return ()
                    else
                        return! loop result.Model
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
    