module Ifai.Lib.Runtime

open Ifai.Lib.Modes
open Ifai.Lib.Content


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
    let newGameMode = (r.State |> modeMap) :: (if model.GameMode.IsEmpty then [] else model.GameMode.Tail)
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
                if model.GameMode.IsEmpty then failwith "Cannot parse input while in empty mode"
                else
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


/// <summary>
/// Pushes of pops a new game mode onto the game mode stack for the given model
/// </summary>
/// <returns>
/// Returns `Some` if the model was changed and `None` otherwise
/// </returns>
let runTransition (transition: ModeTransition) (model: Model) : Model * RuntimeAction<Event> * RenderAction =
    match transition with
    | ModeTransition.Nothing ->
        model,
        RuntimeAction.Nothing,
        RenderAction.Nothing
    | Finished ->
        { model with GameMode = if model.GameMode.IsEmpty then [] else model.GameMode.Tail },
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
