module Ifai.Lib.Modes.Transitioning

open Ifai.Lib

(*
    This game mode describes the complete transition from one room to another.
    The entering and leaving is done in a single game mode for these reasons:
        1. transitioning becomes an atomic operation
        2. it works well with the stack implementation of the games modes:
            RoomTransition is pushed onto the stack, it modifies the world and tells the runtime
            it has finished. The runtime can them simply remove it from the stack and exploring
            continues with the new world
*)

type Input = Nothing


let parser (_: Language) (_: string) : Input =
    Input.Nothing


type TransitioningPhase =
    // Leaving the current room comes first,
    // entering the new room second.
    | Leaving
    | Entering
    | Finished
    
    
type TransitioningResult =
    | Success
    | Failed of string
    | Forbidden of Text


type TransitioningState = {
    LeavingMode: LeavingRoomMode
    EnteringMode: EnteringRoomMode
    CurrentPhase: TransitioningPhase
    FromRoomId: RoomId
    ToRoomId: RoomId
}


type TransitioningEvent =
    | Nothing
    | AdvanceTransition
    | UserInput of Input


let activate (parameters: ToTransitioningParameters) : ActivationResult<TransitioningState, TransitioningEvent> =
    let phase =
        match parameters.LeavingMode, parameters.EnteringMode with
        | LeavingRoomMode.Skip, SkipAll -> TransitioningPhase.Finished
        | LeavingRoomMode.Skip, EventOnly
        | LeavingRoomMode.Skip, EventOnly -> TransitioningPhase.Entering
        | LeavingRoomMode.Skip, Full
        | LeavingRoomMode.Skip, EventOnly -> TransitioningPhase.Entering
        | LeavingRoomMode.Skip, SkipEvent -> TransitioningPhase.Entering
        | LeavingRoomMode.Full, _ -> TransitioningPhase.Leaving

    ActivationResult.init
        {
            EnteringMode = parameters.EnteringMode
            LeavingMode = parameters.LeavingMode
            CurrentPhase = phase
            FromRoomId = parameters.FromRoomId
            ToRoomId = parameters.ToRoomId
        }
    |> ActivationResult.withRuntime (RuntimeAction.OfEvent TransitioningEvent.AdvanceTransition)
    

let handleLeavingEvent (world: World) (state: TransitioningState) : StepResult<TransitioningState, TransitioningEvent> =
    match state.LeavingMode with
    | LeavingRoomMode.Skip ->
        StepResult.init world { state with CurrentPhase = TransitioningPhase.Entering }
        |> StepResult.withRuntime (RuntimeAction.OfEvent AdvanceTransition)
    | LeavingRoomMode.Full ->
        let room = world |> World.currentRoom
        StepResult.init world { state with CurrentPhase = TransitioningPhase.Entering }
        |> StepResult.withRender (RenderAction.Batch [RenderAction.Fallback "[system] Handling leaving events is not yet supported"; RenderAction.Fallback "[system] You are leaving: "; RenderAction.Text room.Name])
        |> StepResult.withRuntime (RuntimeAction.OfEvent AdvanceTransition)


let handleEnteringEvent (world: World) (state: TransitioningState) : StepResult<TransitioningState, TransitioningEvent> =
    let world = { world with CurrentRoomId = state.ToRoomId }
    let room = world |> World.currentRoom

    match state.EnteringMode with
    | Full ->
        StepResult.init world { state with CurrentPhase = TransitioningPhase.Finished }
        |> StepResult.withRender (RenderAction.Batch [RenderAction.Fallback "[system] Handling entering events is not yet supported"; RenderAction.Text room.Name; RenderAction.Text room.Description])
    | EventOnly ->
        StepResult.init world { state with CurrentPhase = TransitioningPhase.Finished }
        |> StepResult.withRender (RenderAction.Batch [RenderAction.Fallback "[system] Handling entering events is not yet supported"])
    | SkipEvent ->
        StepResult.init world { state with CurrentPhase = TransitioningPhase.Finished }
        |> StepResult.withRender (RenderAction.Batch [RenderAction.Text room.Name; RenderAction.Text room.Description])
    | SkipAll ->
        StepResult.init world state


    StepResult.init world { state with CurrentPhase = TransitioningPhase.Finished }
    |> StepResult.withRender (RenderAction.Batch [RenderAction.Text room.Name; RenderAction.Text room.Description])
    |> StepResult.withTransition ModeTransition.Finished


let update (input: StepInput<TransitioningState, TransitioningEvent>) : StepResult<TransitioningState, TransitioningEvent> =
    match input.Event, input.State.CurrentPhase with
    | Nothing, _ ->
        StepResult.init input.World input.State

    | AdvanceTransition, TransitioningPhase.Leaving ->
        handleLeavingEvent input.World input.State
    | AdvanceTransition, TransitioningPhase.Entering ->
        handleEnteringEvent input.World input.State
    | AdvanceTransition, TransitioningPhase.Finished ->
        failwith "An AdvanceTransition-event was sent to the already finished transitioning state"

    | UserInput _, _ ->
        StepResult.init input.World input.State
        |> StepResult.withRender (RenderAction.Fallback "UserInput is not supported in Transitioning mode")