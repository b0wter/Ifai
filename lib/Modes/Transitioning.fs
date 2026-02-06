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


type TransitioningState = {
    CurrentPhase: TransitioningPhase
    FromRoomId: RoomId
    ToRoomId: RoomId
    HasHandledLeavingEvent: bool
    HasHandledEnteringEvent: bool
}


type TransitioningEvent =
    | Nothing
    | UserInput of Input


let activate (parameters: ToTransitioningParameters) : TransitioningState * RuntimeAction * RenderAction =
    {
        CurrentPhase = Leaving
        FromRoomId = parameters.FromRoomId
        ToRoomId = parameters.ToRoomId
        HasHandledEnteringEvent = false
        HasHandledLeavingEvent = false
    },
    RuntimeAction.Nothing,
    RenderAction.Nothing
    

let update (stepInput: StepInput<TransitioningState, TransitioningEvent>) : StepResult<TransitioningState> =
    match stepInput.Event with
    | Nothing -> failwith "todo"
    | UserInput _ ->
        StepResult.init stepInput.World stepInput.State
        |> StepResult.withRender (RenderAction.Fallback "UserInput is not supported in Transitioning mode")