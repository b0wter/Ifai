module Ifai.Lib.Modes.EnteringRoom

open Ifai.Lib

(*
    This is a phase that is set once the player newly enters a room.
    It allows handling of some events and will eventually end in the
    Exploring-state (most of the time)
*)

type Input = Nothing


type EnteringRoomEvent =
    | HandleEvent of RoomEvent
    | Finished
    | UserInput of Input


type EnteringRoomState = {
    FromRoomId: RoomId
    ToRoomId: RoomId
}


let init (world: World) (parameters: ToEnteringRoomParameters) : World * EnteringRoomState * RuntimeAction * RenderAction =
    let state =
        {
            FromRoomId = parameters.FromRoomId
            ToRoomId = parameters.ToRoomId
        }
    let updatedWorld = world |> World.setCurrentRoomId parameters.ToRoomId
    let currentRoom = updatedWorld |> World.currentRoom
    let renderAction =
        RenderAction.RenderMany
            [ currentRoom.Name
              currentRoom.Description ]
    updatedWorld,
    state,
    RuntimeAction.Nothing,
    renderAction


let parser _ _ : Input =
    Input.Nothing


let update (world: World) (state: EnteringRoomState) (event: EnteringRoomEvent) : World * EnteringRoomState * RuntimeAction * RenderAction * ModeTransition =
    world,
    state,
    RuntimeAction.Nothing,
    RenderAction.Nothing,
    ModeTransition.Nothing
    