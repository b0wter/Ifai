module Ifai.Lib.Modes.LeavingRoom

open Ifai.Lib

type Input = Nothing

type LeavingRoomEvent =
    | HandleEvent of RoomEvent
    | Finished
    /// UserInput is only defined to make this mode look like the other modes from the outside
    | UserInput of Input


type LeavingRoomState = {
    /// Room to go to after leaving this room has been handled
    FromRoomId: RoomId
    ToRoomId: RoomId
}


let init (parameters: ToLeavingModeParameters) =
    {
        FromRoomId = parameters.FromRoomId
        ToRoomId = parameters.ToRoomId
    }


let parser _ _ : Input =
    Input.Nothing


let update (world: World) (state: LeavingRoomState) (event: LeavingRoomEvent) : World * LeavingRoomState * RuntimeAction * RenderAction * ModeTransition =
    world,
    state,
    RuntimeAction.Nothing,
    RenderAction.Render { ResourceKey = TextKey.create "leaving_current_room"; NarrativeStyle = None; Parameters = None; ParameterFormatting = None},
    ModeTransition.StartEnteringRoom { FromRoomId = world.CurrentRoomId; ToRoomId = state.ToRoomId }