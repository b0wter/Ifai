module Ifai.Lib.LeavingRoom

open Ifai.Lib


type LeavingRoomMsg =
    | Leaving
    | HandleEvent of RoomEvent
    | Finished


type LeavingRoomCmd =
    | Nothing


type LeavingRoomState = {
    /// Room to go to after leaving this room has been handled
    TargetRoom: RoomId
}


let update (world: World) (state: LeavingRoomState) (msg: LeavingRoomMsg) : (World * LeavingRoomState * LeavingRoomCmd) =
    failwith "LeavingRoom update not implemented"