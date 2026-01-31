module Ifai.Lib.EnteringRoom

(*
    This is a phase that is set once the player newly enters a room.
    It allows handling of some events and will eventually end in the
    Exploring-state (most of the time)
*)

type EnteringRoomMsg =
    | Entered
    | HandleEvent of RoomEvent
    | Finished


type EnteringRoomCmd =
    | Move of RoomId
    | Nothing


type EnteringRoomState = {
    Foo: int
}


let update (world: World) (state: EnteringRoomState) (msg:EnteringRoomMsg) : (World * EnteringRoomState * EnteringRoomCmd) =
    failwith "EnteringRoom not implemented"