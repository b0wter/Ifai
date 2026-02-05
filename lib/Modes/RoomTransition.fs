module Ifai.Lib.Modes.RoomTransition

(*
    This game mode describes the complete transition from one room to another.
    The entering and leaving is done in a single game mode for these reasons:
        1. transitioning becomes an atomic operation
        2. it works well with the stack implementation of the games modes:
            RoomTransition is pushed onto the stack, it modifies the world and tells the runtime
            it has finished. The runtime can them simply remove it from the stack and exploring
            continues with the new world
*)



