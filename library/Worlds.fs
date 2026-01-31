module Ifai.Library.Worlds

type World = {
    Turn: uint
    CurrentRoomId: RoomIds.RoomId 
    Rooms: Map<RoomIds.RoomId, Room>
    /// <summary>
    /// List of `currentMode :: previousModes`, stack-like representation so that one can return to a previous state
    /// </summary>
    Mode: GameModes.GameMode list
}


/// <summary>
/// Updates the world so that the previous game mode is now the current game mode.
/// If there is no mode to fall back to the fallback value will be used
/// </summary>
let returnToPreviousState (w: World) (fallback: GameModes.GameMode) : World =
    let updatedMode =
        match w.Mode with
        | [] -> [ fallback ]
        | head :: tail when tail.IsEmpty -> [ fallback ]
        | _ :: tail -> tail
    { w with Mode = updatedMode }
    
    
/// <summary>
/// Updates the world so that the previous game mode is now the current game mode.
/// </summary>
/// <returns>
/// `Some World` if there is a game mode to fall back to `None` otherwise
/// </returns>
let tryReturnToPreviousState (w: World) : World option =
    match w.Mode with
    | [] -> None
    | _ :: tail when tail.IsEmpty -> None
    | _ :: tail -> Some { w with Mode = tail }

