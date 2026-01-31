module Ifai.Library.Exploring

(*
    
*)


type MovementDirection =
    | Up
    | Down
    | North
    | NorthEast
    | East
    | SouthEast 
    | South
    | SouthWest 
    | West
    | NorthWest

    
type ExplorationIntent =
    | Directional of MovementDirection
    | Prompt of string
    | Unknown


type GameMsg =
    | Wait
    | Move of MovementDirection


type SystemMsg =
    | RequestExit
    | SaveWorld of SaveWorldMsg
    | LoadGame
    | ChangeGameMode of GameMode


let updateViaGameMessage (message: GameMsg) (world: Worlds.World) : Worlds.World * Cmd<Msg> =
    match message with
    | Wait ->
        { world with Turn = world.Turn + 1u }, Effect (Print "You wait for five minutes")
    | Move direction ->
        let currentRoom = 
            world.Rooms
            |> Map.find world.CurrentRoomId
        let maybeNextRoom =
            currentRoom.Connections
            |> Map.tryFind direction
                   
        match maybeNextRoom with 
        | None -> world, Effect (Print "You cannot go that way")
        | Some roomId ->
            let nextRoom = world.Rooms |> Map.find roomId 
            { world with CurrentRoomId = roomId }, Effect (Print $"Entered: %s{nextRoom.Name}")


let updateViaSaveWorldMessage (message: SaveWorldMsg) (world: World) : World * Cmd<Msg> =
    match message with
    | SaveWorldMsg.SaveGame filename ->
        world, Effect (SaveGame filename)
    | SavedSuccessfully -> { world with Mode = Exploring }, Cmd.Nothing
    | SaveFailed error -> { world with Mode = Exploring }, Effect (Print $"Could not save the world because: %s{error}")
    | ConfirmOverwrite filename -> { world with Mode = AwaitingOverrideSaveFile filename }, Effect (Print $"Do you want to override the file: %s{filename}")


let updateViaSystemMessage (message: SystemMsg) (world: World) : World * Cmd<Msg> =
    match message with
    | RequestExit -> world, Effect ExitGame
    | SystemMsg.SaveWorld msg -> world |> updateViaSaveWorldMessage msg
    | LoadGame -> failwith "not implemented"
    | ChangeGameMode gameMode -> { world with Mode = gameMode }, Cmd.Nothing
 
 
 