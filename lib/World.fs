namespace Ifai.Lib

type World = {
    Turn: uint
    Rooms: Map<RoomId, Room>
    CurrentRoomId: RoomId
}

module World =
    let init (rooms: Room list) (initialRoom: RoomId) =
        let roomMap =
            rooms
            |> List.map (fun r -> r.Id, r)
            |> Map.ofList
        { Turn = 0u; Rooms = roomMap; CurrentRoomId = initialRoom }

    
    /// Gets the id of the room that is connected to the current room via the given exit
    let getRoomIdForExit (exit: Directions.Exit) (world: World) : RoomId option =
        world.Rooms
        |> Map.tryFind world.CurrentRoomId
        |> Option.bind (fun r -> r.Connections |> Map.tryFind exit)


    let tryMoveTo (increaseTurnCounter: bool) (exit: Directions.Exit) (world: World) : bool * World =
        let maybeNextRoom = 
            world.Rooms
            |> Map.tryFind world.CurrentRoomId
            |> Option.map _.Connections
            |> Option.bind (Map.tryFind exit)
            |> Option.bind (fun roomId -> world.Rooms |> Map.tryFind roomId)

        let turnIncrement = if increaseTurnCounter then 1u else 0u

        match maybeNextRoom with
        | Some room -> true, { world with CurrentRoomId = room.Id; Turn = world.Turn + turnIncrement }
        | None -> false, world


    let doesRoomIdExist (roomId: RoomId) (world: World) = world.Rooms |> Map.containsKey roomId
    
    let doesRoomExist (room: Room) (world: World) = doesRoomIdExist room.Id world
    
    let doesExitExist (exit: Directions.Exit) (roomId: RoomId) (world: World) =
        world.Rooms
        |> Map.tryFind roomId
        |> Option.map (fun r -> r.Connections |> Map.containsKey exit)
        |> Option.defaultValue false

    let currentRoom (w: World) =
        w.Rooms |> Map.find w.CurrentRoomId

    let setCurrentRoom (r: Room) (w: World) : World =
        { w with CurrentRoomId = r.Id }

    let setCurrentRoomId (rId: RoomId) (w: World) : World =
        { w with CurrentRoomId = rId }