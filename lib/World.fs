namespace Ifai.Lib

type World = {
    Turn: uint
    Rooms: Map<RoomId, Room>
    RoomModifiers: Map<RoomId, Set<RoomModifiers>>
    CurrentRoomId: RoomId
    Items: Map<ItemId, Item>
}

module World =
    let init (rooms: Room list) (initialRoom: RoomId) (items: Item list) =
        let roomMap =
            rooms
            |> List.map (fun r -> r.Id, r)
            |> Map.ofList
        let itemMap =
            items
            |> List.map (fun i -> i.Id, i)
            |> Map.ofList
        { Turn = 0u; Rooms = roomMap; RoomModifiers = Map.empty; CurrentRoomId = initialRoom; Items = itemMap }

    
    /// Gets the id of the room that is connected to the current room via the given exit
    let getConnectionForExit (exit: Exit) (world: World) : Connection<RoomId> option =
        world.Rooms
        |> Map.tryFind world.CurrentRoomId
        |> Option.bind (fun r -> r.Connections |> Map.tryFind exit)


    let getRoomById (id: RoomId) (world: World) : Room option =
        world.Rooms
        |> Map.tryFind id

    
    let getExitsByRoomId (roomId: RoomId) (world: World) : Exit list option =
        world.Rooms
        |> Map.tryFind roomId
        |> Option.map (fun r -> r.Connections |> Map.toList |> List.map fst)


    let tryMoveTo (increaseTurnCounter: bool) (exit: Exit) (world: World) : bool * World =
        let maybeNextRoom = 
            world.Rooms
            |> Map.tryFind world.CurrentRoomId
            |> Option.map _.Connections
            |> Option.bind (Map.tryFind exit)
            |> Option.bind (fun connection -> world.Rooms |> Map.tryFind connection.ToId)

        let turnIncrement = if increaseTurnCounter then 1u else 0u

        match maybeNextRoom with
        | Some room -> true, { world with CurrentRoomId = room.Id; Turn = world.Turn + turnIncrement }
        | None -> false, world


    let doesRoomIdExist (roomId: RoomId) (world: World) = world.Rooms |> Map.containsKey roomId
    
    let doesRoomExist (room: Room) (world: World) = doesRoomIdExist room.Id world
    
    let doesExitExist (exit: Exit) (roomId: RoomId) (world: World) =
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