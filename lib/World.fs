namespace Ifai.Lib

type World = {
    Turn: uint
    Rooms: Map<RoomId, Room>
    CurrentRoom: RoomId
}

module World =
    let init (rooms: Room list) (initialRoom: RoomId) =
        let roomMap =
            rooms
            |> List.map (fun r -> r.Id, r)
            |> Map.ofList
        { Turn = 0u; Rooms = roomMap; CurrentRoom = initialRoom }
