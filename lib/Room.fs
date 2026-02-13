namespace Ifai.Lib

type RoomId = private RoomId of string
module RoomId =
    let create g = g |> RoomId 
    let value (RoomId r) = r


type RoomEvent =
    | Nothing
    | Batch of RoomEvent list


module RoomEvent =
    let create e = Batch [e]


type Room = {
    Id: RoomId
    Name: Text
    Description: Text
    Connections: Map<Exit, RoomId>
    OnEnter: RoomEvent option
    OnLeaving: RoomEvent option
}


module Room =
    let create id name desc =
        { Id = id
          Name = name
          Description = desc
          Connections = Map.empty
          OnEnter = None
          OnLeaving = None }
    
    let rename (r: Room) newName = { r with Name = newName }
    
    let name (r: Room) = r.Name
    let description (r: Room) = r.Description
    let onEnter r = r.OnEnter
    let onLeaving r = r.OnLeaving

    let listExits r =
        r.Connections
        |> Map.toList
