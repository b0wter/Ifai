namespace Ifai.Lib

open System

type RoomId = private RoomId of Guid
module RoomId =
    let create g = g |> RoomId 
    let value (RoomId r) = r


type RoomEvent =
    | Batch of RoomEvent list


module RoomEvent =
    let create e = Batch [e]


type Room = {
    Id: RoomId
    Name: Text
    Description: Text
    Connections: Map<Directions.Exit, RoomId>
    OnEnter: RoomEvent option
    OnExit: RoomEvent option
}


module Room =
    let create id name desc = { Id = id; Name = name; Description = desc; Connections = Map.empty; OnEnter = None; OnExit = None }
    
    let rename (r: Room) newName = { r with Name = newName }