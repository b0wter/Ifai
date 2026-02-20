module Ifai.Resources.Rooms

open Ifai.Lib
open Ifai.Resources.Texts

let dummyRoomIds = [|
    "dummy.1" |> RoomId.create
    "dummy.2" |> RoomId.create
    "dummy.3" |> RoomId.create
|]

let private createConnections<'a> (connections: Connection<'a> list) : Map<Exit, Connection<'a>> =
    connections
    |> List.map (fun c -> c.Exit, c)
    |> Map.ofList


let dummyRooms = [
    { Id = dummyRoomIds[0]; Environment = RoomEnvironment.``default``; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy1_name"); Description = textsMap |> Map.find (TextKey.create "dummy1_description"); Connections = [Connection.create (Exit.Dir Direction.East) (dummyRoomIds[2])] |> createConnections }
    { Id = dummyRoomIds[1]; Environment = RoomEnvironment.``default``; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy2_name"); Description = textsMap |> Map.find (TextKey.create "dummy2_description"); Connections = [Connection.create (Exit.Dir Direction.West) (dummyRoomIds[0])] |> createConnections }
    { Id = dummyRoomIds[2]; Environment = RoomEnvironment.``default``; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy3_name"); Description = textsMap |> Map.find (TextKey.create "dummy3_description"); Connections = [Connection.create (Exit.Dir Direction.East) (dummyRoomIds[0])] |> createConnections }
]

