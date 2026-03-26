module Ifai.Dummies.Rooms

open Ifai.Lib
open Ifai.Lib.Content

let dummyRoomIds = [|
    "dummy.1" |> RoomId.create
    "dummy.2" |> RoomId.create
    "dummy.3" |> RoomId.create
|]

let private createConnections<'a> (connections: Connection<'a> list) : Map<Exit, Connection<'a>> =
    connections
    |> List.map (fun c -> c.Exit, c)
    |> Map.ofList


let dummyRooms language = [
    {
        Id = dummyRoomIds[0]
        Environment = RoomEnvironment.``default``
        OnEnter = RoomEvent.Nothing
        OnLeaving = RoomEvent.Nothing
        Name = (Texts.textsMap |> Map.find (TextKey.create "dummy1_name")) |> (Text.localize Texts.textResources language)
        Description = (Texts.textsMap |> Map.find (TextKey.create "dummy1_description") |> (Text.localize Texts.textResources language))
        Connections = [Connection.create (Exit.Dir Direction.East) (dummyRoomIds[1]); Connection.create (Exit.Dir Direction.West) (dummyRoomIds[2])] |> createConnections
    }
    { Id = dummyRoomIds[1]
      Environment = RoomEnvironment.``default``
      OnEnter = RoomEvent.Nothing
      OnLeaving = RoomEvent.Nothing
      Name = (Texts.textsMap |> Map.find (TextKey.create "dummy2_name") |> (Text.localize Texts.textResources language))
      Description = (Texts.textsMap |> Map.find (TextKey.create "dummy2_description") |> (Text.localize Texts.textResources language))
      Connections = [Connection.create (Exit.Dir Direction.West) (dummyRoomIds[0])] |> createConnections }
    { Id = dummyRoomIds[2]
      Environment = RoomEnvironment.``default``
      OnEnter = RoomEvent.Nothing
      OnLeaving = RoomEvent.Nothing
      Name = (Texts.textsMap |> Map.find (TextKey.create "dummy3_name")) |> (Text.localize Texts.textResources language)
      Description = (Texts.textsMap |> Map.find (TextKey.create "dummy3_description")) |> (Text.localize Texts.textResources language)
      Connections = [Connection.create (Exit.Dir Direction.East) (dummyRoomIds[0])] |> createConnections }
]