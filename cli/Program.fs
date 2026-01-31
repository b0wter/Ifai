open System
open System.Threading

open Ifai.Lib


let germanTexts =
    [
        "dummy1_name", "Dummy-Raum #1"
        "dummy1_description", "Dies ist ein Dummy-Raum in dem man einfach gar nichts machen kann."
        "dummy2_name", "Dummy-Raum #2"
        "dummy2_description", "Auch dies ist ein Dummy-Raum in dem man schon wieder nichts machen kann."
        "dummy3_name", "Dummy-Raum #3"
        "dummy3_description", "Überraschend ist in diesem Raum absolut nichts zu tun oder zu erkunden."
    ] |> Map.ofList
    
    
let englishTexts =
    [
        "dummy1_name", "Dummy room #1"
        "dummy1_description", "This is a dummy room in which there is nothing to do."
        "dummy2_name", "Dummy room #2"
        "dummy2_description", "This too, is a room where there is nothing to do."
        "dummy3_name", "Dummy room #3"
        "dummy3_description", "Surprisingly, there is absolutely nothing to do or explore in this room."
    ] |> Map.ofList


let texts =
    [
        {
            ResourceKey = "dummy1_name" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy1_description" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy2_name" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy2_description" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy3_name" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy3_description" |> TextKey.create
            Intent = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
    ]


let textsMap =
    texts
    |> List.map (fun text -> text.ResourceKey, text)
    |> Map.ofList


let dummyRoomIds = [|
    Guid.NewGuid() |> RoomId.create
    Guid.NewGuid() |> RoomId.create
    Guid.NewGuid() |> RoomId.create
|]


let dummyRooms = [
    { Id = dummyRoomIds[0]; OnEnter = None; OnExit = None; Name = textsMap |> Map.find (TextKey.create "dummy1_name"); Description = textsMap |> Map.find (TextKey.create "dummy1_description"); Connections = [(Directions.Exit.Dir Directions.East, dummyRoomIds[1]); (Directions.Exit.Dir Directions.West, dummyRoomIds[2])] |> Map.ofList }
    { Id = dummyRoomIds[1]; OnEnter = None; OnExit = None; Name = textsMap |> Map.find (TextKey.create "dummy2_name"); Description = textsMap |> Map.find (TextKey.create "dummy2_description"); Connections = [(Directions.Exit.Dir Directions.West, dummyRoomIds[0])] |> Map.ofList  }
    { Id = dummyRoomIds[2]; OnEnter = None; OnExit = None; Name = textsMap |> Map.find (TextKey.create "dummy3_name"); Description = textsMap |> Map.find (TextKey.create "dummy3_description"); Connections = [(Directions.Exit.Dir Directions.East, dummyRoomIds[0])] |> Map.ofList } ] |> List.map (fun r -> r.Id, r) |> Map.ofList


let world = World.init (dummyRooms |> Map.values |> List.ofSeq) dummyRoomIds[0]
let state = { Exploring.ExploringState.Foo = 0 } |> Loop.GameMode.Exploring
let model = { Loop.World = world; Loop.GameMode = [state]; Loop.Language = Language.create "en" }

Loop.run model