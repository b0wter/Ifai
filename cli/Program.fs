open System
open System.Text.Json.Serialization

open Ifai.Lib
open Ifai.Lib.Modes


let germanTexts =
    [
        TextKey.create "unknown_intent", "Entschuldigung, ich verstehe die Eingabe nicht"
        TextKey.create "dummy1_name", "Dummy-Raum #1"
        TextKey.create "dummy1_description", "Dies ist ein Dummy-Raum in dem man einfach gar nichts machen kann."
        TextKey.create "dummy2_name", "Dummy-Raum #2"
        TextKey.create "dummy2_description", "Auch dies ist ein Dummy-Raum in dem man schon wieder nichts machen kann."
        TextKey.create "dummy3_name", "Dummy-Raum #3"
        TextKey.create "dummy3_description", "Überraschend ist in diesem Raum absolut nichts zu tun oder zu erkunden."
        TextKey.create "no_exit_found", "In dieser Richtung gibt es keinen Ausgang."
        TextKey.create "leaving_current_Room", "Du verlässt den Ort"
        TextKey.create "directions_description", "Du kannst in folgende Richtungen gehen:"
        TextKey.create "north", "Norden"
        TextKey.create "northeast", "Nordosten"
        TextKey.create "east", "Osten"
        TextKey.create "southeast", "Südosten"
        TextKey.create "south", "Süden"
        TextKey.create "southwest", "Südwesten"
        TextKey.create "west", "Westen"
        TextKey.create "northwest", "Nordwesten"
        TextKey.create "up", "hoch"
        TextKey.create "down", "runter"
        TextKey.create "left", "links"
        TextKey.create "right", "rechts"
    ] |> Map.ofList
    
    
let englishTexts =
    [
        TextKey.create "unknown_intent", "Sorry, I do not understand the input"
        TextKey.create "dummy1_name", "Dummy room #1"
        TextKey.create "dummy1_description", "This is a dummy room in which there is nothing to do."
        TextKey.create "dummy2_name", "Dummy room #2"
        TextKey.create "dummy2_description", "This too, is a room where there is nothing to do."
        TextKey.create "dummy3_name", "Dummy room #3"
        TextKey.create "dummy3_description", "Surprisingly, there is absolutely nothing to do or explore in this room."
        TextKey.create "no_exit_found", "There is no exit in that direction."
        TextKey.create "leaving_current_room", "You are leaving"
        TextKey.create "directions_description", "You can go in the following directions:"
        TextKey.create "north", "north"
        TextKey.create "northeast", "north-east"
        TextKey.create "east", "east"
        TextKey.create "southeast", "south-east"
        TextKey.create "south", "south"
        TextKey.create "southwest", "south-west"
        TextKey.create "west", "west"
        TextKey.create "northwest", "north-west"
        TextKey.create "up", "up"
        TextKey.create "down", "down"
        TextKey.create "left", "left"
        TextKey.create "right", "right"
    ] |> Map.ofList
    

let textResources =
    [
        Language.create "ger", germanTexts
        Language.create "en", englishTexts
    ] |> Map.ofList


let texts =
    [
        {
            ResourceKey = "dummy1_name" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy1_description" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Regular
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy2_name" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy2_description" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Regular
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy3_name" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Emphasized
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "dummy3_description" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Regular
            Parameters = None
            ParameterFormatting = None
        }
        {
            ResourceKey = "no_exit_found" |> TextKey.create
            NarrativeStyle = Some NarrativeStyle.Regular
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
    { Id = dummyRoomIds[0]; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy1_name"); Description = textsMap |> Map.find (TextKey.create "dummy1_description"); Connections = [(Exit.Dir Direction.East, dummyRoomIds[1]); (Exit.Dir Direction.West, dummyRoomIds[2])] |> Map.ofList }
    { Id = dummyRoomIds[1]; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy2_name"); Description = textsMap |> Map.find (TextKey.create "dummy2_description"); Connections = [(Exit.Dir Direction.West, dummyRoomIds[0])] |> Map.ofList  }
    { Id = dummyRoomIds[2]; OnEnter = None; OnLeaving = None; Name = textsMap |> Map.find (TextKey.create "dummy3_name"); Description = textsMap |> Map.find (TextKey.create "dummy3_description"); Connections = [(Exit.Dir Direction.East, dummyRoomIds[0])] |> Map.ofList } ] |> List.map (fun r -> r.Id, r) |> Map.ofList


let world = World.init (dummyRooms |> Map.values |> List.ofSeq) dummyRoomIds[0] []
let state = { Exploring.ExploringState.Foo = 0 } |> GameMode.Exploring
let model = { Model.World = world; Model.GameMode = [state]; Model.Language = Language.create "en"; Model.TextResources = textResources }

let textWriter filename allowOverwrite (content: string) =
    try
        if (filename |> IO.File.Exists) && (not allowOverwrite) then filename |> WriteFileResult.AlreadyExists
        else
            do IO.File.WriteAllText(filename, content)
            WriteFileResult.Success
    with
    | exn -> WriteFileResult.Failure exn.Message


let options =
    JsonFSharpOptions
        .Default()
        .WithAllowNullFields()
        .ToJsonSerializerOptions()

let serializer object =
    try
        Text.Json.JsonSerializer.Serialize(object, options) |> Ok
    with
    | exn -> Error exn.Message


Runtime.run textWriter serializer model