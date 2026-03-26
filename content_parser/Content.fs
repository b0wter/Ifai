module Ifai.ContentParser.Content

open FsToolkit.ErrorHandling
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.ContentParser.IndentationMapper

let parseFile (file: string) : Result<MappedContent, string> =
    if System.IO.File.Exists file then
        try
            Ok (System.IO.File.ReadAllText file)
            |> Result.map IndentationParser.parse
            |> Result.map mapFullContent
        with e -> Error $"Error parsing '{file}': {e.Message}"
    else Error $"File not found: {file}"


let ifaiFileExtension = ".ifa"


let parseFolder (folder: string) : Result<MappedContent list, string> =
    if System.IO.Directory.Exists folder then
        System.IO.Directory.GetFiles(folder, "*" + ifaiFileExtension)
        |> Array.toList
        |> List.map parseFile
        |> List.fold (fun acc next ->
            match acc, next with
            | Ok results, Ok content -> Ok (content :: results)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        ) (Ok [])
    else Error $"Folder not found: {folder}"


let createWorld (contents: MappedContent list) : World =
    let adventures = contents |> List.choose _.Adventure
    if adventures.IsEmpty then failwith "Cannot create world without an adventure root"
    else if adventures.Length > 1 then failwith "Cannot create world with multiple adventure roots"
    else
        let adventure = adventures |> List.exactlyOne
        let allRooms = contents |> List.collect _.Rooms
        let allThings = contents |> List.collect _.Things
        
        let rooms = allRooms |> List.map fst
        let things = allThings |> List.map (fun (t, _, _) -> t)
        let itemLocations = 
            allThings 
            |> List.map (fun (t, _, l) -> t.Id, l)
            |> Map.ofList
        
        let world = World.init rooms adventure.InitialRoom things itemLocations
        
        let roomModifiers = 
            allRooms 
            |> List.map (fun (r, m) -> r.Id, Set.ofList m)
            |> Map.ofList
            
        let thingModifiers =
            allThings
            |> List.map (fun (t, m, _) -> t.Id, Set.ofList m)
            |> Map.ofList
            
        { world with 
            RoomModifiers = roomModifiers
            ItemModifiers = thingModifiers }


let createWorldFromFolder (folder: string) : Result<World, string> =
    result {
        let! mappedContent = folder |> parseFolder
        return createWorld mappedContent
    }