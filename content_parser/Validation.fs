module Ifai.ContentParser.Validation

open System
open Ifai.ContentParser.IndentationMapper
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Lib.Shared
open FsToolkit.ErrorHandling


let parametersRegex = @"\{(?:[^:{}]+:)?[^{}]+\}"


let validateRoom (room: Room, modifiers: RoomModifier list) : Result<unit, string list> =
    Ok ()


let validateThing (thing: Thing, modifiers: ThingModifier list, location: ThingLocation) : Result<unit, string list> =
    let nameAndDescription = thing.Description.Text + thing.Name.Text
    let parameters =
        System.Text.RegularExpressions.Regex.Matches(nameAndDescription, parametersRegex)
        |> Seq.map _.Value
        |> List.ofSeq

    let nonInitializedModifiers =
        parameters
        |> List.filter (fun parameter ->
            modifiers
            |> List.exists (function ThingModifier.Custom (name, _) -> not <| String.Equals(name |> AttributeId.value, parameter, StringComparison.InvariantCultureIgnoreCase) | _ -> true))
    
    if nonInitializedModifiers.IsEmpty then Ok ()
    else Error ["Missing"]


let validateIdUniqueness (content: MappedContent) : Result<unit, string list> =
    let roomIds = content.Rooms |> List.map (fst >> _.Id)
    let thingIds = content.Things |> List.map (fun (t, _, _) -> t.Id)
        
    let allIds = (roomIds |> List.map RoomId.value) @ (thingIds |> List.map ThingId.value)
    let nonUniqueIds = 
        allIds 
        |> List.groupBy id 
        |> List.filter (snd >> List.length >> (<) 1)
        |> List.map fst
        
    if nonUniqueIds.IsEmpty then Ok ()
    else Error (nonUniqueIds |> List.map (fun i -> $"Duplicate id: %s{i}"))


let validate (content: MappedContent) : Result<unit, string list> =
    result {
        let! _ = content |> validateIdUniqueness
        let! _ = content.Rooms |> List.map validateRoom |> List.sequenceResultM
        let! _ = content.Things |> List.map validateThing |> List.sequenceResultA |> Result.mapError List.concat
        return ()
    }