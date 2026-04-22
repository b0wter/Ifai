module Ifai.Runtime.RuntimeMappers

open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Runtime


let constructGameSnapshot (model: Model) : GameSnapshot =
    let asDisplayableL (t: Text.LocalizedText) =
        match t |> Text.mergeParameters None with
        | Ok d -> d
        | Error e -> e

    let asDisplayable t =
        match Text.localize model.TextResources model.Language t |> Text.mergeParameters None with
        | Ok d -> d
        | Error e -> e

    let constructConnectionSnapshot (c: Connection<RoomId>) : ConnectionSnapshot =
        let name = c.Exit |> Exit.asText |> asDisplayable
        let visibility = c.Visibility
        { ConnectionSnapshot.Visibility = visibility
          ConnectionSnapshot.Description = c.Description |> Option.map asDisplayable
          ConnectionSnapshot.Name = name }

    let constructRoomSnapshot (room: Room) : RoomSnapshot =
        { RoomSnapshot.Name = room.Name |> asDisplayableL
          RoomSnapshot.Description = room.Description |> asDisplayableL
          RoomSnapshot.Exits = Array.empty
          RoomSnapshot.Things = Array.empty
          RoomSnapshot.Characters = Array.empty }

    let room = model.World |> World.currentRoom |> constructRoomSnapshot
    let player = { PlayerSnapshot.Inventory = Array.empty }
    { GameSnapshot.Player = player; GameSnapshot.Room = room }


let rec render (resources: TextResources) (language: Language) (action: RenderAction) : EngineMessage option =
    let clear = EngineMessage.ClearScreen

    let render (text: Text.DisplayableText) =
        EngineMessage.NewHistoryItem (text.Text, text.NarrativeStyle)

    let localizedTextFormatter (text: Text.LocalizedText) =
        text
        |> Text.mergeParameters None
        |> function Ok t -> t | Error t -> t

    let textFormatter (text: Text) =
        text
        |> Text.localize resources language
        |> Text.mergeParameters None
        |> function Ok t -> t | Error t -> t

    let rec asFunction (renderable: RenderAction) : EngineMessage list =
        match renderable with
        | RenderAction.Nothing -> []
        | RenderAction.Clear -> [clear]
        | RenderAction.Text text -> [text |> textFormatter |> render]
        | RenderAction.LocalizedText text -> [text |> localizedTextFormatter |> render]
        | RenderAction.Fallback s ->
            let asDisplayable = { Text.DisplayableText.Text = s; Text.DisplayableText.NarrativeStyle = NarrativeStyle.Regular }
            [ asDisplayable |> render ]
        | RenderAction.Batch batch -> batch |> List.collect asFunction

    match action |> asFunction with
    | [] -> None
    | [single] -> Some single
    | many -> many |> Array.ofList |> EngineMessage.Batch |> Some
