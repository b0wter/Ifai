module Ifai.Runtime.Interop.Mappers

open Ifai.Lib
open Ifai.Lib.Shared
open Ifai.Runtime
open Ifai.Runtime.Interop.Types


let mapVisibility (v: Visibility) : VisibilityInfo =
    match v with
    | Visible -> VisibilityInfo.Visible
    | Hidden -> VisibilityInfo.Hidden
    | Concealed -> VisibilityInfo.Concealed


let mapNarrativeStyle (s: NarrativeStyle) : NarrativeStyleInfo =
    match s with
    | Regular -> NarrativeStyleInfo.Regular
    | Emphasized -> NarrativeStyleInfo.Emphasized
    | Hint -> NarrativeStyleInfo.Hint
    | Dialogue -> NarrativeStyleInfo.Dialogue
    | NarrativeStyle.System -> NarrativeStyleInfo.System


let mapThingSnapshot (t: ThingSnapshot) : ThingInfo =
    { ThingInfo.Name = t.Name
      ThingInfo.Description = t.Description
      ThingInfo.Visibility = t.Visibility |> mapVisibility }


let mapCharacterSnapshot (c: CharacterSnapshot) : CharacterInfo =
    { CharacterInfo.Name = c.Name
      CharacterInfo.Description = c.Description
      CharacterInfo.Visibility = c.Visibility |> mapVisibility }


let mapConnectionSnapshot (c: ConnectionSnapshot) : ConnectionInfo =
    { ConnectionInfo.Name = c.Name
      ConnectionInfo.Description = c.Description
      ConnectionInfo.Visibility = c.Visibility |> mapVisibility }


let mapRoomSnapshot (r: RoomSnapshot) : RoomInfo =
    { RoomInfo.Name = r.Name
      RoomInfo.Description = r.Description
      RoomInfo.Exits = r.Exits |> Array.map mapConnectionSnapshot
      RoomInfo.Things = r.Things |> Array.map mapThingSnapshot
      RoomInfo.Characters = r.Characters |> Array.map mapCharacterSnapshot }


let mapPlayerSnapshot (p: PlayerSnapshot) : PlayerInfo =
    { PlayerInfo.Inventory = p.Inventory |> Array.map mapThingSnapshot }


let mapGameSnapshot (g: GameSnapshot) : GameStateInfo =
    { GameStateInfo.Room = g.Room |> mapRoomSnapshot
      GameStateInfo.Player = g.Player |> mapPlayerSnapshot }


let rec mapEngineMessage (msg: Ifai.Runtime.EngineMessage) : EngineMessageInfo =
    match msg with
    | Ifai.Runtime.EngineMessage.UpdatedGameState snapshot ->
        UpdatedGameStateMessage(snapshot |> mapGameSnapshot) :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.NewHistoryItem (text, style) ->
        NewHistoryItemMessage(text, style |> mapNarrativeStyle) :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.ClearScreen ->
        ClearScreenMessage() :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.DebugOutputResult (result, event) ->
        DebugOutputResultMessage(result, event) :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.DebugOutputMessage message ->
        DebugOutputMessageMessage(message) :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.RequestQuit ->
        RequestQuitMessage() :> EngineMessageInfo
    | Ifai.Runtime.EngineMessage.Batch messages ->
        let infos = messages |> Array.map mapEngineMessage
        BatchMessage(infos) :> EngineMessageInfo


let wrapEngine (runtimeEngine: RuntimeEngine) : Engine =
    let engineMessageEvent = Event<EngineMessageInfo>()

    let _subscription =
        runtimeEngine.Output
        |> Observable.subscribe (fun msg ->
            msg |> mapEngineMessage |> engineMessageEvent.Trigger)

    { Engine.Input = runtimeEngine.Input
      Engine.Output = engineMessageEvent.Publish
      Engine.CancellationTokenSource = runtimeEngine.CancellationTokenSource }
