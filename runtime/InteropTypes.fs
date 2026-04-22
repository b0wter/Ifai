namespace Ifai.Runtime.Interop.Types

open System
open Ifai.Lib


type VisibilityInfo =
    | Visible = 0
    | Hidden = 1
    | Concealed = 2


type NarrativeStyleInfo =
    | Regular = 0
    | Emphasized = 1
    | Hint = 2
    | Dialogue = 3
    | System = 4


type ThingInfo =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Visibility: VisibilityInfo }


type CharacterInfo =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Visibility: VisibilityInfo }


type ConnectionInfo =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText option
      Visibility: VisibilityInfo }


type RoomInfo =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Exits: ConnectionInfo[]
      Things: ThingInfo[]
      Characters: CharacterInfo[] }


type PlayerInfo =
    { Inventory: ThingInfo[] }


type GameStateInfo =
    { Room: RoomInfo
      Player: PlayerInfo }


[<AbstractClass>]
type EngineMessageInfo() =
    class end

and UpdatedGameStateMessage(gameState: GameStateInfo) =
    inherit EngineMessageInfo()
    member _.GameState = gameState

and NewHistoryItemMessage(text: string, style: NarrativeStyleInfo) =
    inherit EngineMessageInfo()
    member _.Text = text
    member _.Style = style

and ClearScreenMessage() =
    inherit EngineMessageInfo()

and DebugOutputResultMessage(result: GlobalResult, event: Ifai.Lib.Event) =
    inherit EngineMessageInfo()
    member _.Result = result
    member _.Event = event

and DebugOutputMessageMessage(message: string) =
    inherit EngineMessageInfo()
    member _.Message = message

and RequestQuitMessage() =
    inherit EngineMessageInfo()

and BatchMessage(messages: EngineMessageInfo[]) =
    inherit EngineMessageInfo()
    member _.Messages = messages


type Engine = {
    Input: Ifai.Runtime.IEngineInput
    Output: IObservable<EngineMessageInfo>
    CancellationTokenSource: System.Threading.CancellationTokenSource
}
