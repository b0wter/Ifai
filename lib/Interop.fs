namespace Ifai.Lib

open System

(*
 To make it interop better with C# we change all lists to arrays since F# lists are awkward to use in C#.
 F# records work fine for our use case as the clients simply display but do not modify the data.
*)

type VisibilityInfo =
    | Visible = 0
    | Hidden = 1
    | Concealed = 2
    
module VisibilityInfo =
    let fromVisibility (v: Shared.Visibility) : VisibilityInfo =
        match v with
        | Shared.Visible -> VisibilityInfo.Visible
        | Shared.Hidden -> VisibilityInfo.Hidden
        | Shared.Concealed -> VisibilityInfo.Concealed

type NarrativeStyleInfo =
    | Regular = 0
    | Emphasized = 1
    | Hint = 2
    | Dialogue = 3
    | System = 4

type ItemInfo =
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
      Items: ItemInfo[]
      Characters: CharacterInfo[] }

type PlayerInfo =
    { Inventory: ItemInfo[] }

type GameStateInfo =
    { Room: RoomInfo
      Player: PlayerInfo }


type IRenderer =
    abstract member Clear : unit -> unit
    abstract member RenderText : string * NarrativeStyle -> unit
    abstract member RenderGameState: GameStateInfo -> unit


type IFileIO =
    abstract member WriteFile : filename:string -> allowOverwrite:bool -> content:string -> WriteFileResult
    abstract member Serialize: obj:obj -> Result<string, string>


type IDebugOutput =
    abstract member RenderState: GlobalResult -> Event -> unit
    abstract member RenderSystemMessage: string -> unit
    
 
type IAsyncInput =
    abstract member ReadInput : System.Threading.CancellationToken -> Async<string>


type IParserHelper =
    abstract member ResolveActionsForSentence : string -> obj []

type EngineMessage =
    | UpdatedGameState of GameStateInfo
    | NewHistoryItem of string * NarrativeStyleInfo
    | ClearScreen
    | DebugOutputResult of GlobalResult * Ifai.Lib.Event
    | DebugOutputMessage of string
    | RequestQuit
    | Batch of EngineMessage []

[<AbstractClass>]
type EngineMessageInfo(msg) =
    member _.OriginalMessage : EngineMessage = msg
    static member FromEngineMessage(msg: EngineMessage) : EngineMessageInfo =
        match msg with
        | UpdatedGameState gameState -> UpdatedGameStateMessage(gameState) :> EngineMessageInfo
        | NewHistoryItem (text, style) -> NewHistoryItemMessage(text, style) :> EngineMessageInfo
        | ClearScreen -> ClearScreenMessage() :> EngineMessageInfo
        | DebugOutputResult (result, event) -> DebugOutputResultMessage(result, event) :> EngineMessageInfo
        | DebugOutputMessage message -> DebugOutputMessageMessage(message) :> EngineMessageInfo
        | RequestQuit -> RequestQuitMessage() :> EngineMessageInfo
        | Batch messages -> 
            let infos = messages |> Array.map EngineMessageInfo.FromEngineMessage
            BatchMessage(infos) :> EngineMessageInfo

and UpdatedGameStateMessage(gameState: GameStateInfo) =
    inherit EngineMessageInfo(EngineMessage.UpdatedGameState gameState)
    member _.GameState = gameState

and NewHistoryItemMessage(text: string, style: NarrativeStyleInfo) =
    inherit EngineMessageInfo(EngineMessage.NewHistoryItem (text, style))
    member _.Text = text
    member _.Style = style

and ClearScreenMessage() =
    inherit EngineMessageInfo(EngineMessage.ClearScreen)

and DebugOutputResultMessage(result: GlobalResult, event: Ifai.Lib.Event) =
    inherit EngineMessageInfo(EngineMessage.DebugOutputResult (result, event))
    member _.Result = result
    member _.Event = event

and DebugOutputMessageMessage(message: string) =
    inherit EngineMessageInfo(EngineMessage.DebugOutputMessage message)
    member _.Message = message

and RequestQuitMessage() =
    inherit EngineMessageInfo(EngineMessage.RequestQuit)

and BatchMessage(messages: EngineMessageInfo[]) =
    inherit EngineMessageInfo(EngineMessage.Batch (messages |> Array.map (fun m -> m.OriginalMessage)))
    member _.Messages = messages
 

type EngineCommand =
    | UserInput of string
 

type IEngineInput =
    abstract member Send : EngineCommand -> unit

/// <summary>
/// This represents the in- and output definition of the game loop<br/>
/// <c>Input</c> provides a way for the host of the engine to send input into the loop <br/>
/// <c>Output</c> needs to be subscribed to by the host program to receive messages from the game loop
/// <c>CancellationToken</c> is used to stop the game loop
/// </summary>
type Engine = {
    Input: IEngineInput
    Output: IObservable<EngineMessageInfo>
    CancellationTokenSource: System.Threading.CancellationTokenSource
}