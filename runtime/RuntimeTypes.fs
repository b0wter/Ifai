namespace Ifai.Runtime

open System
open Ifai.Lib
open Ifai.Lib.Shared


type ThingSnapshot =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Visibility: Visibility }


type CharacterSnapshot =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Visibility: Visibility }


type ConnectionSnapshot =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText option
      Visibility: Visibility }


type RoomSnapshot =
    { Name: Text.DisplayableText
      Description: Text.DisplayableText
      Exits: ConnectionSnapshot[]
      Things: ThingSnapshot[]
      Characters: CharacterSnapshot[] }


type PlayerSnapshot =
    { Inventory: ThingSnapshot[] }


type GameSnapshot =
    { Room: RoomSnapshot
      Player: PlayerSnapshot }


type EngineMessage =
    | UpdatedGameState of GameSnapshot
    | NewHistoryItem of text:string * style:NarrativeStyle
    | ClearScreen
    | DebugOutputResult of GlobalResult * Event
    | DebugOutputMessage of string
    | RequestQuit
    | Batch of EngineMessage[]


type EngineCommand =
    | UserInput of string


type IEngineInput =
    abstract member Send : EngineCommand -> unit


/// <summary>
/// This represents the in- and output definition of the game loop<br/>
/// <c>Input</c> provides a way for the host of the engine to send input into the loop <br/>
/// <c>Output</c> needs to be subscribed to by the host program to receive messages from the game loop
/// <c>CancellationTokenSource</c> is used to stop the game loop
/// </summary>
type RuntimeEngine = {
    Input: IEngineInput
    Output: IObservable<EngineMessage>
    CancellationTokenSource: System.Threading.CancellationTokenSource
}


type IRenderer =
    abstract member Clear : unit -> unit
    abstract member RenderText : string * NarrativeStyle -> unit
    abstract member RenderGameState: GameSnapshot -> unit


type IDebugOutput =
    abstract member RenderState: GlobalResult -> Event -> unit
    abstract member RenderSystemMessage: string -> unit


type IAsyncInput =
    abstract member ReadInput : System.Threading.CancellationToken -> Async<string>


type IParserHelper =
    abstract member ResolveActionsForSentence : string -> obj []
