namespace Ifai.Lib

(*
 To make it interop better with C# we change all lists to arrays since F# lists are awkward to use in C#.
 F# records work fine for our use case as the clients simply display but do not modify the data.
*)

type VisibilityInfo =
    | Visible = 0
    | Hidden = 1
    | Concealed = 2

type ItemInfo =
    { Name: string
      Description: string
      Visibility: VisibilityInfo }

type CharacterInfo =
    { Name: string
      Description: string
      Visibility: VisibilityInfo }

type ExitInfo =
    { Name: string
      Visibility: VisibilityInfo }

type RoomInfo =
    { Name: string
      Description: string
      Exits: ExitInfo[]
      Characters: CharacterInfo[] }

type PlayerInfo =
    { Inventory: ItemInfo[] }

type GameStateView =
    { Room: RoomInfo
      Player: PlayerInfo }

type IRenderer =
    abstract member Clear : unit -> unit
    abstract member RenderText : string * NarrativeStyle -> unit
    abstract member RenderGameState: GameStateView -> unit


type IFileIO =
    abstract member WriteFile : filename:string -> allowOverwrite:bool -> content:string -> WriteFileResult
    abstract member Serializer: obj:obj -> Result<string, string>
    
    
type IDebugOutput =
    abstract member Render : (GlobalResult * Event) -> unit