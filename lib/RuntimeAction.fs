namespace Ifai.Lib

type Parsing =
    | ParseExploringInput of Language * string
    | ParseSavingInput of Language * string
    | ParseLoadingInput of Language * string
    | ParseEnteringRoomInput of Language * string
    | ParseLeavingRoomInput of Language * string


type WriteFileResult =
    | Success
    | Failure of reason:string
    | AlreadyExists of filename:string


type ReadFileResult =
    | Success of content:string
    | Failure of reason:string
    | DoesNotExist of filename:string


type RuntimeAction =
    | Nothing
    | Parsing of Parsing
    | SaveGame of filename:string * allowOverwrite:bool
    | SerializeAndWriteToFile of filename:string * allowOverwrite:bool * content:obj
    | WriteFile of filename:string * allowOverwrite:bool * content:string
    | ReadFile of filename:string
    | Quit


type RenderAction =
    | Nothing
    | Render of Text
    | RenderMany of Text list   // having Render and RenderMany is just for convenience
    | Clear
    | ClearAndRender of Text
    | ClearAndRenderMany of Text list
    /// This is to be only used in case of "emergencies" when there cannot be a predefined text.
    /// Most likely for errors and exceptions
    | FallbackRender of string


type ToSavingModeParameters = {
    Filename: string option
}


type ToLoadingModeParameters = {
    Filename: string option
}


type ToLeavingModeParameters = {
    FromRoomId: RoomId
    ToRoomId: RoomId
}


type ToEnteringRoomParameters = {
    FromRoomId: RoomId
    ToRoomId: RoomId
}


type ModeTransition =
    | Nothing
    | Finished
    | StartExploring
    | StartSaving of ToSavingModeParameters
    | StartLoading of ToLoadingModeParameters
    | StartEnteringRoom of ToEnteringRoomParameters
    | StartLeavingRoom of ToLeavingModeParameters
