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
    | Clear
    | Text of Text
    | Fallback of string
    | Batch of RenderAction list


type ToSavingModeParameters = {
    Filename: string option
}


type ToLoadingModeParameters = {
    Filename: string option
}


type ToTransitionParameters = {
    FromRoomId: RoomId
    ToRoomId: RoomId
}


type ModeTransition =
    | Nothing
    | Finished
    | StartExploring
    | StartSaving of ToSavingModeParameters
    | StartLoading of ToLoadingModeParameters
    | StartTransition of ToTransitionParameters
