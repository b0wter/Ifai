namespace Ifai.Lib

type Parsing =
    | ParseExploringInput of Language * string
    | ParseSavingInput of Language * string
    | ParseLoadingInput of Language * string
    | ParseTransitioningInput of Language * string


type WriteFileResult =
    | Success
    | Failure of reason:string
    | AlreadyExists of filename:string


type ReadFileResult =
    | Success of content:string
    | Failure of reason:string
    | DoesNotExist of filename:string


type RuntimeAction<'event> =
    | Nothing
    | Parsing of Parsing
    | SaveGame of filename:string * allowOverwrite:bool
    | SerializeAndWriteToFile of filename:string * allowOverwrite:bool * content:obj
    | WriteFile of filename:string * allowOverwrite:bool * content:string
    | ReadFile of filename:string
    | OfEvent of 'event
    | Quit

module RuntimeAction =
    let map (map: 'a -> 'b) (r: RuntimeAction<'a>) : RuntimeAction<'b> =
        match r with
        | Nothing -> Nothing
        | Parsing p -> Parsing p
        | SaveGame(filename, allowOverwrite) -> SaveGame(filename, allowOverwrite)
        | SerializeAndWriteToFile(filename, allowOverwrite, content) -> SerializeAndWriteToFile(filename, allowOverwrite, content)
        | WriteFile(filename, allowOverwrite, content) -> WriteFile(filename, allowOverwrite, content)
        | ReadFile filename -> ReadFile filename
        | OfEvent a -> OfEvent (a |> map)
        | Quit -> Quit


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


type LeavingRoomMode =
    /// Handle the event triggered by leaving the room
    | Full
    /// Skip the event triggered by leaving the room
    | Skip


type EnteringRoomMode =
    /// Handle the event triggered by entering the room and display the room description
    | Full
    /// Handle the event triggered by entering the room but skip the room description
    | EventOnly
    /// Skip the event triggered by entering the room but display the room description
    | SkipEvent
    /// Skip everything, including the room description
    | SkipAll


type ToTransitioningParameters = {
    LeavingMode: LeavingRoomMode
    EnteringMode: EnteringRoomMode
    FromRoomId: RoomId
    ToRoomId: RoomId
}


type ModeTransition =
    | Nothing
    | Finished
    | StartExploring
    | StartSaving of ToSavingModeParameters
    | StartLoading of ToLoadingModeParameters
    | StartTransition of ToTransitioningParameters
