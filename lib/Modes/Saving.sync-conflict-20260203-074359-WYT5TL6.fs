module Ifai.Lib.Saving


type SavingBuiltIn =
    | Yes
    | No
    | Abort


let private english : (string list * (string option -> SavingBuiltIn)) list =
    [
    ["y"; "yes"], fun _ -> Yes
    ["n"; "no"], fun _ -> No
    ["q"; "abort"], fun _ -> Abort
    ]
    
    
let private german : (string list * (string option -> SavingBuiltIn)) list =
    [
    ["j"; "ja"], fun _ -> Yes
    ["n"; "nein"], fun _ -> No
    ["abbrechen"], fun _ -> Abort
    ]


let builtIns =
    [ Language.create "en", english
      Language.create "de", german
    ] |> Map.ofList


type Input =
    | BuiltIn of SavingBuiltIn
    | Ambiguous of SavingBuiltIn list
    | Filename of string


/// This represents the user's intent as extracted from the entered input
type SavingIntent =
    | EnterFilename of filename:string
    | OverwriteFile of filename:string
    | DoNotOverwriteFile
    | Abort
    | Unknown


type SavingEvent =
    | Save of filename:string
    | UserInput of Input
    | AskForOverwriting of filename:string
    | Cancel
    | Success
    | Failure of string


type SavingState =
    | AskingForFilename
    | SavingFile of filename:string
    | AskingForOverwritePermission of filename:string
    | FinishedWithSavedFile
    | FinishedWithoutSavingFile
    | Failure
    
    
let init (parameters: ToSavingModeParameters) : SavingState =
    match parameters.Filename with
    | Some filename -> filename |> SavingFile
    | None -> AskingForFilename

(*
type SavingState = {
    AskedForOverWritePermission: bool
    HasOverwritePermission: bool
    Filename: string option
}


let init (parameters: ToSavingModeParameters) : SavingState =
    { Filename = parameters.Filename; AskedForOverWritePermission = false; HasOverwritePermission = false }
*)

/// Turns raw text input into intent
let parser (language: Language) (input: string) : Input =
    let matches = input |> Parser.tryParseBuiltIn builtIns language
    if matches.Length = 0 then input |> Filename
    else if matches.Length = 1 then matches.Head |> Input.BuiltIn
    else matches |> Input.Ambiguous
    

let resolveUserIntent (state: SavingState) (input: Input) : SavingIntent =
    match state, input with
    | AskingForFilename, Input.Filename f -> f |> SavingIntent.EnterFilename
    | AskingForFilename, Input.Ambiguous _ -> SavingIntent.Unknown
    | AskingForFilename, Input.BuiltIn _ -> SavingIntent.Unknown
    | AskingForOverwritePermission _, Input.Filename _ -> SavingIntent.Unknown
    | AskingForOverwritePermission _, Input.BuiltIn SavingBuiltIn.No -> SavingIntent.DoNotOverwriteFile
    | AskingForOverwritePermission f, Input.BuiltIn SavingBuiltIn.Yes -> f |> SavingIntent.OverwriteFile
    | AskingForOverwritePermission _, Input.Ambiguous _ -> SavingIntent.Unknown
    | AskingForOverwrit


let updateSaving (world: World) (state: SavingState) (msg: SavingEvent) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match state, msg with
    | AskingForFilename, UserInput input
    | _ -> world, state, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.Nothing