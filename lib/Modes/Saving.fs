module Ifai.Lib.Modes.Saving

open Ifai.Lib

type SavingBuiltIn =
    | Yes
    | No
    | Abort


let private english : (string list * (string option -> SavingBuiltIn)) list =
    [
    ["y"; "yes"], fun _ -> Yes
    ["n"; "no"], fun _ -> No
    ["q"; "a"; "abort"], fun _ -> Abort
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
    | OverwriteFile
    | DoNotOverwriteFile
    | Abort
    | Unknown


type SavingEvent =
    | UserInput of Input
    /// Sent to this game mode from the runtime when saving was successful
    | IoSuccess
    /// Sent to this game mode from the runtime when saving failed
    | IoFailure of string
    /// Sent to this game mode from the runtime when the file to save to already exists
    | FileAlreadyExists of string


type SavingState =
    /// The user is asked for a filename
    | AskingForFilename
    /// The runtime was ordered to try to save the file and the save-mode is waiting for the result
    | SavingFile of filename:string * allowOverwrite:bool
    /// The user is asked if they want to overwrite the existing file
    | AskingForOverwritePermission of filename:string
    /// Defines a state reached by successfully saving
    | Finished

    
let init (parameters: ToSavingModeParameters) : SavingState * RuntimeAction * RenderAction =
    match parameters.Filename with
    | Some filename -> (filename, false) |> SavingFile, (filename, false) |> RuntimeAction.SaveGame, RenderAction.Nothing
    | None -> AskingForFilename, RuntimeAction.Nothing, RenderAction.FallbackRender "Enter the name of the file you want to save to:"


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
    // ---
    | SavingFile _, _ -> SavingIntent.Unknown
    // ---
    | AskingForOverwritePermission _, Input.Filename _ -> SavingIntent.Unknown
    | AskingForOverwritePermission _, Input.BuiltIn SavingBuiltIn.No -> SavingIntent.DoNotOverwriteFile
    | AskingForOverwritePermission _, Input.BuiltIn SavingBuiltIn.Yes -> SavingIntent.OverwriteFile
    | AskingForOverwritePermission _, Input.BuiltIn SavingBuiltIn.Abort -> SavingIntent.Abort
    | AskingForOverwritePermission _, Input.Ambiguous _ -> SavingIntent.Unknown
    // ---
    | Finished, _ -> SavingIntent.Unknown


let handleIntentForAskingForFilename (world: World) (intent: SavingIntent) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match intent with
    | EnterFilename filename -> world, (filename, false) |> SavingFile, RuntimeAction.SaveGame (filename, false), RenderAction.Nothing, ModeTransition.Nothing
    | Abort -> world, AskingForFilename, RuntimeAction.Nothing, RenderAction.FallbackRender "Okay, I will not save the game.", ModeTransition.Finished
    // intents that do not match the current state
    | OverwriteFile
    | DoNotOverwriteFile
    | Unknown -> world, AskingForFilename, RuntimeAction.Nothing, RenderAction.FallbackRender "I do not understand, please try again. Enter the name of the file you want to save to", ModeTransition.Nothing


let handleIntentForAskingForOverwritePermission (world: World) (intent: SavingIntent) filename : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match intent with
    | OverwriteFile -> world, SavingState.SavingFile (filename, true), RuntimeAction.SaveGame (filename, true), RenderAction.Nothing, ModeTransition.Nothing
    | DoNotOverwriteFile -> world, SavingState.Finished, RuntimeAction.Nothing, RenderAction.FallbackRender "I will not overwrite the file.", ModeTransition.Finished
    | Abort -> world, SavingState.Finished, RuntimeAction.Nothing, RenderAction.FallbackRender "Okay, I will not save the game.", ModeTransition.Finished
    // intents that do not match the current state
    | EnterFilename _ -> world, filename |> AskingForOverwritePermission, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.Nothing
    | Unknown -> world, filename |> AskingForOverwritePermission, RuntimeAction.Nothing, RenderAction.FallbackRender "I do not understand, please try again. Do you want to overwrite the file? [yes/no/abort]", ModeTransition.Nothing


let handleIntentForSavingFile (world: World) (intent: SavingIntent) (filename, allowOverwrite) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match intent with
    | _ -> world, (filename, allowOverwrite) |> SavingFile, RuntimeAction.Nothing, RenderAction.FallbackRender "Inputs are not supported while saving the game", ModeTransition.Nothing


let handleIntentForFinished (world: World) (intent: SavingIntent) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match intent with
    | _ -> world, Finished, RuntimeAction.Nothing, RenderAction.FallbackRender "Inputs are not supported while transitioning modes", ModeTransition.Finished


let handleIntent (world: World) (state: SavingState) (intent: SavingIntent) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match state with
    | AskingForFilename -> handleIntentForAskingForFilename world intent
    | AskingForOverwritePermission filename -> handleIntentForAskingForOverwritePermission world intent filename
    | SavingFile (filename, allowOverwrite) -> handleIntentForSavingFile world intent (filename, allowOverwrite)
    | Finished -> handleIntentForFinished world intent


let updateSaving (world: World) (state: SavingState) (msg: SavingEvent) : World * SavingState * RuntimeAction * RenderAction * ModeTransition =
    match state, msg with
    | _, UserInput input ->
        input
        |> resolveUserIntent state
        |> (handleIntent world state)
    | SavingFile _, SavingEvent.IoSuccess ->
        world, SavingState.Finished, RuntimeAction.Nothing, RenderAction.FallbackRender "Saved game successfully", ModeTransition.Finished
    | SavingFile _, SavingEvent.IoFailure error ->
        world, SavingState.Finished, RuntimeAction.Nothing, RenderAction.FallbackRender $"Could not save game because: %s{error}", ModeTransition.Finished
    | SavingFile _, SavingEvent.FileAlreadyExists filename ->
        world, filename |> SavingState.AskingForOverwritePermission, RuntimeAction.Nothing, RenderAction.FallbackRender $"The file '%s{filename}' already exists. Overwrite? [y/n/a]", ModeTransition.Nothing
    | _ ->
        failwith $"The state %A{state} is not yet implemented"
        //world, state, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.Nothing