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
    | None -> AskingForFilename, RuntimeAction.Nothing, RenderAction.Fallback "Enter the name of the file you want to save to:"


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


let handleIntentForAskingForFilename (world: World) (intent: SavingIntent) : StepResult<SavingState> =
    match intent with
    | EnterFilename filename ->
        StepResult.init
            world
            ((filename, false) |> SavingFile)
        |> StepResult.withRuntime (RuntimeAction.SaveGame (filename, false))
 
    | Abort ->
        StepResult.init
            world
            AskingForFilename
        |> StepResult.withRender (RenderAction.Fallback "Okay, I will not save the game.")
        |> StepResult.withTransition ModeTransition.Finished
    // intents that do not match the current state
    | OverwriteFile
    | DoNotOverwriteFile
    | Unknown ->
        StepResult.init
            world
            AskingForFilename
        |> StepResult.withRender (RenderAction.Fallback "I do not understand, please try again. Enter the name of the file you want to save to")


let handleIntentForAskingForOverwritePermission (world: World) (intent: SavingIntent) filename : StepResult<SavingState> =
    match intent with
    | OverwriteFile ->
        StepResult.init
            world
            (SavingState.SavingFile (filename, true))
        |> StepResult.withRuntime (RuntimeAction.SaveGame (filename, true))

    | DoNotOverwriteFile ->
        StepResult.init
            world
            SavingState.Finished
        |> StepResult.withRender (RenderAction.Fallback "I will not overwrite the file.")
        |> StepResult.withTransition ModeTransition.Finished

    | Abort ->
        StepResult.init
            world
            SavingState.Finished
        |> StepResult.withRender (RenderAction.Fallback "Okay, I will not save the game.")
        |> StepResult.withTransition ModeTransition.Finished

    // intents that do not match the current state
    | EnterFilename _ ->
        StepResult.init
            world
            (filename |> AskingForOverwritePermission)
    | Unknown ->
        StepResult.init
            world
            (filename |> AskingForOverwritePermission)
        |> StepResult.withRender (RenderAction.Fallback "I do not understand, please try again. Do you want to overwrite the file? [yes/no/abort]")


let handleIntentForSavingFile (world: World) (intent: SavingIntent) (filename, allowOverwrite) : StepResult<SavingState> =
    match intent with
    | _ ->
        StepResult.init
            world
            ((filename, allowOverwrite) |> SavingFile)
        |> StepResult.withRender (RenderAction.Fallback "Inputs are not supported while saving the game")


let handleIntentForFinished (world: World) (intent: SavingIntent) : StepResult<SavingState> =
    match intent with
    | _ ->
        StepResult.init
            world
            Finished
        |> StepResult.withRender (RenderAction.Fallback "Inputs are not supported while transitioning modes")
        |> StepResult.withTransition ModeTransition.Finished


let handleIntent (world: World) (state: SavingState) (intent: SavingIntent) : StepResult<SavingState> =
    match state with
    | AskingForFilename -> handleIntentForAskingForFilename world intent
    | AskingForOverwritePermission filename -> handleIntentForAskingForOverwritePermission world intent filename
    | SavingFile (filename, allowOverwrite) -> handleIntentForSavingFile world intent (filename, allowOverwrite)
    | Finished -> handleIntentForFinished world intent


let updateSaving (world: World) (state: SavingState) (msg: SavingEvent) : StepResult<SavingState> =
    match state, msg with
    | _, UserInput input ->
        input
        |> resolveUserIntent state
        |> (handleIntent world state)

    | SavingFile _, SavingEvent.IoSuccess ->
        StepResult.init world SavingState.Finished
        |> StepResult.withRender (RenderAction.Text (Text.create (TextKey.create "saved_game_successfully")))
        |> StepResult.withTransition ModeTransition.Finished

    | SavingFile _, SavingEvent.IoFailure error ->
        StepResult.init world SavingState.Finished
        |> StepResult.withRender (RenderAction.Batch [ RenderAction.Text (Text.create (TextKey.create "error_while_saving_game")); RenderAction.Fallback error ])
        |> StepResult.withTransition ModeTransition.Finished

    | SavingFile _, SavingEvent.FileAlreadyExists filename ->
        StepResult.init
            world
            (filename |> SavingState.AskingForOverwritePermission)
        |> StepResult.withRender (RenderAction.Fallback $"The file '%s{filename}' already exists. Overwrite? [y/n/a]")

    | _ ->
        failwith $"The state %A{state} is not yet implemented"
        //world, state, RuntimeAction.Nothing, RenderAction.Nothing, ModeTransition.Nothing