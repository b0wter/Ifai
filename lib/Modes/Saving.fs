module Ifai.Lib.Saving


type SavingBuiltIn =
    | Yes
    | No
    | SaveAs of filename:string


let private english : (string list * (string -> SavingBuiltIn)) list =
    [
    ["y"; "yes"], fun _ -> Yes
    ["n"; "no"], fun _ -> No
    ]
    
    
let private german : (string list * (string -> SavingBuiltIn)) list =
    [
    ["j"; "ja"], fun _ -> Yes
    ["n"; "nein"], fun _ -> No
    ]


let builtIns =
    [ Language.create "en", english
      Language.create "de", german
    ] |> Map.ofList


type Input =
    | BuiltIn of SavingBuiltIn
    | Sentence of string


/// This represents the user's intent as extracted from the entered input
type SavingIntent =
    | EnterFilename
    | OverwriteFile
    | DoNotOverwriteFile
    | Abort

type SavingMsg =
    | Save of filename:string
    | UserInput of Input
    | AskForOverwriting of filename:string
    | Cancel
    | Success
    | Failure of string


type SavingCmd =
    | Nothing


type SavingState = {
    Filename: string option
}


let runCmd (cmd: SavingCmd) : SavingMsg list =
    match cmd with
    | Nothing -> []


/// Turns raw text input into intent
let parser (language: Language) (input: string) : Input =
    failwith "Not implemented"
    
    
let updateSaving (world: World) (state: SavingState) (msg: SavingMsg) : World * SavingState * SavingCmd =
    match msg with
    | _ -> world, state, Nothing