namespace Ifai.Lib

open Ifai.Lib
open Ifai.Lib.Modes

type GameMode =
    | Exploring of Exploring.ExploringState
    | Loading of Loading.LoadingState
    | Saving of Saving.SavingState


type Model = {
    World: World
    GameMode: GameMode list
    Language: Language
    TextResources: TextResources
}


type Event =
    | Exploring of Exploring.ExploringEvent
    | Saving of Saving.SavingEvent
    | RawInput of string
    | FileWrittenSuccessfully
    | FileWriteFailed of reason:string
    | FileAlreadyExists of filename:string


/// This is the result of the top-level update function
type GlobalResult = {
    Model: Model
    Runtime: RuntimeAction
    Render: RenderAction
    Transition: ModeTransition
}

module GlobalResult =
    let init m = { Model = m; Runtime = RuntimeAction.Nothing; Render = RenderAction.Nothing; Transition = ModeTransition.Nothing }
    
    let withRuntime (a: RuntimeAction) (r: GlobalResult) : GlobalResult =
        { r with Runtime = a }

    let withRender (a: RenderAction) (r: GlobalResult) : GlobalResult =
        { r with Render = a }

    let withTransition (t: ModeTransition) (r: GlobalResult) : GlobalResult =
        { r with Transition = t }

