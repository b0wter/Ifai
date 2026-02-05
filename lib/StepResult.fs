namespace Ifai.Lib

open Ifai.Lib


/// This is the result of a game-mode-specific update
type StepResult<'state> = {
    World: World
    State: 'state
    Runtime: RuntimeAction
    Render: RenderAction
    Transition: ModeTransition
}

module StepResult =
    let init w s =
        { World = w; State = s; Runtime = RuntimeAction.Nothing; Render = RenderAction.Nothing; Transition = ModeTransition.Nothing }
        
    let withRuntime<'state> (a: RuntimeAction) (r: StepResult<'state>) : StepResult<'state> =
        { r with Runtime = a }

    let withRender<'state> (a: RenderAction) (r: StepResult<'state>) : StepResult<'state> =
        { r with Render = a }

    let withTransition<'state> (t: ModeTransition) (r: StepResult<'state>) : StepResult<'state> =
        { r with Transition = t }
