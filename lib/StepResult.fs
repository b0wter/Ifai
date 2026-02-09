namespace Ifai.Lib

open Ifai.Lib


type StepInput<'state, 'event> = {
    World: World
    State: 'state
    Event: 'event
}
module StepInput =
    let init w s e = { World = w; State = s; Event = e }


/// This is the result of a game-mode-specific update
type StepResult<'state, 'event> = {
    World: World
    State: 'state
    Runtime: RuntimeAction<'event>
    Render: RenderAction
    Transition: ModeTransition
}
module StepResult =
    let init<'a,'b> w (s: 'a) =
        { World = w; State = s; Runtime = RuntimeAction<'b>.Nothing; Render = RenderAction.Nothing; Transition = ModeTransition.Nothing }
        
    let withRuntime<'state, 'event> (a: RuntimeAction<'event>) (r: StepResult<'state, 'event>) : StepResult<'state, 'event> =
        { r with Runtime = a }

    let withRender<'state, 'event> (a: RenderAction) (r: StepResult<'state, 'event>) : StepResult<'state, 'event> =
        { r with Render = a }

    let withTransition<'state, 'event> (t: ModeTransition) (r: StepResult<'state, 'event>) : StepResult<'state, 'event> =
        { r with Transition = t }
