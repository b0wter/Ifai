namespace Ifai.Lib

open Ifai.Lib

type ActivationResult<'state, 'event> = {
    State: 'state
    Event: 'event option
    Runtime: RuntimeAction<'event>
    Render: RenderAction
}

module ActivationResult =

    let init state =
        {
            State = state
            Event = None
            Runtime = RuntimeAction.Nothing
            Render = RenderAction.Nothing
        }

    let withEvent<'event> (event: 'event) state =
        { state with Event = Some event }

    let withRuntime action state =
        { state with Runtime = action }
        
    let withRender render state =
        { state with Render = render }
