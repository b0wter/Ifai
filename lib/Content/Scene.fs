namespace Ifai.Lib.Content

open Ifai.Lib

type StartSceneTrigger =
    | TurnTrigger of uint


type EndSceneTrigger =
    | TurnTrigger of uint


/// <summary>
/// A scene describes an event that takes place over several turns, like a storm.<br/>
/// Scenes can modify room properties, like descriptions, attributes and add/remove modifiers.
/// They are started and ended by triggers
/// </summary>
type Scene = {
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    /// Text that is shown when the scene is started
    StartText: Text.LocalizedText
    /// Text that is shown when the scene is ended
    EndText: Text.LocalizedText
    /// Trigger is checked at each turn. Is optional because scenes can also be started through other means
    StartTrigger: StartSceneTrigger option
    /// Trigger is checked at each turn. Is optional because scenes can also be ended through other means
    EndTrigger: EndSceneTrigger option
}

