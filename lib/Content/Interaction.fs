namespace Ifai.Lib

type Actor =
    | Player
    | Character of CharacterId


type Target =
    | Many of Target list
    | Player
    | Room of RoomId
    | Character of CharacterId
    | Item of ItemId
    | Decoration of ItemId
    

type Instrument =
    | Nothing
    | Item of ItemId
    | Decoration of ItemId
    | Character of CharacterId


/// The interaction context contains all information required to resolve an interaction.
/// Is constructed at runtime and does not describe an action as given by the content files.
type InteractionContext = {
    Actor: Actor
    /// <summary>
    /// Any tool that it used in the interaction
    /// </summary>
    /// <example>
    /// "Throw pebble at lake" -> pebble
    /// </example>
    Instrument: Instrument
    Verb: string
    Target: Target
    /// Location context for the interaction
    Room: RoomId
}

module Interaction =
    open System

