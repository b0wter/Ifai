namespace Ifai.Lib.Content

open Ifai.Lib

/// A region is a collection of rooms that all share the same set of modifiers.
type Region = {
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    RoomIds: RoomId list
    Modifiers: RoomModifier list
}
