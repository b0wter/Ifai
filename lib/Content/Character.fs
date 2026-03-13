namespace Ifai.Lib.Content

open Ifai.Lib

type CharacterLocation =
    | Nowhere
    | Room of RoomId


type RelationshipDynamic =
    | Hostile
    | Friendly
    | Neutral
    | Romantic


type Character = {
    Id: CharacterId
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    PlayerRelationship: RelationshipDynamic
}


type CharacterModifiers =
    /// Allows authors to add custom attributes to characters
    | Custom of name:Shared.AttributeId * value:Shared.AttributeValue
    
    
module Character =
    let id c = c.Id
    let name c = c.Name
    let desc c = c.Description
    let playerRelationship c = c.PlayerRelationship
