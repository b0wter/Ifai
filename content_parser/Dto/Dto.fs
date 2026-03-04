namespace Ifai.ContentParser

type RoomDto = {
    Id: string
    Name: string
    Description: string
}

type TargetDto
    = ItemTarget of Ifai.Lib.ItemId
    | RoomTarget of Ifai.Lib.RoomId
    | CharacterTarget of Ifai.Lib.CharacterId

type Interaction = {
    Synonyms: string list
    Target: TargetDto
    Destroy: string option
    MoveTo: string option
    Say: string option
}

type SetDto = {
    Variable: string
    Value: string
}

type ConditionDto
    = Not of ConditionDto
    | And of ConditionDto * ConditionDto
    | Or of ConditionDto * ConditionDto
    | Variable of string

type SubActionDto = {
    If: ConditionDto
    Say: string option
    Set: SetDto list
}

/// This record is used to construct a SubActionDto by reading content line after content line
type SubActionInConstructionDto = {
    If: ConditionDto option
    Say: string option
    Set: SetDto list
}

type ActionDto = {
    Synonyms: string list
    SubActions: SubActionDto list
}

type ItemDto = {
    Id: string option
    Synonyms: string list
    Description: string
    Interactions: Interaction list
    State: Map<string, string>
    Actions: ActionDto list
}

type DecorationDto = {
    Id: string option
    Synonyms: string list
    Description: string
    State: Map<string, string>
    Actions: ActionDto list
}

type Dto
    = Room of RoomDto
    | Item of ItemDto
    | Decoration of DecorationDto
