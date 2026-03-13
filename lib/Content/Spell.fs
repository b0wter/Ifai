namespace Ifai.Lib.Content

open Ifai.Lib

type SpellTarget =
    | Room
    | Character
    | Item


type Spell = {
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    AllowedTargets: Set<SpellTarget>
    DurationInTurns: uint
}


module Spell =
    let name s = s.Name
    let desc s = s.Description
    let allowedTargets s = s.AllowedTargets
    let durationInTurns s = s.DurationInTurns
