namespace Ifai.Lib.Content

open Ifai.Lib

type ThingLocation =
    | Room of RoomId
    | Player
    | InOtherThing of ThingId
    | HeldBy of CharacterId
    | Nowhere


type Interactability =
    /// This thing may serve a purpose, but the player has no way of interacting with it.
    /// Can also be used to enrich room descriptions
    | Nothing
    /// Can be examined but not used in any meaningful way
    | ExamineOnly
    | All


type ThingModifier =
    // There is no dedicated "Fixed" state since that is the default.
    // Things that are broken at the beginning of the game should also treat the
    // 'fixed' state as their default and start with the 'broken' modifier.
    // This is to prevent a collision; what happens if a thing has the 'broken' and 'fixed' modifier?
    | Broken
    /// Change in temperature, use this to model hot, cold or frozen things
    | Temperature of delta:int
    | Wetness of level:int
    /// Changes the divine alignment of the thing, values lesser 0 mean the thing is cursed and
    /// values greater 0 mean it is blessed
    | DivineAlignment of delta:int
    /// Thing is not visible because it is hidden behind/under/over/... another thing
    | CoveredBy of ThingId

    /// Thing is cloaked and thus invisible; allows defining a thing that causes this effect
    | Cloaked of cloakingThing: ThingId option
    /// Thing is no longer in this dimension; allows defining a thing that causes this effect
    | OutOfPhase of phasingThing: ThingId option
    /// Thing has deliberately been camouflaged
    | Camouflaged of camouflageQuality:uint
    /// Under the influence of an invisibility spell (or similar)
    | HiddenBySpell of SpellId
    /// Thing has been hidden by some other reason defined via scripting
    | HiddenBy of name:Shared.AttributeId * value:Shared.AttributeValue
    /// Allows authors to add custom attributes to thing
    | Custom of name:Shared.AttributeId * value:Shared.AttributeValue


type LegalOwner =
    | Player
    | Character of CharacterId
    | Nobody
    

type Thing = {
    Id: ThingId
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    Synonyms: Text.LocalizedText list
    /// Describes if the thing can theoretically be moved/taken.
    /// Is not the final truth since the modifiers need to be applied
    IsPortable: bool
    Weight: uint
    /// Abstract things are never represented in the world, regardless of any visibility rules.
    /// Use abstract things to implement concepts/items/stuff that has no visible representation
    /// in the world like magic, "the Force" or advanced scripting helpers
    IsAbstract: bool
    Interactability: Interactability
    LegalOwner: LegalOwner
    Traits: Trait list
    /// A thing that needs discovery will be ignored by certain actions (like Take All)
    /// until it is discovered by the player. This is useful for hidden items, like keys
    /// under doormats. The value of this property should never change because this acts as
    /// the initial value. The fact that this item needs to be discovered not if it was discovered.
    /// Discovery of things is handled in the PlayerKnowledge
    NeedsDiscovery: bool
}


module Thing =
    let name i = i.Name
    let desc i = i.Description
    let synonyms i = i.Synonyms
    let isPortable i = i.IsPortable
    let weight i = i.Weight
    let isAbstract i = i.IsAbstract
    let interactability i = i.Interactability
    let legalOwner i = i.LegalOwner

    
    let applyModifier (m: ThingModifier) (modifiers: Set<ThingModifier>) : Set<ThingModifier> =
        match m with
        | Broken
        | HiddenBy _
        | CoveredBy _
        | Custom _
        | Cloaked _
        | OutOfPhase _
        | Camouflaged _ -> modifiers |> Set.add m
        | HiddenBySpell _ ->
            // This one is tough because what we probably want is to extend the spell duration.
            // That's not possible at the moment because we only have the spell id which is the global id of the
            // spell and not the specific cast!
            failwith "not implemented"
        | Temperature deltaT ->
            Shared.applyDeltaModifier
                0
                (+)
                id
                id
                (function Temperature _ -> true | _ -> false)
                (function Temperature dt -> dt | _ -> 0)
                Temperature
                deltaT
                modifiers
        | DivineAlignment deltaA ->
            Shared.applyDeltaModifier
                0
                (+)
                id
                id
                (function DivineAlignment _ -> true | _ -> false)
                (function DivineAlignment dA -> dA | _ -> 0)
                DivineAlignment
                deltaA
                modifiers
        | Wetness deltaW ->
            Shared.applyDeltaModifier
                0
                (+)
                (fun i -> System.Math.Max(i, 0))
                id
                (function Wetness _ -> true | _ -> false)
                (function Wetness dW -> dW | _ -> 0)
                Wetness
                deltaW
                modifiers
