namespace Ifai.Lib


type ItemLocation =
    | Room of RoomId
    | Player
    | InOtherItem of ItemId
    | Nowhere


type Interactability =
    /// This item may serve a purpose, but the player has no way of interacting with it.
    /// Can also be used to enrich room descriptions
    | Nothing
    /// Can be examined but not used in any meaningful way
    | ExamineOnly
    | All


type ItemModifier =
    // There is no dedicated "Fixed" state since that is the default.
    // Items that are broken at the beginning of the game should also treat the
    // 'fixed' state as their default and start with the 'broken' modifier.
    // This is to prevent a collision; what happens if an item has the 'broken' and 'fixed' modifier?
    | Broken
    /// Change in temperature, use this to model hot, cold or frozen items
    | Temperature of delta:int
    | Wetness of level:int
    /// Changes the divine alignment of the item, values lesser 0 mean the item us cursed and
    /// values greater 0 mean it is blessed
    | DivineAlignment of delta:int
    /// Item is not visible because it is hidden behind/under/over/... another item
    | CoveredBy of ItemId
    /// Item is cloaked and thus invisible; allows defining an item that causes this effect
    | Cloaked of cloakingItem:ItemId option
    /// Item is no longer in this dimension; allows defining an item that causes this effect
    | OutOfPhase of phasingItem:ItemId option
    /// Item has deliberately been camouflaged
    | Camouflaged of camouflageQuality:uint
    /// Under the influence of an invisibility spell (or similar)
    | HiddenBySpell of SpellId
    /// Item has been hidden by some other reason defined via scripting
    | HiddenBy of name:Shared.AttributeId * value:Shared.AttributeValue
    /// Allows authors to add custom attributes to items
    | Custom of name:Shared.AttributeId * value:Shared.AttributeValue


type LegalOwner =
    | Player
    | Character of CharacterId
    | Nobody
    

type Item = {
    Id: ItemId
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    Synonyms: Text.LocalizedText list
    MayContainItems: bool
    /// Describes if the item can theoretically be moved/taken.
    /// Is not the final truth since the modifiers need to be applied
    IsPortable: bool
    Weight: uint
    /// Abstract items are never represented in the world, regardless of any visibility rules.
    /// Use abstract items to implement concepts/items/stuff that has no visible representation
    /// in the world like magic, "the Force" or advanced scripting helpers
    IsAbstract: bool
    Interactability: Interactability
    LegalOwner: LegalOwner
}


module Item =
    let name i = i.Name
    let desc i = i.Description
    let synonyms i = i.Synonyms
    let isPortable i = i.IsPortable
    let weight i = i.Weight
    let isAbstract i = i.IsAbstract
    let interactability i = i.Interactability
    let legalOwner i = i.LegalOwner

    
    let applyModifier (m: ItemModifier) (modifiers: Set<ItemModifier>) : Set<ItemModifier> =
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
