namespace Ifai.Lib


type ItemId = ItemId of string
module ItemId =
    let create id = id |> ItemId
    let fromGuid guid = guid |> ItemId
    let value (ItemId i) = i
    

type ItemLocation =
    | Room of RoomId
    | Player
    | InOtherItem of ItemId
    | Nowhere


type Interactability =
    | Nothing
    | ExamineOnly
    | All


type Item = {
    Id: ItemId
    Name: Text
    Description: Text
    IsPortable: bool
    Visibility: Shared.Visibility
    Interactability: Interactability
}

module Item =
    open System

