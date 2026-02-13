namespace Ifai.Lib

open System

type Visibility =
    | Visible
    | Hidden
    | Concealed


type Interactability =
    | Nothing
    | ExamineOnly
    | All


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


type Item = {
    Id: ItemId
    Name: Text
    Description: Text
    IsPortable: bool
    Visibility: Visibility
    Interactability: Interactability
}

module Item =
    open System

