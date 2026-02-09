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



type ItemId = ItemId of Guid
module ItemId =
    let create () = Guid.NewGuid() |> ItemId
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

