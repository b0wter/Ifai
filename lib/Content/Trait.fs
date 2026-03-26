namespace Ifai.Lib.Content

open Ifai.Lib
open Ifai.Lib.Shared

type IHaveTraitParameters =
    abstract member AsMap: Map<string, obj>

type Openable = {
    IsOpen: bool
    OnOpened: string option
    OnClosed: string option
}

type Lockable = {
    IsLocked: bool
    OnLocked: string option
    OnUnlocked: string option
}

type Passage = {
    To: RoomId
}

type Container = {
    MaximumNumberOfThings: uint option
    MaximumWeight: uint option
}

type LightSource = {
    Brightness: uint option
}

type Trait =
    | Openable of Openable
    | Lockable of Lockable
    | Passage of Passage
    | Container of Container
    | LightSource of LightSource
    interface IHaveTraitParameters with
        member this.AsMap =
            match this with
            | Openable o ->
                Map.empty
                |> Map.add (nameof(o.IsOpen)) o.IsOpen
            | Lockable l ->
                Map.empty
                |> Map.add (nameof(l.IsLocked)) l.IsLocked
            | Container c ->
                Map.empty
                |> Map.add (nameof(c.MaximumNumberOfThings)) (c.MaximumNumberOfThings :> obj)
                |> Map.add (nameof(c.MaximumWeight)) (c.MaximumWeight :> obj)
            | Passage _ ->
                Map.empty
            | LightSource l ->
                Map.empty
                |> Map.add (nameof(l.Brightness)) (l.Brightness :> obj)

    interface IAttributeTranslator with
        member this.Translate() =
            match this with
            | Openable o ->
                (AttributeId.create "isOpen", (o.IsOpen |> AttributeValue.Bool)) |> List.singleton
            | Lockable l ->
                (AttributeId.create "isLocked", (l.IsLocked |> AttributeValue.Bool)) |> List.singleton
            | Passage _ -> []
            | Container _ -> []
            | LightSource _ -> []
