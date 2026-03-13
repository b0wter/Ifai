namespace Ifai.Lib.Content

open Ifai.Lib

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
    MaximumNumberOfItems: uint option
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

type Door = {
    InitiallyLocked: bool
    InitiallyOpen: bool
    OnOpened: string option
    OnClosed: string option
    OnLocked: string option
    OnUnlocked: string option
}

type CompositeTrait =
    | Door of Door

module Traits =
    let expandCompositeTraits = function
        | Door state ->
            [ Openable { IsOpen = state.InitiallyOpen; OnOpened = state.OnOpened; OnClosed = state.OnClosed }
            , Lockable { IsLocked = state.InitiallyLocked; OnLocked = state.OnLocked; OnUnlocked = state.OnUnlocked }
            ]
