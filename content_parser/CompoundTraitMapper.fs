module Ifai.ContentParser.CompoundTraitMapper

open Ifai.Lib.Content

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


let expandCompositeTraits = function
    | Door state ->
        [ Openable { IsOpen = state.InitiallyOpen; OnOpened = state.OnOpened; OnClosed = state.OnClosed }
        , Lockable { IsLocked = state.InitiallyLocked; OnLocked = state.OnLocked; OnUnlocked = state.OnUnlocked }
        ]
