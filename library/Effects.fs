module Ifai.Library.Effects

type Effect =
    | Print of string       
    | SaveGame of filename:string
    | ExitGame
    // there is no batch for effect since a command describes an effect
    // and commands also offer batching 
        