module Ifai.Lib.Modes.Loading

open Ifai.Lib

(*
    Does it really make sense to create a loading mode?
    Quitting and restarting the game is almost instant and loading the world could
*)


type LoadingState = {
    Filename: string option
}


let init (parameters: ToLoadingModeParameters) : LoadingState =
    { Filename = parameters.Filename }