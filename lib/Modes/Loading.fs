module Ifai.Lib.Modes.Loading

open Ifai.Lib

type LoadingState = {
    Filename: string option
}


let init (parameters: ToLoadingModeParameters) : LoadingState =
    { Filename = parameters.Filename }