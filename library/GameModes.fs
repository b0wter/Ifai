module Ifai.Library.GameModes

type GameMode =
    | Exploring
    | AwaitingOverrideSaveFile of filename:string
