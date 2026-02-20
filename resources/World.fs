module Ifai.Resources.World

open System.Collections.Generic
open Ifai.Lib
open Ifai.Lib.Modes

let init (rooms: IEnumerable<Room>) firstRoomId language textResources =
    {
        World = World.init (rooms |> List.ofSeq) firstRoomId []
        GameMode = { Exploring.ExploringState.Foo = 0 } |> GameMode.Exploring |> List.singleton
        Language = language
        TextResources = textResources
    }