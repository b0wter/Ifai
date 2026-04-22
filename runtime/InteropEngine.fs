module Ifai.Runtime.Interop.Engine

open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Runtime
open Ifai.Runtime.Interop.Mappers
open Ifai.Runtime.Interop.Types


let run (fileIo: IFileIO) (model: Model) : Engine =
    Engine.run fileIo model
    |> wrapEngine


let initializeModel (world: World) (language: string) (texts: TextResources) : Model =
    Engine.initializeModel world language texts
