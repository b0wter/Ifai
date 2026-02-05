module Ifai.Library.Tests.ParserTests

open System.Collections.Generic
open FsUnit.Xunit
open FsUnit.CustomMatchers
open Ifai.Lib
open Ifai.Lib.Modes
open Xunit

type EnglishTestData() =
    interface IEnumerable<obj[]> with
        member _.GetEnumerator() : IEnumerator<obj[]> =
            (seq {
                yield [| "n" :> obj; [ Exploring.ExploringBuiltIn.North ] :> obj |]
                yield [| "north" :> obj; [ Exploring.ExploringBuiltIn.North ] :> obj |]
                yield [| "ne" :> obj; [ Exploring.ExploringBuiltIn.NorthEast ] :> obj |]
                yield [| "northeast" :> obj; [ Exploring.ExploringBuiltIn.NorthEast ] :> obj |]
                yield [| "e" :> obj; [ Exploring.ExploringBuiltIn.East ] :> obj |]
                yield [| "east" :> obj; [ Exploring.ExploringBuiltIn.East ] :> obj |]
                yield [| "se" :> obj; [ Exploring.ExploringBuiltIn.SouthEast ] :> obj |]
                yield [| "southeast" :> obj; [ Exploring.ExploringBuiltIn.SouthEast ] :> obj |]
                yield [| "s" :> obj; [ Exploring.ExploringBuiltIn.South ] :> obj |]
                yield [| "south" :> obj; [ Exploring.ExploringBuiltIn.South ] :> obj |]
                yield [| "sw" :> obj; [ Exploring.ExploringBuiltIn.SouthWest ] :> obj |]
                yield [| "southwest" :> obj; [ Exploring.ExploringBuiltIn.SouthWest ] :> obj |]
                yield [| "w" :> obj; [ Exploring.ExploringBuiltIn.West ] :> obj |]
                yield [| "west" :> obj; [ Exploring.ExploringBuiltIn.West ] :> obj |]
                yield [| "nw" :> obj; [ Exploring.ExploringBuiltIn.NorthWest ] :> obj |]
                yield [| "northwest" :> obj; [ Exploring.ExploringBuiltIn.NorthWest ] :> obj |]
                
                yield [| "u" :> obj; [ Exploring.ExploringBuiltIn.Up ] :> obj |]
                yield [| "up" :> obj; [ Exploring.ExploringBuiltIn.Up ] :> obj |]
                yield [| "d" :> obj; [ Exploring.ExploringBuiltIn.Down ] :> obj |]
                yield [| "down" :> obj; [ Exploring.ExploringBuiltIn.Down ] :> obj |]
                yield [| "l" :> obj; [ Exploring.ExploringBuiltIn.Left; Exploring.ExploringBuiltIn.LookAround ] :> obj |]
                yield [| "left" :> obj; [ Exploring.ExploringBuiltIn.Left ] :> obj |]
                yield [| "r" :> obj; [ Exploring.ExploringBuiltIn.Right ] :> obj |]
                yield [| "right" :> obj; [ Exploring.ExploringBuiltIn.Right ] :> obj |]
                
                yield [| "look" :> obj; [ Exploring.ExploringBuiltIn.LookAround ] :> obj |]
                yield [| "x item" :> obj; [ Exploring.ExploringBuiltIn.Examine "item" ] :> obj |]
                yield [| "examine item" :> obj; [ Exploring.ExploringBuiltIn.Examine "item" ] :> obj |]
                yield [| "t item" :> obj; [ Exploring.ExploringBuiltIn.Take "item" ] :> obj |]
                yield [| "take item" :> obj; [ Exploring.ExploringBuiltIn.Take "item" ] :> obj |]
                // TODO: This will also be matched as `Take` with the argument `all`
                yield [| "take all" :> obj; [ Exploring.ExploringBuiltIn.Take "all";  Exploring.ExploringBuiltIn.TakeAll ] :> obj |]
                yield [| "get all" :> obj; [ Exploring.ExploringBuiltIn.TakeAll ] :> obj |]
                
            }).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (this :> IEnumerable<obj[]>).GetEnumerator() :> System.Collections.IEnumerator


[<Theory>]
[<ClassData(typeof<EnglishTestData>)>]
let ``Parsing the basic english built-ins returns the properly parsed built-ins`` (input: string, expected: Exploring.ExploringBuiltIn list) =
    let parser = Parser.tryParseBuiltIn Exploring.builtIns (Language.create "en")
    
    let result = input |> parser
    
    result |> should equal expected