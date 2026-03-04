namespace Ifai.Library.Tests

open Xunit
open FsUnit.Xunit
open System.IO

(*
module IfaParserTests =

    let ifaContent = File.ReadAllText "/home/b0wter/work/ifai/resources/Data/cave_underground_lake.ifa"

    [<Fact>]
    let ``Parse room correctly`` () =
        let dtos = IfaParser.parse ifaContent
        let rooms = dtos |> List.choose (function Room r -> Some r | _ -> None)
        rooms.Length |> should equal 1
        let room = rooms.[0]
        room.Id |> should equal "underground_lake"
        room.Name |> should equal "Cave entrance"
        room.Description.Contains "You stand at the entrance to a cave." |> should be True

    [<Fact>]
    let ``Parse items correctly`` () =
        let dtos = IfaParser.parse ifaContent
        let items = dtos |> List.choose (function Item i -> Some i | _ -> None)
        items.Length |> should equal 1
        let item = items.[0]
        item.Synonyms |> should contain "pebble"
        item.Description.Contains "A pebble that nicely fits into your palm." |> should be True
        item.Interactions.Length |> should equal 2

    [<Fact>]
    let ``Parse decorations correctly`` () =
        let dtos = IfaParser.parse ifaContent
        let decorations = dtos |> List.choose (function Decoration d -> Some d | _ -> None)
        decorations.Length |> should equal 3
        
        let sign = decorations |> List.find (fun d -> d.Id = Some "sign")
        sign.State.ContainsKey "isDown" |> should be True
        sign.Actions.Length |> should equal 3
*)