module RoomPtoTests

open Ifai.ContentParser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Convert ContentLine list to RoomPto`` () =
    let room, _ = TestHelper.readFragment "room_with_exits.ifa" |> LineParser.parse |> BlockParser.toRoomPto

    room.Exits[0].Direction |> should equal ["north"]
    room.Exits[0].Via |> should equal None

    room.Exits[1].Direction |> should equal ["south"]
    room.Exits[1].Via |> should equal (Some "decorations.underground_lake.door")

    room.Exits[2].Direction |> should equal ["behind rock"; "under rock"; "behind boulder"; "under boulder"]
    room.Exits[2].Via |> should equal (Some "decorations.underground_lake.giant_rock")

    let expectedDesc = """You stand at the entrance to a cave. The entrance is dimly lit, small plants and brush cover the rock.
{item:sign}
There is only one way forward. You feel like not being able to go back."""

    room.Desc.Replace("\r\n", "\n") |> should equal (expectedDesc.Replace("\r\n", "\n"))

[<Fact>]
let ``Convert whole in_front_of_house.ifa to RoomPto`` () =
    let _, unused = TestHelper.readTestData "in_front_of_house.ifa" |> LineParser.parse |> BlockParser.toRoomPto
    match unused |> List.head with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "decoration"
    | _ -> failwith "Expected decoration: block"


[<Fact>]
let ``toRoomPto works with actual file`` () =
    let lines = TestHelper.readTestData "in_front_of_house.ifa" |> LineParser.parse
    let room, _ = BlockParser.toRoomPto lines

    room.Id |> should equal "in_front_of_house"

    let expectedDesc = """Du stehst in einem adretten Garten vor einem alten Haus. Der Wind weht leicht durch zwei kleine Kirschbäume und kitzelt dich an der Nase.
Hinter dir, in südlicher Richtung, hörst du das Gartentor im Wind schlagen. Vor dir erhebt sich ein etwas in die Jahre gekommenes Haus.
Die Fenster sind liebevoll dekoriert, wenn auch ein wenig schmuddelig."""
    room.Desc |> should equal expectedDesc

    room.Exits.Length |> should equal 1
    room.Exits[0].Direction |> should contain "haustür"
    room.Exits[0].ToId |> should equal "rooms.hallway"
    room.Exits[0].Via |> should equal (Some "decorations.in_front_of_house.door")

    room.Desc.Contains("Du stehst in einem adretten Garten") |> should be True
    room.Variables.Length |> should equal 0


[<Fact>]
let ``toRoomPto returns unused lines following the room definition`` () =
    let lines = TestHelper.readFragment "room_followed_by_item_and_decoration.ifa" |> LineParser.parse
    let room, unused = BlockParser.toRoomPto lines

    room.Id |> should equal "first_room"

    unused.Length |> should be (greaterThan 0)

    match unused[0] with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "item"
    | _ -> failwith "Expected item line"

    let decorationLine = unused |> List.find (function
        | ContentLine.ComplexLine cl when cl.Key = "decoration" -> true
        | _ -> false)
    match decorationLine with
    | ContentLine.ComplexLine cl -> cl.Indentation |> should equal 0u
    | _ -> failwith "Expected decoration line"

[<Fact>]
let ``toRoomPto fails if first line is not room start`` () =
    let input = """
# Some comment
item:
  id: some_item

room:
  id: some_room
  name: "Some Room"
  desc: ""
"""
    let lines = LineParser.parse input
    (fun () -> BlockParser.toRoomPto lines |> ignore) |> should throw typeof<System.Exception>
