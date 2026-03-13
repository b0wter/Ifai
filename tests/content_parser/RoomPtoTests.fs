module RoomPtoTests

open System.IO
open Ifai.ContentParser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Convert ContentLine list to RoomPto`` () =
    let input = """room:
  id: underground_lake
  name: "Cave entrance"
  exits:
    - north: rooms.cave_tunnel
    - south: rooms.storage_room
      via: decorations.underground_lake.door
    - behind rock, under rock, behind boulder, under boulder: rooms.secret_room
      via: decorations.underground_lake.giant_rock
  
  desc: |
    You stand at the entrance to a cave. The entrance is dimly lit, small plants and brush cover the rock.
    {item:sign}
    There is only one way forward. You feel like not being able to go back.
"""
    let room, _ = IndentationParser.parse input |> IndentationTokens.toRoomPto
    
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
let ``Convert whole underground_lake.ifa to RoomPto`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let _, unused = IndentationParser.parse content |> IndentationTokens.toRoomPto
    // The first unused line should be the 'item:' block start
    match unused |> List.head with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "item"
    | _ -> failwith "Expected item: block"
    

[<Fact>]
let ``toRoomPto works with actual file`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let input = File.ReadAllText(path)
    let lines = IndentationParser.parse input
    let room, unused = IndentationTokens.toRoomPto lines
    
    room.Id |> should equal "underground_lake"
    
    let expectedDesc = """You stand at the entrance to a cave. The entrance is dimly lit, small plants and brush cover the rock.
{item:sign}
There is only one way forward. You feel like not being able to go back."""
    room.Desc |> should equal expectedDesc
    
    
    // We expect item: (0), id: (2), name: (2), [empty line is skipped by parser], decoration: (0), id: (2)
    // Actually IndentationParser.parse skips empty lines.
    unused.Length |> should be (greaterThan 0)
    
    match unused[0] with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "item"
    | _ -> failwith "Expected item line"

    // Check that decoration is also there
    let decorationLine = unused |> List.find (function 
        | ContentLine.ComplexLine cl when cl.Key = "decoration" -> true 
        | _ -> false)
    match decorationLine with
    | ContentLine.ComplexLine cl -> cl.Indentation |> should equal 0u
    | _ -> failwith "Expected decoration line"    
    

[<Fact>]
let ``toRoomPto returns unused lines following the room definition`` () =
    let input = """room:
  id: first_room
  name: "First Room"
  desc: "A room"

item:
  id: some_item
  name: "Some Item"

decoration:
  id: some_decoration
"""
    let lines = IndentationParser.parse input
    let room, unused = IndentationTokens.toRoomPto lines
    
    room.Id |> should equal "first_room"
    
    // We expect item: (0), id: (2), name: (2), [empty line is skipped by parser], decoration: (0), id: (2)
    // Actually IndentationParser.parse skips empty lines.
    unused.Length |> should be (greaterThan 0)
    
    match unused[0] with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "item"
    | _ -> failwith "Expected item line"

    // Check that decoration is also there
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
    let lines = IndentationParser.parse input
    (fun () -> IndentationTokens.toRoomPto lines |> ignore) |> should throw typeof<System.Exception>
