namespace IndentationMapperTests

open System.IO
open Ifai.ContentParser
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Lib.Shared
open Xunit
open FsUnit.Xunit

module IndentationMapperTests =

    [<Fact>]
    let ``Map entire underground_lake.ifa file`` () =
        let path = Path.Combine("TestData", "underground_lake.ifa")
        let content = File.ReadAllText(path)
        let lines = IndentationParser.parse content
        
        let mapped = IndentationMapper.mapFullContent lines
        
        // 1. Check Rooms
        mapped.Rooms.Length |> should equal 1
        let room, _ = mapped.Rooms[0]
        room.Id |> RoomId.value |> should equal "underground_lake"
        room.Name.Text |> should equal "Cave entrance"
        
        // 2. Check Connections
        // underground_lake.ifa has 3 exit blocks, but "behind rock, under rock, ..." has 4 directions.
        // Total unique Exits: north (1), south (1), behind rock (1), under rock (1), behind boulder (1), under boulder (1) = 6
        room.Connections.Count |> should equal 6
        
        // North exit
        let northExit = Dir Direction.North
        room.Connections.ContainsKey(northExit) |> should be True
        let northConn = room.Connections[northExit]
        northConn.ToId |> RoomId.value |> should equal "rooms.cave_tunnel"
        
        // South exit
        let southExit = Dir Direction.South
        room.Connections.ContainsKey(southExit) |> should be True
        let southConn = room.Connections[southExit]
        southConn.ToId |> RoomId.value |> should equal "rooms.storage_room"
        
        // Custom exit "behind rock"
        let behindRock = Ifai.Lib.Exit.Custom (Text.create (TextKey.create "behind rock"))
        room.Connections.ContainsKey(behindRock) |> should be True
        let behindRockConn = room.Connections[behindRock]
        behindRockConn.ToId |> RoomId.value |> should equal "rooms.secret_room"
        
        // 3. Check Things
        mapped.Things.Length |> should equal 10
        
        let findThingBySynonym synonym =
            match mapped.Things |> List.tryFind (fun (t, _) -> t.Synonyms |> List.exists (fun s -> s.Text = synonym)) with
            | Some (t, m) -> (t, m)
            | None -> 
                let names = mapped.Things |> List.map (fst >> _.Name >> _.Text) |> String.concat ", "
                failwithf "Could not find thing with synonym %s. Available names: %s" synonym names

        let findThingById id =
            let thingId = ThingId.create id
            match mapped.Things |> List.tryFind (fun (t, _) -> t.Id = thingId) with
            | Some (t, m) -> (t, m)
            | None -> 
                let ids = mapped.Things |> List.map (fst >> _.Id >> ThingId.value) |> String.concat ", "
                failwithf "Could not find thing with id %s. Available ids: %s" id ids

        // Pebble
        let pebble, _ = findThingBySynonym "pebble"
        pebble.IsPortable |> should be True
        pebble.Synonyms |> List.map _.Text |> should contain "pebble"
        
        // Sign
        let sign, signMods = findThingById "decorations.underground_lake.sign"
        sign.IsPortable |> should be False
        signMods |> List.exists (function ThingModifier.Custom (AttributeId "isDown", AttributeValue.Bool false) -> true | _ -> false) |> should be True
        signMods |> List.exists (function ThingModifier.Custom (AttributeId "text", AttributeValue.String "\"The lake has strong underwater currents\"") -> true | _ -> false) |> should be True
        
        // Door
        let door, _ = findThingById "decorations.underground_lake.wooden_door"
        door.Traits.Length |> should equal 2 // Openable and Lockable
        let isLocked = door.Traits |> List.choose (function Lockable l -> Some l.IsLocked | _ -> None) |> List.tryHead
        isLocked |> should equal (Some true)

    [<Fact>]
    let ``Map room with variables to RoomModifiers`` () =
        let content = """
room:
  id: cave
  name: "Dark Cave"
  variables:
    - isDark: true
    - temperature: 10
      type: int
  desc: |
    A very dark cave.
"""
        let lines = IndentationParser.parse content
        let mapped = IndentationMapper.mapFullContent lines
        
        mapped.Rooms.Length |> should equal 1
        let _, mods = mapped.Rooms[0]
        
        mods.Length |> should equal 2
        mods |> List.exists (function RoomModifier.Custom (AttributeId "isDark", AttributeValue.Bool true) -> true | _ -> false) |> should be True
        mods |> List.exists (function RoomModifier.Custom (AttributeId "temperature", AttributeValue.Int (10, _, _)) -> true | _ -> false) |> should be True
