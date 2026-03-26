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
    let ``Map entire in_front_of_house.ifa file`` () =
        let path = Path.Combine("TestData", "in_front_of_house.ifa")
        let content = File.ReadAllText(path)
        let lines = IndentationParser.parse content
        
        let mapped = IndentationMapper.mapFullContent lines
        
        // 1. Check Rooms
        mapped.Rooms.Length |> should equal 1
        let room, _ = mapped.Rooms[0]
        room.Id |> RoomId.value |> should equal "in_front_of_house"
        room.Name.Text |> should equal "Vor dem Haus"
        
        // 2. Check Connections
        // in_front_of_house.ifa has 1 exit block: haustür: rooms.hallway
        room.Connections.Count |> should equal 1
        
        // haustür exit
        let hausTuerExit = Ifai.Lib.Exit.Custom (Text.create (TextKey.create "haustür"))
        room.Connections.ContainsKey(hausTuerExit) |> should be True
        let hausTuerConn = room.Connections[hausTuerExit]
        hausTuerConn.ToId |> RoomId.value |> should equal "rooms.hallway"
        
        // 3. Check Things
        mapped.Things.Length |> should equal 10
        
        let findThingBySynonym synonym =
            match mapped.Things |> List.tryFind (fun (t, _, _) -> t.Synonyms |> List.exists (fun s -> s.Text = synonym)) with
            | Some (t, m, l) -> (t, m, l)
            | None -> 
                let names = mapped.Things |> List.map (fun (t, _, _) -> t.Name.Text) |> String.concat ", "
                failwithf "Could not find thing with synonym %s. Available names: %s" synonym names

        let findThingById id =
            let thingId = ThingId.create id
            match mapped.Things |> List.tryFind (fun (t, _, _) -> t.Id = thingId) with
            | Some (t, m, l) -> (t, m, l)
            | None -> 
                let ids = mapped.Things |> List.map (fun (t, _, _) -> t.Id |> ThingId.value) |> String.concat ", "
                failwithf "Could not find thing with id %s. Available ids: %s" id ids

        // Himmel
        let himmel, _, _ = findThingBySynonym "Himmel"
        himmel.IsPortable |> should be False
        himmel.Synonyms |> List.map _.Text |> should contain "Himmel"
        
        // Doormat
        let doormat, _, _ = findThingById "decorations.in_front_of_house.doormat"
        doormat.IsPortable |> should be False
        doormat.Synonyms |> List.map _.Text |> should contain "Türmatte"
        
        // Haustür
        let tuer, _, _ = findThingBySynonym "haustür"
        tuer.Traits.Length |> should equal 2 // Openable and Lockable
        let isLocked = tuer.Traits |> List.choose (function Lockable l -> Some l.IsLocked | _ -> None) |> List.tryHead
        isLocked |> should equal (Some true)

    [<Fact>]
    let ``Map room with modifiers to RoomModifiers`` () =
        let content = """
room:
  id: cave
  name: "Dark Cave"
  modifiers:
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
