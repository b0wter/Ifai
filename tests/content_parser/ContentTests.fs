namespace ContentTests

open System.IO
open Ifai.ContentParser
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Lib.Shared
open Xunit
open FsUnit.Xunit

module ContentTests =
    let testRoomId = RoomId.create "in_front_of_house"
    let testLanguage = Language.create "en"
    let dummyAdventureContent =
        { Ifai.ContentParser.IndentationMapper.MappedContent.Rooms = []
          Ifai.ContentParser.IndentationMapper.MappedContent.Things = []
          Ifai.ContentParser.IndentationMapper.MappedContent.Adventure =
            Some { Language = testLanguage; Name = "Test Adventure"; InitialRoom = testRoomId } }

    [<Fact>]
    let ``parseFolder returns all content from TestData`` () =
        let folder = "TestData"
        let result = Content.parseFolder folder
        
        match result with
        | Ok contents ->
            contents.Length |> should be (greaterThan 1)
            let totalRooms = contents |> List.collect _.Rooms |> List.length
            totalRooms |> should be (greaterThan 1)
        | Error e -> failwith e

    [<Fact>]
    let ``createWorld creates a world with rooms and items from multiple contents`` () =
        let folder = "TestData"
        let contents =
            dummyAdventureContent
            :: (Content.parseFolder folder |> Result.defaultWith failwith)
        
        let initialRoomId = RoomId.create "in_front_of_house"
        let world = Content.createWorld contents
        world.CurrentRoomId |> should equal initialRoomId
        world.Rooms.Count |> should be (greaterThan 1)
        world.Things.Count |> should be (greaterThan 0)
        
        // Check that item locations are correctly set
        world.ThingLocations.Count |> should equal world.Things.Count
        
        // Check for specific room from underground_lake.ifa
        let lakeId = RoomId.create "underground_lake"
        world.Rooms.ContainsKey(lakeId) |> should be True
        
    [<Fact>]
    let ``createWorld applies room and item modifiers`` () =
        let folder = "TestData"
        let contents =
            dummyAdventureContent
            :: (Content.parseFolder folder |> Result.defaultWith failwith)
        
        let world = Content.createWorld contents
        // In in_front_of_house.ifa, there's a door_key with a 'discovered' modifier
        let doorKeyId = ThingId.create "items.door_key"
        world.ThingModifiers.ContainsKey(doorKeyId) |> should be True
        let modifiers = world.ThingModifiers[doorKeyId]
        modifiers |> Set.exists (function ThingModifier.Custom (AttributeId "discovered", AttributeValue.Bool false) -> true | _ -> false) |> should be True
