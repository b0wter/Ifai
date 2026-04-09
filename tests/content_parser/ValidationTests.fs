module ValidationTests

open FsUnit.CustomMatchers
open Ifai.ContentParser.Validation
open Ifai.ContentParser.DomainMapper
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Lib.Content.Thing
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``validateIdUniqueness returns Ok when all IDs are unique`` () =
    let room1 = { Id = RoomId.create "room1"
                  Name = toLocalizedText "Room 1"
                  Description = toLocalizedText "A room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let room2 = { Id = RoomId.create "room2"
                  Name = toLocalizedText "Room 2"
                  Description = toLocalizedText "Another room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let thing1 = { Id = ThingId.create "thing1"
                   Name = toLocalizedText "Thing 1"
                   Synonyms = [toLocalizedText "thing"]
                   Description = toLocalizedText "A thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let thing2 = { Id = ThingId.create "thing2"
                   Name = toLocalizedText "Thing 2"
                   Synonyms = [toLocalizedText "item"]
                   Description = toLocalizedText "Another thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let content = { Adventure = None
                    Rooms = [(room1, []); (room2, [])]
                    Things = [(thing1, [], ThingLocation.Nowhere); (thing2, [], ThingLocation.Nowhere)] }

    let result = validateIdUniqueness content
    result |> should be (ofCase <@ Result<unit, string list>.Ok () @>)


[<Fact>]
let ``validateIdUniqueness returns Error when room IDs are duplicated`` () =
    let room1 = { Id = RoomId.create "duplicate_room"
                  Name = toLocalizedText "Room 1"
                  Description = toLocalizedText "A room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let room2 = { Id = RoomId.create "duplicate_room"
                  Name = toLocalizedText "Room 2"
                  Description = toLocalizedText "Another room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let content = { Adventure = None
                    Rooms = [(room1, []); (room2, [])]
                    Things = [] }

    let result = validateIdUniqueness content
    match result with
    | Error errors ->
        errors |> should haveLength 1
        errors |> should contain "Duplicate id: duplicate_room"
    | Ok _ -> failwith "Expected Error but got Ok"

[<Fact>]
let ``validateIdUniqueness returns Error when thing IDs are duplicated`` () =
    let thing1 = { Id = ThingId.create "duplicate_thing"
                   Name = toLocalizedText "Thing 1"
                   Synonyms = [toLocalizedText "thing"]
                   Description = toLocalizedText "A thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let thing2 = { Id = ThingId.create "duplicate_thing"
                   Name = toLocalizedText "Thing 2"
                   Synonyms = [toLocalizedText "item"]
                   Description = toLocalizedText "Another thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let content = { Adventure = None
                    Rooms = []
                    Things = [(thing1, [], ThingLocation.Nowhere); (thing2, [], ThingLocation.Nowhere)] }

    let result = validateIdUniqueness content
    match result with
    | Error errors ->
        errors |> should haveLength 1
        errors |> should contain "Duplicate id: duplicate_thing"
    | Ok _ -> failwith "Expected Error but got Ok"

[<Fact>]
let ``validateIdUniqueness returns Error when room and thing IDs collide`` () =
    let room = { Id = RoomId.create "conflicting_id"
                 Name = toLocalizedText "Room"
                 Description = toLocalizedText "A room"
                 Connections = Map.empty
                 OnEnter = RoomEvent.Nothing
                 OnLeaving = RoomEvent.Nothing
                 Environment = RoomEnvironment.``default`` }

    let thing = { Id = ThingId.create "conflicting_id"
                  Name = toLocalizedText "Thing"
                  Synonyms = [toLocalizedText "thing"]
                  Description = toLocalizedText "A thing"
                  IsAbstract = false
                  IsPortable = true
                  LegalOwner = LegalOwner.Nobody
                  Traits = []
                  Interactability = Interactability.All
                  NeedsDiscovery = false
                  Weight = 0u }

    let content = { Adventure = None
                    Rooms = [(room, [])]
                    Things = [(thing, [], ThingLocation.Nowhere)] }

    let result = validateIdUniqueness content
    match result with
    | Error errors ->
        errors |> should haveLength 1
        errors |> should contain "Duplicate id: conflicting_id"
    | Ok _ -> failwith "Expected Error but got Ok"

[<Fact>]
let ``validateIdUniqueness returns all duplicate IDs`` () =
    let room1 = { Id = RoomId.create "dup1"
                  Name = toLocalizedText "Room 1"
                  Description = toLocalizedText "A room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let room2 = { Id = RoomId.create "dup1"
                  Name = toLocalizedText "Room 2"
                  Description = toLocalizedText "Another room"
                  Connections = Map.empty
                  OnEnter = RoomEvent.Nothing
                  OnLeaving = RoomEvent.Nothing
                  Environment = RoomEnvironment.``default`` }

    let thing1 = { Id = ThingId.create "dup2"
                   Name = toLocalizedText "Thing 1"
                   Synonyms = [toLocalizedText "thing"]
                   Description = toLocalizedText "A thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let thing2 = { Id = ThingId.create "dup2"
                   Name = toLocalizedText "Thing 2"
                   Synonyms = [toLocalizedText "item"]
                   Description = toLocalizedText "Another thing"
                   IsAbstract = false
                   IsPortable = true
                   LegalOwner = LegalOwner.Nobody
                   Traits = []
                   Interactability = Interactability.All
                   NeedsDiscovery = false
                   Weight = 0u }

    let content = { Adventure = None
                    Rooms = [(room1, []); (room2, [])]
                    Things = [(thing1, [], ThingLocation.Nowhere); (thing2, [], ThingLocation.Nowhere)] }

    let result = validateIdUniqueness content
    match result with
    | Error errors ->
        errors |> should haveLength 2
        errors |> should contain "Duplicate id: dup1"
        errors |> should contain "Duplicate id: dup2"
    | Ok _ -> failwith "Expected Error but got Ok"

[<Fact>]
let ``validateIdUniqueness returns Ok for empty content`` () =
    let content = { Adventure = None; Rooms = []; Things = [] }

    let result = validateIdUniqueness content
    match result with
    | Ok _ -> ()
    | Error errors -> failwith $"Expected Ok but got Error: %A{errors}"

[<Fact>]
let ``validateThing finds all parameters`` () =
    let input = """
        You stand at the entrance to a cave. The entrance is dimly lit, small plants and brush cover the rock.
        {item:sign}
        There is only one way forward. You feel like not being able to go back."""
    let thing = { Id = ThingId.create "dup1"
                  Name = toLocalizedText "Thing 1"
                  Synonyms = [toLocalizedText "item"]
                  Description = toLocalizedText input
                  IsAbstract = false
                  IsPortable = true
                  LegalOwner = LegalOwner.Nobody
                  Traits = []
                  Interactability = Interactability.All
                  NeedsDiscovery = false
                  Weight = 0u }

    (thing, [], ThingLocation.Nowhere) |> validateThing |> should be (ofCase <@ Result<unit, string list>.Ok @>)
