module Ifai.Library.Tests.WorldTests

open Xunit
open FsUnit.Xunit

open Ifai.Lib
open Ifai.Lib.Content


// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let private localizedText text : Text.LocalizedText =
    { Text = text
      NarrativeStyle = NarrativeStyle.Regular
      Parameters = None
      ParameterFormatting = None }

let private mkRoom id name connections =
    { Room.create (RoomId.create id) (localizedText name) (localizedText $"%s{name} description")
        with Connections = connections }

let private mkConnection exitDir toId =
    Connection.create (Dir exitDir) (RoomId.create toId)

let private mkWorld rooms initialRoomId things thingLocations =
    World.init rooms (RoomId.create initialRoomId) things thingLocations

let private mkThing id name =
    { Id = ThingId.create id
      Name = localizedText name
      Description = localizedText $"%s{name} description"
      Synonyms = []
      IsPortable = true
      Weight = 1u
      IsAbstract = false
      Interactability = Interactability.All
      LegalOwner = LegalOwner.Nobody
      Traits = []
      NeedsDiscovery = false }

/// Two rooms connected North/South: roomA --North--> roomB
let private twoRoomWorld () =
    let connAtoB = [ (Dir Direction.North, mkConnection Direction.North "roomB") ] |> Map.ofList
    let connBtoA = [ (Dir Direction.South, mkConnection Direction.South "roomA") ] |> Map.ofList
    let roomA = mkRoom "roomA" "Room A" connAtoB
    let roomB = mkRoom "roomB" "Room B" connBtoA
    mkWorld [ roomA; roomB ] "roomA" [] Map.empty


// ---------------------------------------------------------------------------
// World.init
// ---------------------------------------------------------------------------

[<Fact>]
let ``init sets turn to zero`` () =
    let world = twoRoomWorld ()
    world.Turn |> should equal 0u

[<Fact>]
let ``init sets current room to initial room`` () =
    let world = twoRoomWorld ()
    world.CurrentRoomId |> should equal (RoomId.create "roomA")

[<Fact>]
let ``init indexes rooms by id`` () =
    let world = twoRoomWorld ()
    world.Rooms |> Map.count |> should equal 2
    world.Rooms |> Map.containsKey (RoomId.create "roomA") |> should be True
    world.Rooms |> Map.containsKey (RoomId.create "roomB") |> should be True

[<Fact>]
let ``init assigns Nowhere to things without explicit location`` () =
    let thing = mkThing "sword" "Sword"
    let world = mkWorld [] "roomA" [ thing ] Map.empty
    world.ThingLocations |> Map.find (ThingId.create "sword") |> should equal ThingLocation.Nowhere

[<Fact>]
let ``init preserves explicit thing locations`` () =
    let thing = mkThing "sword" "Sword"
    let locations = [ (ThingId.create "sword", ThingLocation.Player) ] |> Map.ofList
    let room = mkRoom "roomA" "Room A" Map.empty
    let world = mkWorld [ room ] "roomA" [ thing ] locations
    world.ThingLocations |> Map.find (ThingId.create "sword") |> should equal ThingLocation.Player

[<Fact>]
let ``init with mixed locations assigns Nowhere only to unlocated things`` () =
    let sword = mkThing "sword" "Sword"
    let shield = mkThing "shield" "Shield"
    let locations = [ (ThingId.create "sword", ThingLocation.Player) ] |> Map.ofList
    let room = mkRoom "roomA" "Room A" Map.empty
    let world = mkWorld [ room ] "roomA" [ sword; shield ] locations
    world.ThingLocations |> Map.find (ThingId.create "sword") |> should equal ThingLocation.Player
    world.ThingLocations |> Map.find (ThingId.create "shield") |> should equal ThingLocation.Nowhere


// ---------------------------------------------------------------------------
// World.getRoomById
// ---------------------------------------------------------------------------

[<Fact>]
let ``getRoomById with existing id returns Some`` () =
    let world = twoRoomWorld ()
    world |> World.getRoomById (RoomId.create "roomA") |> Option.isSome |> should be True

[<Fact>]
let ``getRoomById with missing id returns None`` () =
    let world = twoRoomWorld ()
    world |> World.getRoomById (RoomId.create "noSuchRoom") |> Option.isNone |> should be True


// ---------------------------------------------------------------------------
// World.doesRoomIdExist / doesExitExist
// ---------------------------------------------------------------------------

[<Fact>]
let ``doesRoomIdExist returns true for existing room`` () =
    let world = twoRoomWorld ()
    world |> World.doesRoomIdExist (RoomId.create "roomA") |> should be True

[<Fact>]
let ``doesRoomIdExist returns false for missing room`` () =
    let world = twoRoomWorld ()
    world |> World.doesRoomIdExist (RoomId.create "nope") |> should be False

[<Fact>]
let ``doesExitExist returns true for existing exit`` () =
    let world = twoRoomWorld ()
    world |> World.doesExitExist (Dir Direction.North) (RoomId.create "roomA") |> should be True

[<Fact>]
let ``doesExitExist returns false for non-existing exit`` () =
    let world = twoRoomWorld ()
    world |> World.doesExitExist (Dir Direction.East) (RoomId.create "roomA") |> should be False

[<Fact>]
let ``doesExitExist returns false for non-existing room`` () =
    let world = twoRoomWorld ()
    world |> World.doesExitExist (Dir Direction.North) (RoomId.create "nope") |> should be False


// ---------------------------------------------------------------------------
// World.getConnectionForExit
// ---------------------------------------------------------------------------

[<Fact>]
let ``getConnectionForExit returns connection for valid exit from current room`` () =
    let world = twoRoomWorld ()
    let conn = world |> World.getConnectionForExit (Dir Direction.North)
    conn |> Option.isSome |> should be True
    conn.Value.ToId |> should equal (RoomId.create "roomB")

[<Fact>]
let ``getConnectionForExit returns None for invalid exit`` () =
    let world = twoRoomWorld ()
    world |> World.getConnectionForExit (Dir Direction.West) |> Option.isNone |> should be True


// ---------------------------------------------------------------------------
// World.getExitsByRoomId
// ---------------------------------------------------------------------------

[<Fact>]
let ``getExitsByRoomId returns exits for existing room`` () =
    let world = twoRoomWorld ()
    let exits = world |> World.getExitsByRoomId (RoomId.create "roomA")
    exits |> Option.isSome |> should be True
    exits.Value |> should contain (Dir Direction.North)

[<Fact>]
let ``getExitsByRoomId returns None for missing room`` () =
    let world = twoRoomWorld ()
    world |> World.getExitsByRoomId (RoomId.create "nope") |> Option.isNone |> should be True


// ---------------------------------------------------------------------------
// World.tryMoveTo
// ---------------------------------------------------------------------------

[<Fact>]
let ``tryMoveTo valid exit returns true and updates current room`` () =
    let world = twoRoomWorld ()
    let (success, newWorld) = world |> World.tryMoveTo true (Dir Direction.North)
    success |> should be True
    newWorld.CurrentRoomId |> should equal (RoomId.create "roomB")

[<Fact>]
let ``tryMoveTo valid exit with turn increment increases turn`` () =
    let world = twoRoomWorld ()
    let (_, newWorld) = world |> World.tryMoveTo true (Dir Direction.North)
    newWorld.Turn |> should equal 1u

[<Fact>]
let ``tryMoveTo valid exit without turn increment keeps turn`` () =
    let world = twoRoomWorld ()
    let (_, newWorld) = world |> World.tryMoveTo false (Dir Direction.North)
    newWorld.Turn |> should equal 0u

[<Fact>]
let ``tryMoveTo invalid exit returns false and unchanged world`` () =
    let world = twoRoomWorld ()
    let (success, newWorld) = world |> World.tryMoveTo true (Dir Direction.East)
    success |> should be False
    newWorld.CurrentRoomId |> should equal (RoomId.create "roomA")
    newWorld.Turn |> should equal 0u

[<Fact>]
let ``tryMoveTo and back returns to original room`` () =
    let world = twoRoomWorld ()
    let (_, afterNorth) = world |> World.tryMoveTo true (Dir Direction.North)
    let (success, afterSouth) = afterNorth |> World.tryMoveTo true (Dir Direction.South)
    success |> should be True
    afterSouth.CurrentRoomId |> should equal (RoomId.create "roomA")
    afterSouth.Turn |> should equal 2u


// ---------------------------------------------------------------------------
// World.currentRoom / setCurrentRoom / setCurrentRoomId
// ---------------------------------------------------------------------------

[<Fact>]
let ``currentRoom returns the room matching CurrentRoomId`` () =
    let world = twoRoomWorld ()
    let room = world |> World.currentRoom
    room.Id |> should equal (RoomId.create "roomA")

[<Fact>]
let ``setCurrentRoomId changes current room`` () =
    let world = twoRoomWorld ()
    let updated = world |> World.setCurrentRoomId (RoomId.create "roomB")
    updated.CurrentRoomId |> should equal (RoomId.create "roomB")

[<Fact>]
let ``setCurrentRoom changes current room by room record`` () =
    let world = twoRoomWorld ()
    let roomB = world.Rooms |> Map.find (RoomId.create "roomB")
    let updated = world |> World.setCurrentRoom roomB
    updated.CurrentRoomId |> should equal (RoomId.create "roomB")
