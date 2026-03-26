module ItemPtoTests

open System.IO
open Ifai.ContentParser
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Lib.Shared
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Convert ContentLine list to ItemPto`` () =
    let input = """item:
  synonyms: pebble, stone, small stone
  desc: |
    A pebble that nicely fits into your palm. Endless possibilities!
"""
    let lines = IndentationParser.parse input
    let item, unused = IndentationTokens.toItemPto lines
    
    item.Synonyms |> should equal ["pebble"; "stone"; "small stone"]
    item.Desc.Replace("\r\n", "\n") |> should equal "A pebble that nicely fits into your palm. Endless possibilities!"
    item.Category |> should equal ItemCategory.Item
    unused.Length |> should equal 0

[<Fact>]
let ``Convert ContentLine list to DecorationPto`` () =
    let input = """decoration:
  id: lake
  synonyms: lake, water
  desc: |
    A calm lake.
"""
    let lines = IndentationParser.parse input
    let item, unused = IndentationTokens.toItemPto lines
    
    item.Id |> should equal (Some "lake")
    item.Synonyms |> should equal ["lake"; "water"]
    item.Desc.Replace("\r\n", "\n") |> should equal "A calm lake."
    item.Category |> should equal ItemCategory.Decoration
    unused.Length |> should equal 0

[<Fact>]
let ``toItemPto returns unused lines following the item definition`` () =
    let input = """item:
  id: some_item
  synonyms: item
  desc: "An item"

decoration:
  id: some_decoration
"""
    let lines = IndentationParser.parse input
    let item, unused = IndentationTokens.toItemPto lines
    
    item.Id |> should equal (Some "some_item")
    
    unused.Length |> should be (greaterThan 0)
    match unused[0] with
    | ContentLine.ComplexLine cl -> cl.Key |> should equal "decoration"
    | _ -> failwith "Expected decoration line"

[<Fact>]
let ``toItemPto fails if first line is not item or decoration start`` () =
    let input = """room:
  id: some_room
"""
    let lines = IndentationParser.parse input
    (fun () -> IndentationTokens.toItemPto lines |> ignore) |> should throw typeof<System.Exception>


[<Fact>]
let ``Convert ItemPto with interactions`` () =
    let input = """item:
  synonyms: torch
  desc: |
    An ancient-looking torch
  interactions:
    - light:
        synonyms: light up, burn
        requires:
          - items.matches
        outcomes:
          - if: !isLit
            say: "The torch is lit"
            set: isLit = true
          - if: isLit
            say: "The torch is already lit"
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    item.Interactions.Length |> should equal 1
    let inter = item.Interactions[0]
    inter.Name |> should equal "light"
    inter.Synonyms |> should equal ["light up"; "burn"]
    inter.Requires |> should equal ["items.matches"]
    inter.Outcomes.Length |> should equal 2
    
    inter.Outcomes[0].If |> should equal "!isLit"
    inter.Outcomes[0].Actions.Length |> should equal 2
    inter.Outcomes[0].Actions[0].Operation |> should equal "say"
    inter.Outcomes[0].Actions[0].Arguments |> should equal "\"The torch is lit\""
    inter.Outcomes[0].Actions[1].Operation |> should equal "set"
    inter.Outcomes[0].Actions[1].Arguments |> should equal "isLit = true"
    
    inter.Outcomes[1].If |> should equal "isLit"
    inter.Outcomes[1].Actions.Length |> should equal 1
    inter.Outcomes[1].Actions[0].Operation |> should equal "say"
    inter.Outcomes[1].Actions[0].Arguments |> should equal "\"The torch is already lit\""


[<Fact>]
let ``Convert ItemPto with shorthand interactions`` () =
    let input = """item:
  synonyms: pebble
  desc: A pebble.
  interactions:
    - read:
        say: "The sign says: something"
    - kick, punch, strike:
        - if: !isDown
          say: "You kick the sign"
          set: isDown = true
        - if: isDown
          say: "The sign is already down"
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    item.Interactions.Length |> should equal 2
    
    let read = item.Interactions |> List.find (fun i -> i.Name = "read")
    read.Outcomes.Length |> should equal 1
    read.Outcomes[0].Actions[0].Operation |> should equal "say"
    read.Outcomes[0].Actions[0].Arguments |> should equal "\"The sign says: something\""
    
    let kick = item.Interactions |> List.find (fun i -> i.Name = "kick")
    kick.Synonyms |> should equal ["punch"; "strike"]
    kick.Outcomes.Length |> should equal 2
    kick.Outcomes[0].If |> should equal "!isDown"
    kick.Outcomes[1].If |> should equal "isDown"

[<Fact>]
let ``Convert ItemPto with complex interactions from underground_lake`` () =
    let input = """decoration:
  id: wooden_door
  interactions:
    - open:
        outcomes:
          - if: isLocked
            say: "The door is locked"
          - if: !isLocked && !isOpen
            say: "You open the door"
            set: isOpen = true
          - if: isOpen
            say: "The door is already open"
    - unlock:
        requires:
          - items.iron_key
        outcomes:
          - if: this.isLocked
            target: decorations.underground_lake.door
            set: this.isLocked = false
            say: The door has been unlocked.
          - if: !target.isLocked
            target: decorations.underground_lake.door
            say: The door is already unlocked
        otherwise:
          say: You cannot unlock the door without the key
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    let openInter = item.Interactions |> List.find (fun i -> i.Name = "open")
    openInter.Outcomes.Length |> should equal 3
    
    let unlockInter = item.Interactions |> List.find (fun i -> i.Name = "unlock")
    unlockInter.Requires |> should equal ["items.iron_key"]
    unlockInter.Outcomes.Length |> should equal 3
    unlockInter.Outcomes[2].If |> should equal "otherwise"
    unlockInter.Outcomes[2].Actions[0].Operation |> should equal "say"
    unlockInter.Outcomes[2].Actions[0].Arguments |> should equal "You cannot unlock the door without the key"

[<Fact>]
let ``Extract key item with interactions from in_front_of_house.ifa`` () =
    let path = Path.Combine("TestData", "in_front_of_house.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    // Recursive helper to find the doormat item
    let rec findDoormat l =
        match l with
        | [] -> failwith "Doormat not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "decoration" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Id = Some "doormat" then item else findDoormat rest
        | _ :: rest -> findDoormat rest
        
    let doormat = findDoormat lines
    
    doormat.Interactions.Length |> should equal 1
    let inter = doormat.Interactions[0]
    inter.Name |> should equal "unter die Matte gucken"
    inter.Synonyms |> should equal ["gucke unter die Matte"; "suche unter der Matte"; "suche unter Matte"; "schaue unter Matte"; "gucke unter Matte"]
    inter.Outcomes.Length |> should equal 1
    
    inter.Outcomes[0].Actions.Length |> should equal 5
    inter.Outcomes[0].Actions |> List.exists (fun a -> a.Operation = "say" && a.Arguments = "Unter der Matte befindet sich ein Schlüssel") |> should be True
    inter.Outcomes[0].Actions |> List.exists (fun a -> a.Operation = "remove" && a.Arguments = "") |> should be True

[<Fact>]
let ``Convert ItemPto with traits`` () =
    let input = """decoration:
  id: wooden_door
  traits:
    - door:
        isLocked: true
        isOpen: false
        key: items.iron_key, items.master_key
  desc: A wooden door.
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    item.Traits.Length |> should equal 1
    let t = item.Traits[0]
    t.Name |> should equal "door"
    t.Properties.Length |> should equal 3
    t.Properties |> List.find (fun (k, v) -> k = "isLocked") |> snd |> should equal "true"
    t.Properties |> List.find (fun (k, v) -> k = "isOpen") |> snd |> should equal "false"
    t.Properties |> List.find (fun (k, v) -> k = "key") |> snd |> should equal "items.iron_key, items.master_key"

[<Fact>]
let ``Convert ItemPto with multiple traits and no properties`` () =
    let input = """item:
  synonyms: rock
  traits:
    - heavy
    - throwable
  desc: A rock.
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    item.Traits.Length |> should equal 2
    item.Traits[0].Name |> should equal "heavy"
    item.Traits[0].Properties.Length |> should equal 0
    item.Traits[1].Name |> should equal "throwable"
    item.Traits[1].Properties.Length |> should equal 0

[<Fact>]
let ``Extract haustür with traits from in_front_of_house.ifa`` () =
    let path = Path.Combine("TestData", "in_front_of_house.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    let rec findDoor l =
        match l with
        | [] -> failwith "Door not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "decoration" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Synonyms |> List.contains "haustür" then item else findDoor rest
        | _ :: rest -> findDoor rest
        
    let door = findDoor lines
    
    door.Traits.Length |> should equal 1
    let t = door.Traits[0]
    t.Name |> should equal "door"
    t.Properties.Length |> should equal 3
    t.Properties |> List.exists (fun (k, _) -> k = "isLocked") |> should be True
    t.Properties |> List.exists (fun (k, _) -> k = "isOpen") |> should be True
    t.Properties |> List.exists (fun (k, _) -> k = "key") |> should be True

[<Fact>]
let ``Convert ItemPto with variables`` () =
    let input = """item:
  synonyms: torch
  modifiers:
    - isLit: false
    - lifetime: 123
      type: float
    - strength: 100
  desc: A torch.
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    item.Modifiers.Length |> should equal 3
    
    let isLit = item.Modifiers |> List.find (fun v -> v.Name = "isLit")
    isLit.Value |> should equal "false"
    isLit.Type |> should equal "bool"
    
    let lifetime = item.Modifiers |> List.find (fun v -> v.Name = "lifetime")
    lifetime.Value |> should equal "123"
    lifetime.Type |> should equal "float"
    
    let strength = item.Modifiers |> List.find (fun v -> v.Name = "strength")
    strength.Value |> should equal "100"
    strength.Type |> should equal "integer"

[<Fact>]
let ``Inferred types for variables`` () =
    let input = """item:
  synonyms: item
  modifiers:
    - someInt: 42
    - someFloat: 3.14
    - someString: "hello"
    - someBool: true
  desc: item
"""
    let lines = IndentationParser.parse input
    let item, _ = IndentationTokens.toItemPto lines
    
    let someInt = item.Modifiers |> List.find (fun v -> v.Name = "someInt")
    someInt.Type |> should equal "integer"
    
    let someFloat = item.Modifiers |> List.find (fun v -> v.Name = "someFloat")
    someFloat.Type |> should equal "float"
    
    let someString = item.Modifiers |> List.find (fun v -> v.Name = "someString")
    someString.Type |> should equal "string"
    
    let someBool = item.Modifiers |> List.find (fun v -> v.Name = "someBool")
    someBool.Type |> should equal "bool"

[<Fact>]
let ``Extract door_key with variables from in_front_of_house.ifa`` () =
    let path = Path.Combine("TestData", "in_front_of_house.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    let rec findKey l =
        match l with
        | [] -> failwith "Key not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "item" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Id = Some "door_key" then item else findKey rest
        | _ :: rest -> findKey rest
        
    let key = findKey lines
    
    key.Modifiers.Length |> should equal 2
    key.Modifiers |> List.exists (fun v -> v.Name = "coveredBy" && v.Value = "decorations.in_front_of_house.doormat") |> should be True
    key.Modifiers |> List.exists (fun v -> v.Name = "discovered" && v.Value = "false") |> should be True

[<Fact>]
let ``Extract second decoration from in_front_of_house.ifa`` () =
    let path = Path.Combine("TestData", "in_front_of_house.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    // First, consume the room
    let room, linesAfterRoom = IndentationTokens.toRoomPto lines
    room.Id |> should equal "in_front_of_house"
    
    // Next should be a decoration
    let item1, linesAfterItem1 = IndentationTokens.toItemPto linesAfterRoom
    item1.Synonyms |> should contain "Gartentor"
    
    // Skip some lines until the next decoration
    let rec findNextDecoration l =
        match l with
        | [] -> failwith "No more decorations"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "decoration" && cl.Indentation = 0u -> l
        | _ :: rest -> findNextDecoration rest
        
    let nextItemLines = findNextDecoration linesAfterItem1
    let item2, _ = IndentationTokens.toItemPto nextItemLines
    item2.Synonyms |> List.contains "Himmel" |> should be True
    item2.Desc.Contains("Graue Schlieren zieren den Himmel") |> should be True

[<Fact>]
let ``Extract all items and decorations from in_front_of_house.ifa`` () =
    let path = Path.Combine("TestData", "in_front_of_house.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    // First, consume the room
    let _, linesAfterRoom = IndentationTokens.toRoomPto lines
    
    // Recursive helper to extract all items/decorations
    let rec extractAll l acc =
        match l with
        | [] -> List.rev acc
        | ContentLine.ComplexLine cl :: _ when (cl.Key = "item" || cl.Key = "decoration") && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            extractAll rest (item :: acc)
        | _ :: rest -> extractAll rest acc
        
    let allPto = extractAll linesAfterRoom []
    
    // For each ItemPto, map it to a Thing and ThingModifier list
    let thingsWithModifiers = allPto |> List.map (IndentationMapper.mapItem "in_front_of_house")
    let things = thingsWithModifiers |> List.map fst
    
    things.Length |> should equal 10

    let findThingById id =
        let thingId = ThingId.create id
        match things |> List.tryFind (fun t -> t.Id = thingId) with
        | Some t -> t
        | None -> 
            let ids = things |> List.map _.Id.ToString() |> String.concat ", "
            failwithf "Could not find thing with id %s. Available ids: %s" id ids

    let findThingBySynonym synonym =
        match things |> List.tryFind (fun t -> t.Synonyms |> List.exists (fun s -> s.Text = synonym)) with
        | Some t -> t
        | None -> 
            let names = things |> List.map _.Name.Text |> String.concat ", "
            failwithf "Could not find thing with synonym %s. Available names: %s" synonym names

    let findModifiersByThing (thing: Thing) =
        let pair = thingsWithModifiers |> List.find (fun (t, _) -> t.Id = thing.Id)
        snd pair

    // 1. Gartentor (decoration)
    let tor = findThingBySynonym "Gartentor"
    tor.Id.ToString().Contains("decorations.in_front_of_house.Gartentor") |> should be True
    tor.IsPortable |> should be False
    tor.Synonyms |> List.map _.Text |> should contain "Gartentür"
    tor.Description.Text.Contains("Das Gartentor hängt ein wenig schief") |> should be True

    // 2. Himmel (decoration)
    let himmel = findThingBySynonym "Himmel"
    himmel.Id.ToString().Contains("decorations.in_front_of_house.Himmel") |> should be True
    himmel.Description.Text.Contains("Graue Schlieren zieren den Himmel") |> should be True

    // 3. Sonne (decoration)
    let sonne = findThingBySynonym "Sonne"
    sonne.Id.ToString().Contains("decorations.in_front_of_house.Sonne") |> should be True
    sonne.Description.Text.Contains("Die Sonne ist kaum hinter den Wolken zu erkennen") |> should be True

    // 4. weg (decoration)
    let weg = findThingBySynonym "weg"
    weg.Id.ToString().Contains("decorations.in_front_of_house.weg") |> should be True
    weg.Synonyms |> List.map _.Text |> should contain "gartenweg"
    weg.Description.Text.Contains("knirscht ein alter Schotterweg") |> should be True

    // 5. Garten (decoration)
    let garten = findThingBySynonym "Garten"
    garten.Id.ToString().Contains("decorations.in_front_of_house.Garten") |> should be True
    garten.Synonyms |> List.map _.Text |> should contain "Gras"
    garten.Description.Text.Contains("Pflege mit viel Elan begonnen") |> should be True

    // 6. baum (decoration)
    let baum = findThingBySynonym "baum"
    baum.Id.ToString().Contains("decorations.in_front_of_house.baum") |> should be True
    baum.Synonyms |> List.map _.Text |> should contain "kirschbäume"
    baum.Description.Text.Contains("Die Kirschbäume sind klein und knorrig") |> should be True

    // 7. haustür (decoration)
    let tuer = findThingBySynonym "haustür"
    tuer.Id.ToString().Contains("decorations.in_front_of_house.haustür") |> should be True
    tuer.Traits.Length |> should be (greaterThan 0)
    let isLocked = tuer.Traits |> List.choose (function Lockable l -> Some l.IsLocked | _ -> None) |> List.tryHead
    isLocked |> should equal (Some true)
    tuer.Description.Text.Contains("Die Haustür ist aus altem Holz") |> should be True

    // 8. doormat (decoration)
    let matte = findThingById "decorations.in_front_of_house.doormat"
    matte.Synonyms |> List.map _.Text |> should contain "Türmatte"
    matte.Description.Text.Contains("Die {{ if items.door_key.discovered }}schief vor der Tür liegende {{ end }}Türmatte hat schon bessere Zeiten gesehen") |> should be True
    matte.Traits |> List.exists (function Container _ -> true | _ -> false) |> should be True

    // 9. Klingel (decoration)
    let klingel = findThingBySynonym "Klingel"
    klingel.Id.ToString().Contains("decorations.in_front_of_house.Klingel") |> should be True
    let klingelMods = findModifiersByThing klingel
    klingelMods |> List.exists (function ThingModifier.Custom (AttributeId "counter", AttributeValue.Int (0, _, _)) -> true | _ -> false) |> should be True
    klingel.Description.Text.Contains("Eine einfache Türklingel hängt neben der Haustür") |> should be True

    // 10. door_key (item)
    let key = findThingById "items.door_key"
    key.IsPortable |> should be True
    key.Description.Text.Contains("Ein einfacher Schlüssel zum Öffnen der Haustür") |> should be True
    let keyMods = findModifiersByThing key
    keyMods |> List.exists (function ThingModifier.Custom (AttributeId "discovered", AttributeValue.Bool false) -> true | _ -> false) |> should be True
    keyMods |> List.exists (function ThingModifier.Custom (AttributeId "coveredBy", AttributeValue.String "decorations.in_front_of_house.doormat") -> true | _ -> false) |> should be True