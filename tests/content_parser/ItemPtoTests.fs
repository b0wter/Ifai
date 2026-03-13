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
let ``Extract key item with interactions from underground_lake.ifa`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    // Recursive helper to find the pebble item
    let rec findPebble l =
        match l with
        | [] -> failwith "Pebble not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "item" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Synonyms |> List.contains "pebble" then item else findPebble rest
        | _ :: rest -> findPebble rest
        
    let pebble = findPebble lines
    
    pebble.Interactions.Length |> should equal 1
    let inter = pebble.Interactions[0]
    inter.Name |> should equal "throw"
    inter.Synonyms |> should equal ["toss"]
    inter.Outcomes.Length |> should equal 3
    
    // First outcome: target: decorations.underground_lake.lake
    inter.Outcomes[0].Actions.Length |> should equal 4
    inter.Outcomes[0].Actions |> List.exists (fun a -> a.Operation = "target" && a.Arguments = "decorations.underground_lake.lake") |> should be True
    inter.Outcomes[0].Actions |> List.exists (fun a -> a.Operation = "destroy" && a.Arguments = "this") |> should be True

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
let ``Extract wooden_door with traits from underground_lake.ifa`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    let rec findDoor l =
        match l with
        | [] -> failwith "Door not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "decoration" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Id = Some "wooden_door" then item else findDoor rest
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
let ``Extract sign with variables from underground_lake.ifa`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    let rec findSign l =
        match l with
        | [] -> failwith "Sign not found"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "decoration" && cl.Indentation = 0u ->
            let item, rest = IndentationTokens.toItemPto l
            if item.Id = Some "sign" then item else findSign rest
        | _ :: rest -> findSign rest
        
    let sign = findSign lines
    
    sign.Modifiers.Length |> should equal 3
    sign.Modifiers |> List.exists (fun v -> v.Name = "isDown" && v.Value = "false") |> should be True
    sign.Modifiers |> List.exists (fun v -> v.Name = "isBroken" && v.Value = "false") |> should be True
    sign.Modifiers |> List.exists (fun v -> v.Name = "text" && v.Value = "\"The lake has strong underwater currents\"") |> should be True

[<Fact>]
let ``Extract second item from underground_lake.ifa`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let lines = IndentationParser.parse content
    
    // First, consume the room
    let room, linesAfterRoom = IndentationTokens.toRoomPto lines
    room.Id |> should equal "underground_lake"
    
    // Next should be an item
    let item1, linesAfterItem1 = IndentationTokens.toItemPto linesAfterRoom
    item1.Synonyms |> should contain "pebble"
    
    // Skip some lines until the next item
    let rec findNextItem l =
        match l with
        | [] -> failwith "No more items"
        | ContentLine.ComplexLine cl :: _ when cl.Key = "item" && cl.Indentation = 0u -> l
        | _ :: rest -> findNextItem rest
        
    let nextItemLines = findNextItem linesAfterItem1
    let item2, _ = IndentationTokens.toItemPto nextItemLines
    item2.Synonyms |> List.contains "key" |> should be True
    item2.Desc.Contains("A small iron key") |> should be True

[<Fact>]
let ``Extract all items and decorations from underground_lake.ifa`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
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
    let thingsWithModifiers = allPto |> List.map (IndentationMapper.mapItem "underground_lake")
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

    // 1. pebble (item)
    let pebble = findThingBySynonym "pebble"
    pebble.IsPortable |> should be True
    pebble.Synonyms |> List.map _.Text |> should contain "pebble"
    pebble.Synonyms |> List.map _.Text |> should contain "stone"
    pebble.Synonyms |> List.map _.Text |> should contain "small stone"
    pebble.Description.Text.Contains("A pebble that nicely fits into your palm") |> should be True

    // 2. ground (decoration)
    let ground = findThingBySynonym "ground"
    ground.IsPortable |> should be False
    ground.Synonyms |> List.map _.Text |> should contain "ground"
    ground.Synonyms |> List.map _.Text |> should contain "floor"

    // 3. glint (decoration)
    let glint = findThingBySynonym "glint"
    glint.Synonyms |> List.map _.Text |> should contain "glint"
    glint.Description.Text.Contains("Hidden among the pebbles lies a metallic key") |> should be True

    // 4. key (item)
    let key = findThingBySynonym "key"
    key.IsPortable |> should be True
    key.Synonyms |> List.map _.Text |> should contain "key"
    key.Description.Text.Contains("A small iron key") |> should be True

    // 5. giant_rock (decoration)
    let giantRock = findThingById "decorations.underground_lake.giant_rock"
    giantRock.Synonyms |> List.map _.Text |> should contain "giant rock"
    giantRock.Description.Text.Contains("A giant rock lies in a corner of the room") |> should be True

    // 6. wooden_door (decoration)
    let door = findThingById "decorations.underground_lake.wooden_door"
    door.Traits.Length |> should be (greaterThan 0)
    let isLocked = door.Traits |> List.choose (function Lockable l -> Some l.IsLocked | _ -> None) |> List.tryHead
    isLocked |> should equal (Some true)
    let isOpen = door.Traits |> List.choose (function Openable o -> Some o.IsOpen | _ -> None) |> List.tryHead
    isOpen |> should equal (Some false)

    // 7. torch (item)
    let torch = findThingBySynonym "torch"
    torch.IsPortable |> should be True
    let torchMods = findModifiersByThing torch
    torchMods |> List.exists (function ThingModifier.Custom (AttributeId "isLit", AttributeValue.Bool false) -> true | _ -> false) |> should be True
    torchMods |> List.exists (function ThingModifier.Custom (AttributeId "lifetime", AttributeValue.Float (123.0, _, _)) -> true | _ -> false) |> should be True
    torchMods |> List.exists (function ThingModifier.Custom (AttributeId "strength", AttributeValue.UInt (100u, 1u, 100u)) -> true | _ -> false) |> should be True

    // 8. bush (decoration)
    let bush = findThingBySynonym "bush"
    bush.Synonyms |> List.map _.Text |> should contain "bush"
    bush.Description.Text.Contains("The bushes grow thick") |> should be True

    // 9. lake (decoration)
    let lake = findThingBySynonym "lake"
    lake.Synonyms |> List.map _.Text |> should contain "lake"
    lake.Description.Text.Contains("The lakes lies silently") |> should be True

    // 10. sign (decoration)
    let sign = findThingById "decorations.underground_lake.sign"
    let signMods = findModifiersByThing sign
    signMods |> List.exists (function ThingModifier.Custom (AttributeId "isDown", AttributeValue.Bool false) -> true | _ -> false) |> should be True
    signMods |> List.exists (function ThingModifier.Custom (AttributeId "text", AttributeValue.String "\"The lake has strong underwater currents\"") -> true | _ -> false) |> should be True