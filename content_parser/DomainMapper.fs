namespace Ifai.ContentParser

open System
open System.Linq
open Ifai.Lib


module DomainMapper =
    open Ifai.Lib
    open Ifai.Lib.Content
    open Ifai.Lib.Shared

    type AdventureRoot = {
        Name: string
        InitialRoom: RoomId
        Language: Language
    }
    
    let private forceBool (s: string) : bool =
        match s.ToLowerInvariant() with
        | "false" -> false
        | "true" -> true
        | other -> failwith $"Cannot convert '%s{other}' to bool"

    let private forceInt (s: string) : int =
        Int32.Parse(s)

    let private forceFloat (s: string) : float =
        Double.Parse(s)

    let find (input: (string * string) list) a : string =
        input |> List.find (fun (k, _) -> k.ToLowerInvariant() = a) |> snd

    let tryFind (input: (string * string) list) a : string option =
        input |> List.tryFind (fun (k, _) -> k.ToLowerInvariant() = a) |> Option.map snd
    
    let mapTrait (t: TraitPto) =
        let find = find t.Properties
        let tryFind = tryFind t.Properties
        match t.Name.ToLowerInvariant() with
        | "openable" ->
            let isOpen = find "isopen" |> forceBool
            let onClosed = tryFind "onclosed"
            let onOpened = tryFind "onopened"
            { Openable.IsOpen = isOpen
              Openable.OnClosed = onClosed
              Openable.OnOpened = onOpened } |> Openable |> List.singleton
        | "lockable" ->
            let isLocked = find "islocked" |> forceBool
            let onLocked = tryFind "onlocked"
            let onUnlocked = tryFind "onunlocked"
            { Lockable.IsLocked = isLocked
              Lockable.OnLocked = onLocked
              Lockable.OnUnlocked = onUnlocked } |> Lockable |> List.singleton
        | "container" ->
            { Container.MaximumNumberOfThings = None
              Container.MaximumWeight = None } |> Container |> List.singleton
        | "door" ->
            let isOpen = find "isopen" |> forceBool
            let isLocked = find "islocked" |> forceBool
            let onLocked = tryFind "onlocked"
            let onUnlocked = tryFind "onunlocked"
            let onClosed = tryFind "onclosed"
            let onOpened = tryFind "onopened"
            [ { Openable.IsOpen = isOpen
                Openable.OnClosed = onClosed
                Openable.OnOpened = onOpened } |> Openable
              { Lockable.IsLocked = isLocked
                Lockable.OnLocked = onLocked
                Lockable.OnUnlocked = onUnlocked } |> Lockable ]
        | other ->
            failwith $"Trait '%s{other}' is currently not supported"

    let mapVariable (v: VariablePto) : Shared.AttributeId * AttributeValue =
        let id = v.Name |> AttributeId.AttributeId
        let value =
            // TODO: add support for min/max values
            match v.Type.ToLowerInvariant() with
            | "b" | "bool" | "boolean" ->
                let value = v.Value |> forceBool
                AttributeValue.Bool value
            | "i" | "int" | "integer" ->
                let min = v.Min |> Option.map Int32.Parse |> Option.defaultValue Int32.MinValue
                let max = v.Max |> Option.map Int32.Parse |> Option.defaultValue Int32.MaxValue
                let value = v.Value |> Int32.Parse
                AttributeValue.Int (value, min, max)
            | "uint" | "uinteger" ->
                let min = v.Min |> Option.map UInt32.Parse |> Option.defaultValue UInt32.MinValue
                let max = v.Max |> Option.map UInt32.Parse |> Option.defaultValue UInt32.MaxValue
                let value = v.Value |> UInt32.Parse
                AttributeValue.UInt (value, min, max)
            | "f" | "float" | "d" | "double" ->
                let min = v.Min |> Option.map Double.Parse |> Option.defaultValue Double.MinValue
                let max = v.Max |> Option.map Double.Parse |> Option.defaultValue Double.MaxValue
                let value = v.Value |> Double.Parse
                AttributeValue.Float (value, min, max)
            | "s" | "string" ->
                AttributeValue.String v.Value
            | other -> failwith $"The modifier/variable type '%s{other}' is not supported"
        id, value
    
    let mapExit (s: string) : Exit =
        match s.ToLowerInvariant() with
        | "north" -> Dir Direction.North
        | "northeast" -> Dir Direction.NorthEast
        | "east" -> Dir Direction.East
        | "southeast" -> Dir Direction.SouthEast
        | "south" -> Dir Direction.South
        | "southwest" -> Dir Direction.SouthWest
        | "west" -> Dir Direction.West
        | "northwest" -> Dir Direction.NorthWest
        | "up" -> Dir Direction.Up
        | "down" -> Dir Direction.Down
        | "left" -> Dir Direction.Left
        | "right" -> Dir Direction.Right
        | other -> Ifai.Lib.Exit.Custom (other |> TextKey.create |> Text.create)

    let toLocalizedText (s: string) =
        // TODO: add actual handling of parameters!
        { Text.LocalizedText.Text = s
          Text.LocalizedText.NarrativeStyle = NarrativeStyle.Regular
          Text.LocalizedText.Parameters = None
          Text.LocalizedText.ParameterFormatting = None }
    
    let mapThing (roomId: string) (thing: ThingPto) : Thing * ThingModifier list =
        if thing.Synonyms.IsEmpty then failwith $"Cannot create thing from a ThingPto without synonyms: %A{thing}"
        
        let id =
            let prefix =
                match thing.Category with
                | ThingCategory.Decoration -> $"decorations.%s{roomId}."
                | ThingCategory.Item -> "items."
            let id =
                thing.Id
                |> Option.defaultValue $"%s{thing.Synonyms.Head}-%A{Guid.NewGuid()}"
            prefix + id

        let modifiers =
            thing.Modifiers |> List.map (mapVariable >> ThingModifier.Custom)

        {
            Thing.Id = id |> ThingId.create
            Thing.Synonyms = thing.Synonyms |> List.map toLocalizedText
            Thing.Description = thing.Desc |> toLocalizedText
            Thing.IsAbstract = thing.IsAbstract
            Thing.IsPortable = match thing.Category with ThingCategory.Item -> true | ThingCategory.Decoration -> false
            Thing.LegalOwner = LegalOwner.Nobody
            Thing.Name = thing.Synonyms.First() |> toLocalizedText
            Thing.Traits = thing.Traits |> List.collect mapTrait
            Thing.NeedsDiscovery = thing.NeedsDiscovery

            // TODO: set interactibility to a meaningful value
            Thing.Interactability = Interactability.All
            // TODO: set interactibility to a meaningful value
            Thing.Weight = 0u
        }, modifiers
        
    let mapAdventure (adventure: AdventurePto) : AdventureRoot =
        { Name = adventure.Name
          InitialRoom = RoomId.create adventure.InitialRoomId
          Language = Language.create adventure.Language }

    let mapRoom (room: RoomPto) : Room * RoomModifier list =
        let connections =
            room.Exits
            |> List.collect (fun exitPto ->
                exitPto.Direction
                |> List.map (fun dir ->
                    let exit = mapExit dir
                    let targetRoomId = exitPto.ToId |> RoomId.create
                    let connection = Connection.create exit targetRoomId
                    exit, connection
                )
            )
            |> Map.ofList

        let modifiers =
            room.Variables |> List.map (mapVariable >> RoomModifier.Custom)

        { Id = room.Id |> RoomId.create
          Name = room.Name |> toLocalizedText
          Description = room.Desc |> toLocalizedText
          Connections = connections
          OnEnter = RoomEvent.Nothing
          OnLeaving = RoomEvent.Nothing
          Environment = RoomEnvironment.``default`` }, modifiers

    
    type MappedContent = {
        Rooms: (Room * RoomModifier list) list
        Things: (Thing * ThingModifier list * ThingLocation) list
        Adventure: AdventureRoot option
    }

    let mapFullContent (lines: ContentLine list) : MappedContent =
        let rec loop (remaining: ContentLine list) (acc: MappedContent) (currentRoomId: string option) =
            match remaining with
            | [] -> acc
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u && cl.Key = "room" ->
                let roomPto, rest = BlockParser.toRoomPto remaining
                let roomWithMods = mapRoom roomPto
                loop rest { acc with Rooms = roomWithMods :: acc.Rooms } (Some roomPto.Id)
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u && (cl.Key = "item" || cl.Key = "decoration") ->
                match currentRoomId with
                | Some rid ->
                    let thingPto, rest = BlockParser.toThingPto remaining
                    let thing, mods = mapThing rid thingPto
                    let location = ThingLocation.Room (RoomId.create rid)
                    loop rest { acc with Things = (thing, mods, location) :: acc.Things } (Some rid)
                | None -> failwith "Found item/decoration before any room definition"
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u && (cl.Key = "adventure") ->
                let adventurePto, rest = BlockParser.toAdventurePto remaining
                let adventure = mapAdventure adventurePto
                loop rest { acc with Adventure = Some adventure } currentRoomId
                
            | _ :: rest -> loop rest acc currentRoomId

        let result =
            loop
                lines
                { Rooms = []; Things = []; Adventure = None }
                None
        { Rooms = List.rev result.Rooms
          Things = List.rev result.Things
          Adventure = result.Adventure }

