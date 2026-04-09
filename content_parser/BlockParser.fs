namespace Ifai.ContentParser

open System


type ContentKey =
    | SingleKey of string
    | MultiKey of string list

type ContentValue =
    | SingleValue of string
    | MultiValue of string list
    /// Represents the "|" that denotes a multi-line string in the next lines
    | ContentBelow
    | NoValue


type VariablePto = {
    Name: string
    Value: string
    Type: string
    Min: string option
    Max: string option
}

type ActionPto = {
    Operation: string
    Arguments: string
}

type OutcomePto = {
    If: string
    Actions: ActionPto list
}

type InteractionPto = {
    Name: string
    Requires: string list
    Synonyms: string list
    Outcomes: OutcomePto list
}

type TraitPto = {
    Name: string
    Properties: (string * string) list
}


type ThingCategory =
    | Item
    | Decoration


type ThingPto = {
    Id: string option
    Synonyms: string list
    Desc: string
    Interactions: InteractionPto list
    Traits: TraitPto list
    Modifiers: VariablePto list
    Category: ThingCategory
    NeedsDiscovery: bool // default case is false
    IsAbstract: bool // default case is false
}

type ExitPto = {
    Direction: string list
    ToId: string
    Via: string option
}

type RoomPto = {
    Id: string
    Name: string
    Exits: ExitPto list
    Desc: string
    Variables: VariablePto list
}

type AdventurePto = {
    Name: string
    InitialRoomId: string
    Language: string
}

module BlockParser =
    let parseDesc (lines: ContentLine list) =
        let descStartIdx = lines |> List.tryFindIndex (function
            | ContentLine.ComplexLine cl when cl.Key = "desc" -> true
            | _ -> false)
        
        match descStartIdx with
        | None -> ""
        | Some idx ->
            let descLine = match lines[idx] with ContentLine.ComplexLine cl -> cl | _ -> failwith "impossible"
            let followingLines = lines |> List.skip (idx + 1)
            let contentLines = 
                followingLines 
                |> List.takeWhile (function
                    | ContentLine.ComplexLine cl when cl.Indentation <= descLine.Indentation -> false
                    | _ -> true)
            
            contentLines 
            |> List.map (function
                | ContentLine.StringLine s -> s.TrimStart()
                | ContentLine.ComplexLine cl -> 
                    // Reconstruct the line if it was accidentally parsed as ComplexLine
                    let prefix = if cl.StartsWithDash then "- " else ""
                    let value = cl.Value |> Option.map (fun v -> ":" + v) |> Option.defaultValue ""
                    prefix + cl.Key + value
                )
            |> String.concat "\n"

    
    let parseTraits (lines: ContentLine list) =
        let traitStartIdx = lines |> List.tryFindIndex (function
            | ContentLine.ComplexLine cl when cl.Key = "traits" -> true
            | _ -> false)
        
        match traitStartIdx with
        | None -> []
        | Some idx ->
            let headerLine = match lines[idx] with ContentLine.ComplexLine cl -> cl | _ -> failwith "impossible"
            let traitLines = 
                lines 
                |> List.skip (idx + 1)
                |> List.takeWhile (function
                    | ContentLine.ComplexLine cl when cl.Indentation <= headerLine.Indentation -> false
                    | _ -> true)

            let mutable traits = []
            
            let rec parseBlock (lines: ContentLine list) =
                match lines with
                | [] -> ()
                | ContentLine.ComplexLine cl :: rest when cl.StartsWithDash ->
                    let traitName = cl.Key
                    
                    let propLines = 
                        rest 
                        |> List.takeWhile (function
                            | ContentLine.ComplexLine inner when inner.Indentation <= cl.Indentation && inner.StartsWithDash -> false
                            | ContentLine.ComplexLine inner when inner.Indentation <= cl.Indentation -> false
                            | _ -> true)
                    
                    let nextLines = rest |> List.skip propLines.Length
                    
                    let properties = 
                        propLines |> List.choose (function
                            | ContentLine.ComplexLine subCl -> 
                                Some (subCl.Key, subCl.Value |> Option.defaultValue "")
                            | _ -> None)
                    
                    traits <- { Name = traitName; Properties = properties } :: traits
                    parseBlock nextLines
                | _ :: rest -> parseBlock rest

            parseBlock traitLines
            List.rev traits

    let parseModifiers (lines: ContentLine list) =
        let varStartIdx = lines |> List.tryFindIndex (function
            | ContentLine.ComplexLine cl when cl.Key = "modifiers" -> true
            | _ -> false)
        
        match varStartIdx with
        | None -> []
        | Some idx ->
            let headerLine = match lines[idx] with ContentLine.ComplexLine cl -> cl | _ -> failwith "impossible"
            let varLines = 
                lines 
                |> List.skip (idx + 1)
                |> List.takeWhile (function
                    | ContentLine.ComplexLine cl when cl.Indentation <= headerLine.Indentation -> false
                    | _ -> true)

            let mutable variables = []
            
            let rec parseBlock (lines: ContentLine list) =
                match lines with
                | [] -> ()
                | ContentLine.ComplexLine cl :: rest when cl.StartsWithDash ->
                    let varName = cl.Key
                    let varValue = cl.Value |> Option.defaultValue ""
                    
                    let propLines = 
                        rest 
                        |> List.takeWhile (function
                            | ContentLine.ComplexLine inner when inner.Indentation <= cl.Indentation && inner.StartsWithDash -> false
                            | ContentLine.ComplexLine inner when inner.Indentation <= cl.Indentation -> false
                            | _ -> true)
                    
                    let nextLines = rest |> List.skip propLines.Length
                    
                    let varType = 
                        let explicitType = 
                            propLines |> List.tryPick (function
                                | ContentLine.ComplexLine subCl when subCl.Key = "type" -> subCl.Value
                                | _ -> None)
                        
                        match explicitType with
                        | Some t -> t
                        | None ->
                            match Int32.TryParse varValue with
                            | true, _ -> "integer"
                            | _ ->
                                match Double.TryParse varValue with
                                | true, _ -> "float"
                                | _ ->
                                    match Boolean.TryParse varValue with
                                    | true, _ -> "bool"
                                    | _ -> "string"
                    
                    let min =
                        propLines |> List.tryPick (function
                            | ContentLine.ComplexLine l when l.Key = "min" -> l.Value
                            | _ -> None)
                    
                    let max =
                        propLines |> List.tryPick (function
                            | ContentLine.ComplexLine l when l.Key = "max" -> l.Value
                            | _ -> None)
                    
                    variables <- { Name = varName; Value = varValue; Type = varType; Min = min; Max = max } :: variables
                    parseBlock nextLines
                | _ :: rest -> parseBlock rest

            parseBlock varLines
            List.rev variables

    let parseInteractions (lines: ContentLine list) =
        let interactionStartIdx = lines |> List.tryFindIndex (function
            | ContentLine.ComplexLine cl when cl.Key = "interactions" -> true
            | _ -> false)
        
        match interactionStartIdx with
        | None -> []
        | Some idx ->
            let headerLine = match lines[idx] with ContentLine.ComplexLine cl -> cl | _ -> failwith "impossible"
            let interactionLines = 
                lines 
                |> List.skip (idx + 1)
                |> List.takeWhile (function
                    | ContentLine.ComplexLine cl when cl.Indentation <= headerLine.Indentation -> false
                    | _ -> true)

            let mutable interactions = []
            
            let rec parseBlock (lines: ContentLine list) =
                match lines with
                | [] -> ()
                | ContentLine.ComplexLine cl :: rest when cl.StartsWithDash ->
                    // New interaction
                    let names = cl.Key.Split(',') |> Array.toList |> List.map _.Trim()
                    let name = names.Head
                    let synonyms = names.Tail

                    let blockLines = 
                        rest 
                        |> List.takeWhile (function
                            | ContentLine.ComplexLine inner when inner.Indentation <= cl.Indentation && inner.StartsWithDash -> false
                            | _ -> true)
                    
                    let nextLines = rest |> List.skip blockLines.Length

                    let mutable interactionRequires = []
                    let mutable interactionSynonyms = synonyms
                    let mutable outcomes = []

                    // Check for explicit synonyms or requires at the interaction level
                    let mutable i = 0
                    let mutable processedAsOutcomes = false
                    
                    let getDirectActions (lines: ContentLine list) =
                        lines |> List.choose (function
                            | ContentLine.ComplexLine cl -> Some { Operation = cl.Key; Arguments = cl.Value |> Option.defaultValue "" }
                            | _ -> None)

                    while i < blockLines.Length do
                        match blockLines[i] with
                        | ContentLine.ComplexLine subCl when subCl.Key = "synonyms" ->
                            let value = subCl.Value |> Option.defaultValue ""
                            let extraSynonyms = value.Split(',') |> Array.toList |> List.map _.Trim()
                            interactionSynonyms <- interactionSynonyms @ extraSynonyms
                            i <- i + 1
                        | ContentLine.ComplexLine subCl when subCl.Key = "requires" ->
                            let reqLines = 
                                blockLines 
                                |> List.skip (i + 1)
                                |> List.takeWhile (function
                                    | ContentLine.ComplexLine inner when inner.Indentation <= subCl.Indentation -> false
                                    | _ -> true)
                            interactionRequires <- reqLines |> List.choose (function
                                | ContentLine.ComplexLine inner -> Some inner.Key
                                | _ -> None)
                            i <- i + 1 + reqLines.Length
                        | ContentLine.ComplexLine subCl when subCl.Key = "outcomes" ->
                            let outcomeBlockLines = 
                                blockLines 
                                |> List.skip (i + 1)
                                |> List.takeWhile (function
                                    | ContentLine.ComplexLine inner when inner.Indentation > subCl.Indentation -> true
                                    | ContentLine.StringLine _ -> true
                                    | _ -> false)
                            
                            // Parse each outcome in the block
                            let mutable j = 0
                            while j < outcomeBlockLines.Length do
                                match outcomeBlockLines[j] with
                                | ContentLine.ComplexLine oCl when oCl.StartsWithDash ->
                                    let singleOutcomeLines = 
                                        outcomeBlockLines 
                                        |> List.skip (j + 1)
                                        |> List.takeWhile (function
                                            | ContentLine.ComplexLine inner when inner.Indentation > oCl.Indentation -> true
                                            | ContentLine.StringLine _ -> true
                                            | _ -> false)
                                    
                                    let ifCond = 
                                        if oCl.Key = "if" then oCl.Value |> Option.defaultValue ""
                                        else
                                            singleOutcomeLines 
                                            |> List.tryPick (function 
                                                | ContentLine.ComplexLine inner when inner.Key = "if" -> Some (inner.Value |> Option.defaultValue "")
                                                | _ -> None)
                                            |> Option.defaultValue "true"
                                    
                                    let actions = 
                                        singleOutcomeLines 
                                        |> List.filter (function | ContentLine.ComplexLine inner -> inner.Key <> "if" | _ -> false)
                                        |> getDirectActions
                                    
                                    let actions = if oCl.Key <> "if" then { Operation = oCl.Key; Arguments = oCl.Value |> Option.defaultValue "" } :: actions else actions
                                    
                                    outcomes <- { If = ifCond; Actions = actions } :: outcomes
                                    j <- j + 1 + singleOutcomeLines.Length
                                | _ -> j <- j + 1
                            
                            i <- i + 1 + outcomeBlockLines.Length
                            processedAsOutcomes <- true
                        | ContentLine.ComplexLine subCl when subCl.Key = "otherwise" || subCl.Key = "else" || subCl.Key = "default" ->
                            let outcomeLines = 
                                blockLines 
                                |> List.skip (i + 1)
                                |> List.takeWhile (function
                                    | ContentLine.ComplexLine inner when inner.Indentation > subCl.Indentation -> true
                                    | ContentLine.StringLine _ -> true
                                    | _ -> false)
                            
                            let actions = getDirectActions outcomeLines
                            let actions = if subCl.Key <> "if" && subCl.Key <> "else" && subCl.Key <> "otherwise" && subCl.Key <> "default" then { Operation = subCl.Key; Arguments = subCl.Value |> Option.defaultValue "" } :: actions else actions
                            
                            outcomes <- { If = subCl.Key; Actions = actions } :: outcomes
                            i <- i + 1 + outcomeLines.Length
                            processedAsOutcomes <- true
                        | ContentLine.ComplexLine subCl when subCl.StartsWithDash ->
                            // This is a list of outcomes directly under the interaction
                            let singleOutcomeLines = 
                                blockLines 
                                |> List.skip (i + 1)
                                |> List.takeWhile (function
                                    | ContentLine.ComplexLine inner when inner.Indentation > subCl.Indentation -> true
                                    | ContentLine.StringLine _ -> true
                                    | _ -> false)
                            
                            let ifCond = 
                                if subCl.Key = "if" then subCl.Value |> Option.defaultValue "true"
                                elif subCl.Key = "else" || subCl.Key = "otherwise" || subCl.Key = "default" then subCl.Key 
                                else 
                                    singleOutcomeLines 
                                    |> List.tryPick (function 
                                        | ContentLine.ComplexLine inner when inner.Key = "if" -> Some (inner.Value |> Option.defaultValue "true")
                                        | _ -> None)
                                    |> Option.defaultValue "true"

                            let actions = 
                                singleOutcomeLines 
                                |> List.filter (function | ContentLine.ComplexLine inner -> inner.Key <> "if" | _ -> false)
                                |> getDirectActions
                            
                            // If the dash line itself was an action (not a special key), include it
                            let actions = if subCl.Key <> "if" && subCl.Key <> "else" && subCl.Key <> "otherwise" && subCl.Key <> "default" then { Operation = subCl.Key; Arguments = subCl.Value |> Option.defaultValue "" } :: actions else actions

                            outcomes <- { If = ifCond; Actions = actions } :: outcomes
                            i <- i + 1 + singleOutcomeLines.Length
                            processedAsOutcomes <- true
                        | ContentLine.ComplexLine subCl ->
                            // Might be a direct action if no outcomes or requires/synonyms matched yet
                            // but we should wait to see if any outcome dashes appear.
                            // For simplicity, if we haven't seen "outcomes" or a dash, and it's not "requires"/"synonyms",
                            // it's a direct action and we treat the whole interaction as having one outcome.
                            i <- i + 1
                        | _ -> i <- i + 1
                    
                    if not processedAsOutcomes then
                        // Treat all non-requires, non-synonyms complex lines as actions for a single outcome
                        let actions = 
                            blockLines 
                            |> List.choose (function 
                                | ContentLine.ComplexLine subCl when subCl.Key <> "requires" && subCl.Key <> "synonyms" -> 
                                    Some { Operation = subCl.Key; Arguments = subCl.Value |> Option.defaultValue "" }
                                | _ -> None)
                        if not actions.IsEmpty then
                            outcomes <- [{ If = "true"; Actions = actions }]

                    interactions <- {
                        Name = name
                        Requires = interactionRequires
                        Synonyms = interactionSynonyms |> List.distinct
                        Outcomes = List.rev outcomes
                    } :: interactions
                    parseBlock nextLines
                | _ :: rest -> parseBlock rest

            parseBlock interactionLines
            List.rev interactions

    let toAdventurePto (lines: ContentLine list) : AdventurePto * ContentLine list =
        let afterAdventureStart =
            match lines with
            | ContentLine.ComplexLine cl :: rest when cl.Indentation = 0u && cl.Key = "adventure" -> rest
            | _ -> failwith "Input must start with an adventure definition"

        let rec getAdventureBlock acc lines =
            match lines with
            | [] -> (List.rev acc, [])
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u -> (List.rev acc, lines)
            | head :: rest -> getAdventureBlock (head :: acc) rest

        let adventureLines, unusedLines = getAdventureBlock [] afterAdventureStart
        
        let getValue key (lines: ContentLine list) =
            lines |> List.tryPick (function
                | ContentLine.ComplexLine cl when cl.Key = key -> cl.Value
                | _ -> None)
        
        let name = getValue "name" adventureLines |> Option.defaultWith (fun () -> failwith "Cannot create adventure without name")
        let initialRoomId = getValue "initial_room" adventureLines |> Option.defaultWith (fun () -> failwith "Cannot create adventure without initial room key")
        let language = getValue "language" adventureLines |> Option.defaultWith (fun () -> failwith "Cannot create adventure without langauge key")
        { Name = name; InitialRoomId = initialRoomId; Language = language }, unusedLines


    let toRoomPto (lines: ContentLine list) : RoomPto * ContentLine list =
        let afterRoomStart = 
            match lines with
            | ContentLine.ComplexLine cl :: rest when cl.Indentation = 0u && cl.Key = "room" -> rest
            | _ -> failwith "Input must start with a room definition"

        let rec getRoomBlock acc lines =
            match lines with
            | [] -> (List.rev acc, [])
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u -> (List.rev acc, lines)
            | head :: rest -> getRoomBlock (head :: acc) rest

        let roomLines, unusedLines = getRoomBlock [] afterRoomStart

        let getValue key (lines: ContentLine list) =
            lines |> List.tryPick (function
                | ContentLine.ComplexLine cl when cl.Key = key -> cl.Value
                | _ -> None)

        let id = getValue "id" roomLines |> Option.defaultValue ""
        let name = getValue "name" roomLines |> Option.map _.Trim('\"') |> Option.defaultValue ""

        let parseExits (lines: ContentLine list) =
            let mutable exits = []
            let mutable currentExit = None
            
            let mutable inExits = false
            for line in lines do
                match line with
                | ContentLine.ComplexLine cl when cl.Key = "exits" -> 
                    inExits <- true
                | ContentLine.ComplexLine cl when inExits && cl.Indentation = 0u ->
                    inExits <- false
                | ContentLine.ComplexLine cl when inExits && cl.StartsWithDash ->
                    // New exit
                    currentExit |> Option.iter (fun e -> exits <- e :: exits)
                    let directions = cl.Key.Split(',') |> Array.toList |> List.map _.Trim()
                    let target = cl.Value |> Option.defaultValue ""
                    currentExit <- Some { Direction = directions; ToId = target; Via = None }
                | ContentLine.ComplexLine cl when inExits && cl.Key = "via" ->
                    currentExit <- currentExit |> Option.map (fun e -> { e with Via = cl.Value })
                | ContentLine.ComplexLine cl when inExits && cl.Indentation <= 2u && cl.Key <> "exits" ->
                    inExits <- false
                | _ -> ()
            
            currentExit |> Option.iter (fun e -> exits <- e :: exits)
            List.rev exits

        let room = {
            Id = id
            Name = name
            Exits = parseExits roomLines
            Desc = parseDesc roomLines
            Variables = parseModifiers roomLines
        }
        
        (room, unusedLines)

    let toThingPto (lines: ContentLine list) : ThingPto * ContentLine list =
        let afterThingStart, category = 
            match lines with
            | ContentLine.ComplexLine cl :: rest when cl.Indentation = 0u && cl.Key = "item" -> rest, ThingCategory.Item
            | ContentLine.ComplexLine cl :: rest when cl.Indentation = 0u && cl.Key = "decoration" -> rest,
                                                                                                      ThingCategory.Decoration
            | _ -> failwith "Input must start with an thing or decoration definition"

        let rec getThingBlock acc lines =
            match lines with
            | [] -> (List.rev acc, [])
            | ContentLine.ComplexLine cl :: _ when cl.Indentation = 0u -> (List.rev acc, lines)
            | head :: rest -> getThingBlock (head :: acc) rest

        let thingLines, unusedLines = getThingBlock [] afterThingStart

        let getValue key (lines: ContentLine list) =
            lines |> List.tryPick (function
                | ContentLine.ComplexLine cl when cl.Key = key -> cl.Value
                | _ -> None)

        let synonyms = 
            getValue "synonyms" thingLines 
            |> Option.map (fun s -> s.Split(',') |> Array.toList |> List.map _.Trim())
            |> Option.defaultValue []
            
        let isAbstract =
            getValue "isabstract" thingLines
            |> Option.map Boolean.Parse
            |> Option.defaultValue false

        let needsDiscovery =
            getValue "needsdiscovery" thingLines
            |> Option.map Boolean.Parse
            |> Option.defaultValue false

        let thing = {
            Id = getValue "id" thingLines
            Synonyms = synonyms
            Desc = parseDesc thingLines
            Interactions = parseInteractions thingLines
            Traits = parseTraits thingLines
            Modifiers = parseModifiers thingLines
            Category = category
            IsAbstract = isAbstract
            NeedsDiscovery = needsDiscovery
        }
        
        (thing, unusedLines)