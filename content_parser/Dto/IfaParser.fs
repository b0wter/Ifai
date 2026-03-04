namespace Foo.Bar
(*
namespace Ifai.ContentParser.Dto

open System
open Ifai.Lib

module IfaParser =

    type private Line = {
        Indentation: int
        Content: string
    }

    let private parseLine (line: string) =
        let trimmed = line.TrimStart()
        let indent = line.Length - trimmed.Length
        { Indentation = indent; Content = trimmed }

    let rec private parseDescription (lines: Line list) (indent: int) =
        let mutable descLines = []
        let mutable remaining = lines
        let mutable stop = false
        while not remaining.IsEmpty && not stop do
            let line = remaining.Head
            if line.Content = "" then
                descLines <- "" :: descLines
                remaining <- remaining.Tail
            elif line.Indentation >= indent then
                descLines <- line.Content :: descLines
                remaining <- remaining.Tail
            else
                stop <- true
        let (description: string) = String.Join("\n", descLines |> List.rev).TrimEnd()
        description, remaining

    let private splitList (s: string) =
        s.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        |> Array.toList

    let private parseCondition (s: string) : ConditionDto =
        if s.StartsWith("!") then
            ConditionDto.Not (ConditionDto.Variable (s.Substring(1).Trim()))
        elif s.Contains("&&") then
            let (parts: string array) = s.Split([|"&&"|], StringSplitOptions.RemoveEmptyEntries)
            ConditionDto.And (ConditionDto.Variable (parts[0].Trim()), ConditionDto.Variable (parts[1].Trim()))
        elif s.Contains("||") then
            let (parts: string array) = s.Split([|"||"|], StringSplitOptions.RemoveEmptyEntries)
            ConditionDto.Or (ConditionDto.Variable (parts[0].Trim()), ConditionDto.Variable (parts[1].Trim()))
        else
            ConditionDto.Variable (s.Trim())

    let private parseTarget (this: TargetDto) (s: string) : TargetDto =
        if s.StartsWith("decorations.") then
            TargetDto.ItemTarget (s.Replace("decorations.", String.Empty) |> ItemId.create)
        elif s.StartsWith("items.") then
            TargetDto.ItemTarget (s.Replace("items.", String.Empty) |> ItemId.create)
        elif s.StartsWith("rooms.") then
            TargetDto.RoomTarget (s.Replace("rooms.", String.Empty) |> RoomId.create)
        elif s.StartsWith("characters.") then
            TargetDto.CharacterTarget (s.Replace("characters.", String.Empty) |> CharacterId.create)
        elif s = "this" then
            this
        else
            failwith $"Unknown target: {s}"

    let parse (content: string) : Dto list =
        let (allLines: Line list) = 
            content.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseLine
            |> Array.toList
        
        let mutable (results: Dto list) = []
        let mutable (currentLines: Line list) = allLines

        while not currentLines.IsEmpty do
            let line = currentLines.Head
            currentLines <- currentLines.Tail
            
            if line.Content = "room:" then
                let mutable id = ""
                let mutable name = ""
                let mutable desc = ""
                while not currentLines.IsEmpty && currentLines.Head.Indentation > line.Indentation do
                    let child = currentLines.Head
                    currentLines <- currentLines.Tail
                    if child.Content.StartsWith("id:") then id <- child.Content.Substring(3).Trim()
                    elif child.Content.StartsWith("name:") then name <- child.Content.Substring(5).Trim().Trim('"')
                    elif child.Content.StartsWith("desc: |") then
                        let d, rem = parseDescription currentLines (child.Indentation + 2)
                        desc <- d
                        currentLines <- rem
                let (room: RoomDto) = { Id = id; Name = name; Description = desc }
                results <- Dto.Room room :: results

            elif line.Content = "item:" then
                let mutable (id: string option) = None
                let mutable (synonyms: string list) = []
                let mutable (desc: string) = ""
                let mutable (interactions: Interaction list) = []
                let mutable (state: Map<string, string>) = Map.empty
                let mutable (actions: ActionDto list) = []
                
                while not currentLines.IsEmpty && currentLines.Head.Indentation > line.Indentation do
                    let child = currentLines.Head
                    currentLines <- currentLines.Tail
                    if child.Content.StartsWith("id:") then id <- Some (child.Content.Substring(3).Trim())
                    elif child.Content.StartsWith("synonyms:") then synonyms <- splitList (child.Content.Substring(9))
                    elif child.Content.StartsWith("desc: |") then
                        let d, rem = parseDescription currentLines (child.Indentation + 2)
                        desc <- d
                        currentLines <- rem
                    elif child.Content.StartsWith("interactions:") then
                        while not currentLines.IsEmpty && currentLines.Head.Indentation > child.Indentation do
                            let iLine = currentLines.Head
                            currentLines <- currentLines.Tail
                            if iLine.Content.StartsWith("-") then
                                let syns = splitList (iLine.Content.Substring(1).Trim().TrimEnd(':'))
                                let mutable hasSubBlocks = false
                                let mutable tempLines = currentLines
                                while not tempLines.IsEmpty && tempLines.Head.Indentation > iLine.Indentation && not hasSubBlocks do
                                    if tempLines.Head.Content.StartsWith("-") then
                                        hasSubBlocks <- true
                                    tempLines <- tempLines.Tail
                                
                                if hasSubBlocks then
                                    while not currentLines.IsEmpty && currentLines.Head.Indentation > iLine.Indentation do
                                        let subBlockLine = currentLines.Head
                                        if subBlockLine.Content.StartsWith("-") then
                                            currentLines <- currentLines.Tail
                                            let mutable (target: TargetDto option) = None
                                            let mutable (destroy: string option) = None
                                            let mutable (moveTo: string option) = None
                                            let mutable (say: string option) = None
                                            
                                            let processLine (l: Line) =
                                                if l.Content.StartsWith("target:") then
                                                    //target <- Some (parseTarget (l.Content.Substring(7).Trim()))
                                                    failwith "not implemented"
                                                elif l.Content.StartsWith("- target:") then
                                                    //target <- Some (parseTarget (l.Content.Substring(9).Trim()))
                                                    failwith "not implemented"
                                                elif l.Content.StartsWith("destroy:") then
                                                    destroy <- Some (l.Content.Substring(8).Trim())
                                                elif l.Content.StartsWith("moveTo:") then
                                                    moveTo <- Some (l.Content.Substring(7).Trim().Replace("rooms.", ""))
                                                elif l.Content.StartsWith("say:") then
                                                    say <- Some (l.Content.Substring(4).Trim().Trim('"'))

                                            processLine subBlockLine

                                            while not currentLines.IsEmpty && currentLines.Head.Indentation > subBlockLine.Indentation do
                                                let iChild = currentLines.Head
                                                currentLines <- currentLines.Tail
                                                processLine iChild
                                                
                                            (*
                                            let (interaction: Interaction) = { Synonyms = syns; Target = target; Destroy = destroy; MoveTo = moveTo; Say = say }
                                            interactions <- interaction :: interactions
                                            *)
                                            failwith "not implemented"
                                        else
                                            // This should not happen if hasSubBlocks is true and format is correct,
                                            // but for safety we skip lines that are not subblocks if they somehow appear here
                                            currentLines <- currentLines.Tail
                                else
                                    let mutable (target: TargetDto) = TargetDto.ItemTarget (ItemId.create String.Empty)
                                    let mutable (destroy: string option) = None
                                    let mutable (moveTo: string option) = None
                                    let mutable (say: string option) = None
                                    while not currentLines.IsEmpty && currentLines.Head.Indentation > iLine.Indentation do
                                        let iChild = currentLines.Head
                                        currentLines <- currentLines.Tail
                                        if iChild.Content.StartsWith("target:") then
                                            //target <- parseTarget (iChild.Content.Substring(7).Trim())
                                            failwith "not implemented"
                                        elif iChild.Content.StartsWith("- target:") then
                                            //target <- parseTarget (iChild.Content.Substring(9).Trim())
                                            failwith "not implemented"
                                        elif iChild.Content.StartsWith("destroy:") then
                                            destroy <- Some (iChild.Content.Substring(8).Trim())
                                        elif iChild.Content.StartsWith("moveTo:") then
                                            moveTo <- Some (iChild.Content.Substring(7).Trim().Replace("rooms.", ""))
                                        elif iChild.Content.StartsWith("say:") then
                                            say <- Some (iChild.Content.Substring(4).Trim().Trim('"'))
                                    let (interaction: Interaction) = { Synonyms = syns; Target = target; Destroy = destroy; MoveTo = moveTo; Say = say }
                                    interactions <- interaction :: interactions

                    elif child.Content.StartsWith("state:") then
                         while not currentLines.IsEmpty && currentLines.Head.Indentation > child.Indentation do
                            let sLine = currentLines.Head
                            currentLines <- currentLines.Tail
                            let (parts: string array) = sLine.Content.Split(':')
                            state <- state.Add(parts[0].Trim(), parts[1].Trim())

                    elif child.Content.StartsWith("actions:") then
                        while not currentLines.IsEmpty && currentLines.Head.Indentation > child.Indentation do
                            let aLine = currentLines.Head
                            currentLines <- currentLines.Tail
                            if aLine.Content.StartsWith("-") then
                                let syns = splitList (aLine.Content.Substring(1).Trim().TrimEnd(':'))
                                let mutable (subActions: SubActionDto list) = []
                                while not currentLines.IsEmpty && currentLines.Head.Indentation > aLine.Indentation do
                                    let subLine = currentLines.Head
                                    currentLines <- currentLines.Tail
                                    if subLine.Content.StartsWith("-") then
                                        let mutable (cond: ConditionDto) = ConditionDto.Variable "true"
                                        let mutable (say: string option) = None
                                        let mutable (sets: SetDto list) = []
                                        
                                        let processSubActionChild (childLine: Line) =
                                            if childLine.Content.StartsWith("if:") then 
                                                cond <- parseCondition (childLine.Content.Substring(3).Trim())
                                            elif childLine.Content.StartsWith("else:") then 
                                                cond <- ConditionDto.Variable "else"
                                            elif childLine.Content.StartsWith("say:") then 
                                                say <- Some (childLine.Content.Substring(4).Trim().Trim('"'))
                                            elif childLine.Content.StartsWith("set:") then
                                                let (parts: string array) = childLine.Content.Substring(4).Split('=')
                                                sets <- { Variable = parts[0].Trim(); Value = parts[1].Trim() } :: sets
                                        
                                        if subLine.Content.Contains(":") then
                                            processSubActionChild { subLine with Content = subLine.Content.Substring(1).Trim() }

                                        while not currentLines.IsEmpty && currentLines.Head.Indentation > subLine.Indentation do
                                            processSubActionChild currentLines.Head
                                            currentLines <- currentLines.Tail
                                        let (subAction: SubActionDto) = { If = cond; Say = say; Set = sets |> List.rev }
                                        subActions <- subAction :: subActions
                                let (action: ActionDto) = { Synonyms = syns; SubActions = subActions |> List.rev }
                                actions <- action :: actions

                let (item: ItemDto) = { Id = id; Synonyms = synonyms; Description = desc; Interactions = interactions |> List.rev; State = state; Actions = actions |> List.rev }
                results <- Dto.Item item :: results

            elif line.Content = "decoration:" then
                let mutable (id: string option) = None
                let mutable (synonyms: string list) = []
                let mutable (desc: string) = ""
                let mutable (state: Map<string, string>) = Map.empty
                let mutable (actions: ActionDto list) = []
                
                while not currentLines.IsEmpty && currentLines.Head.Indentation > line.Indentation do
                    let child = currentLines.Head
                    currentLines <- currentLines.Tail
                    if child.Content.StartsWith("id:") then id <- Some (child.Content.Substring(3).Trim())
                    elif child.Content.StartsWith("synonyms:") then synonyms <- splitList (child.Content.Substring(9))
                    elif child.Content.StartsWith("desc: |") then
                        let d, rem = parseDescription currentLines (child.Indentation + 2)
                        desc <- d
                        currentLines <- rem
                    elif child.Content.StartsWith("state:") then
                         while not currentLines.IsEmpty && currentLines.Head.Indentation > child.Indentation do
                            let sLine = currentLines.Head
                            currentLines <- currentLines.Tail
                            let (parts: string array) = sLine.Content.Split(':')
                            state <- state.Add(parts[0].Trim(), parts[1].Trim())
                    elif child.Content.StartsWith("actions:") then
                        while not currentLines.IsEmpty && currentLines.Head.Indentation > child.Indentation do
                            let aLine = currentLines.Head
                            currentLines <- currentLines.Tail
                            if aLine.Content.StartsWith("-") then
                                let syns = splitList (aLine.Content.Substring(1).Trim().TrimEnd(':'))
                                let mutable (subActions: SubActionDto list) = []
                                while not currentLines.IsEmpty && currentLines.Head.Indentation > aLine.Indentation do
                                    let subLine = currentLines.Head
                                    currentLines <- currentLines.Tail
                                    if subLine.Content.StartsWith("-") then
                                        let mutable (cond: ConditionDto) = ConditionDto.Variable "true"
                                        let mutable (say: string option) = None
                                        let mutable (sets: SetDto list) = []
                                        
                                        let processSubActionChild (childLine: Line) =
                                            if childLine.Content.StartsWith("if:") then 
                                                cond <- parseCondition (childLine.Content.Substring(3).Trim())
                                            elif childLine.Content.StartsWith("else:") then 
                                                cond <- ConditionDto.Variable "else"
                                            elif childLine.Content.StartsWith("say:") then 
                                                say <- Some (childLine.Content.Substring(4).Trim().Trim('"'))
                                            elif childLine.Content.StartsWith("set:") then
                                                let (parts: string array) = childLine.Content.Substring(4).Split('=')
                                                sets <- { Variable = parts[0].Trim(); Value = parts[1].Trim() } :: sets
                                        
                                        if subLine.Content.Contains(":") then
                                            processSubActionChild { subLine with Content = subLine.Content.Substring(1).Trim() }

                                        while not currentLines.IsEmpty && currentLines.Head.Indentation > subLine.Indentation do
                                            processSubActionChild currentLines.Head
                                            currentLines <- currentLines.Tail
                                        let (subAction: SubActionDto) = { If = cond; Say = say; Set = sets |> List.rev }
                                        subActions <- subAction :: subActions
                                let (action: ActionDto) = { Synonyms = syns; SubActions = subActions |> List.rev }
                                actions <- action :: actions

                let (decoration: DecorationDto) = { Id = id; Synonyms = synonyms; Description = desc; State = state; Actions = actions |> List.rev }
                results <- Dto.Decoration decoration :: results
            else ()
        
        results |> List.rev

*)