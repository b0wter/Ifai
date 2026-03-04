namespace Ifai.ContentParser

open System
open System.Linq
open Ifai.Lib
open Ifai.ContentParser

module ContentParser =

    let rawContent = """
      actions:
        - kick, punch, strike:
            - if: !isDown
              say: "You kick the sign and it falls onto the ground."
              set: isDown = true
            - if: isDown && !isBroken
              say: "The sign breaks under your foot."
              set: isBroken = true
            - default:
              say: "The sign is already broken."
    """
    let content = rawContent.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Skip(1)


    let getIndentation (s: string) : int =
        s.Length - s.TrimStart().Length

    let parseAction (source: string seq) : Result<ActionDto list, string> =
        let emptyAction =
            { Synonyms = []
              SubActions = [] } 
        
        let emptySubAction =
            { If = None
              Say = None
              Set = [] }

        let mapSubAction (inConstruction: SubActionInConstructionDto) : Result<SubActionDto, string> =
            match inConstruction.If with 
            | None -> Error "Cannot construct SubActionDto without `If` part"
            | Some ``if`` ->
                {
                    SubActionDto.If = ``if``
                    SubActionDto.Say = inConstruction.Say
                    SubActionDto.Set = inConstruction.Set
                } |> Ok
     
        let rec step (initialIndentation: int) (remaining: string seq) (current: ActionDto) (currentSub: SubActionInConstructionDto) (acc: ActionDto list) =
            match remaining |> Seq.tryHead with 
            | None ->
                current :: acc
            | Some head when head |> getIndentation <= initialIndentation ->
                current :: acc 
            | Some head ->
                //let isNewSubAction 
                failwith $"found %A{head}" 
                    

        if source |> Seq.isEmpty then
            Error "Given source is empty, cannot create ActionDto from nothing"
        elif source.First().TrimStart().ToLowerInvariant().StartsWith("actions:") then
            let initialIndentation = source.First() |> getIndentation
            step initialIndentation (source |> Seq.skip 1) emptyAction emptySubAction [] |> Ok
        else
            Error "The given source block does not start with the required 'actions:' line"

    parseAction content