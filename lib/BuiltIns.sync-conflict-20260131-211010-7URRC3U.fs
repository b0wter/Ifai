module Ifai.Lib.BuiltIns

open System


type GlobalBuiltIn =
    | Load of string option
    | Save of string option
    | Quit


/// <summary>
/// Describes a dictionary that transforms a string into a built-in.<br/>
/// Uses factory functions to create the BuiltIn because BuiltIns may have an argument.
/// The first part of the tuple is the strings that the BuiltIn matches
/// </summary>
type BuiltInFactoryDictionary = string list * (string -> GlobalBuiltIn)

let private english =
    [
    ["save"; "savegame"], fun argument -> Save argument
    ["quit"; "exit"], fun _ -> Quit
    ]


let private german =
    [
    ["speicher "; "speichern "], fun argument -> Save argument
    ["beenden"; "ende"], fun _ -> Quit
    ]


let globalBuiltIns =
    [ Language.create "en", english
      Language.create "de", german
    ] |> Map.ofList


let tryParseBuiltIn<'parserResult> (builtIns: Map<Language, (string list * (string -> 'parserResult)) list>) (lang: Language) (input: string) : 'parserResult list =
    let lowerCaseInput = input.ToLowerInvariant()
    let primitives = builtIns |> Map.find lang
    let foo =
        primitives
        |> List.choose (fun (primitives, mapper) ->
                let applicablePrimitives =
                    primitives |> List.where (fun primitive -> lowerCaseInput.StartsWith(primitive))
                
                if applicablePrimitives.IsEmpty then None
                else
                    applicablePrimitives
                    |> List.map (fun p ->
                        let maybeArgument =
                            match input.Substring(p.Length).Trim() with
                            | empty when empty |> String.IsNullOrWhiteSpace -> None
                            | argument -> Some argument
                        maybeArgument |> Option.map mapper)
                    |> Some
            )
    //|> List.collect id


let globalParser (language: Language) (input: string) : GlobalBuiltIn option =
    match input |> tryParseBuiltIn globalBuiltIns language with
    | [] -> None
    | [result] -> Some result
    | _ -> failwith "More than one built-in matched in the global parser"