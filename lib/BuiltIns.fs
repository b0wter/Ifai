module Ifai.Lib.BuiltIns


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

let private english : BuiltInFactoryDictionary list =
    [
    ["save"; "savegame"], fun argument -> Save (if argument.Length > 0 then Some argument else None)
    ["quit"; "exit"], fun _ -> Quit
    ]


let private german : BuiltInFactoryDictionary list =
    [
    ["speicher "; "speichern "], fun argument -> Save (if argument.Length > 0 then Some argument else None)
    ["beenden"; "ende"], fun _ -> Quit
    ]


let globalBuiltIns =
    [ Language.create "en", english
      Language.create "de", german
    ] |> Map.ofList


let globalParser (language: Language) (input: string) : GlobalBuiltIn option =
    match input |> Parser.tryParseBuiltIn globalBuiltIns language with
    | [] -> None
    | [result] -> Some result
    | _ -> failwith "More than one built-in matched in the global parser"