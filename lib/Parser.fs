module Ifai.Lib.Parser



// TODO: make the function return a union type with the parser result (not found, argument missing, e.g.)
let tryParseBuiltIn<'parserResult> (builtIns: Map<Language, (string list * (string option -> 'parserResult)) list>) (lang: Language) (input: string) : 'parserResult list =
    let lowerCaseInput = input.ToLowerInvariant().Trim()
    let primitives = builtIns |> Map.find lang
    primitives
    |> List.collect (fun (primitives, mapper) ->
            let applicablePrimitives =
                primitives |> List.where (fun primitive -> lowerCaseInput = primitive || lowerCaseInput.StartsWith($"%s{primitive} "))

            if applicablePrimitives.IsEmpty then []
            else
                applicablePrimitives
                |> List.map (fun p ->
                    match input.Substring(p.Length).Trim() with
                                   | "" -> None
                                   | s -> Some s
                                   |> mapper)
            )
