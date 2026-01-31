module Ifai.Lib.Parser


let tryParseBuiltIn<'parserResult> (builtIns: Map<Language, (string list * (string -> 'parserResult)) list>) (lang: Language) (input: string) : 'parserResult list =
    let lowerCaseInput = input.ToLowerInvariant().Trim()
    let primitives = builtIns |> Map.find lang
    primitives
    |> List.collect (fun (primitives, mapper) ->
            let applicablePrimitives =
                primitives |> List.where (fun primitive -> lowerCaseInput.StartsWith(primitive))

            if applicablePrimitives.IsEmpty then []
            else
                applicablePrimitives
                |> List.choose (fun p ->
                    let argument = input.Substring(p.Length).Trim()
                    if argument.Length > 0 then Some (argument |> mapper)
                    else None)
                )
