namespace Ifai.Lib

open System

type TextKey = private TextKey of string
module TextKey =
    let create (s: string) = TextKey s
    let value (TextKey t) = t


type Language = private Language of string
module Language =
    let create (s: string) = Language s
    let value (Language l) = l


type ParameterKey = private ParameterKey of string
module ParameterKey =
    let create (s: string) = ParameterKey s
    let value (ParameterKey k) = k


type TextResources = Map<Language,Map<TextKey,string>>


type ParameterType =
    | Int
    | Float
    | String
    | Bool
        

type Parameter =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool

module Parameters =
    let getParameterType (p: Parameter) : ParameterType =
        match p with
        | Parameter.Bool _ -> ParameterType.Bool
        | Parameter.Float _ -> ParameterType.Float
        | Parameter.Int _ -> ParameterType.Int
        | Parameter.String _ -> ParameterType.String
        
        
    let private keysRegex = System.Text.RegularExpressions.Regex(@"(?<=\{)[^}]+(?=\})")


    /// <summary>
    /// Searches a string for all parameter keys and returns them as a list
    /// </summary>
    let getAllKeys (s: string) =
        keysRegex.Matches(s)
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.map _.Value
        |> Seq.toList
        |> List.map ParameterKey.create


    /// <summary>
    /// Searches a string for parameter keys that are not present in the given parameter map
    /// </summary>
    let getMissingParameters (parameters: Map<ParameterKey, Parameter>) (text: string) =
        getAllKeys text |> List.filter (not << parameters.ContainsKey)
        
        
    /// <summary>
    /// Default/fallback option to print parameter values.
    /// </summary>
    /// <remarks>
    /// Since we use a discriminated union for the parameters not using this function and using "|> string" instead
    /// would print values like this: "Int 42"
    /// </remarks>
    let stringify = function
        | Parameter.Bool true -> "✔️"
        | Parameter.Bool false -> "❌"
        | Parameter.Float f -> f |> string
        | Parameter.Int i -> i |> string
        | Parameter.String s -> s

    
type NarrativeStyle =
    | Regular
    | Emphasized
    | Hint
    | Dialogue
    | System


/// <summary>
/// This represents a text resource that has not yet been resolved and filled with parameters.
/// </summary>
type Text = {
    ResourceKey: TextKey
    Intent: NarrativeStyle option
    Parameters: Map<ParameterKey, ParameterType> option
    ParameterFormatting: Map<ParameterKey, Parameter -> string> option
}
    

module Text =
    
    let create resourceKey =
        { ResourceKey = resourceKey; Intent = None; Parameters = None; ParameterFormatting = None }
        

    /// <summary>
    /// This text has been localized and all parameters have been replaced.
    /// </summary>
    type DisplayableText = {
        Text: string
        Intent: NarrativeStyle
    }
    
    
    type InsertParameterResult =
        | InsertSuccess
        | MissingParameters of ParameterKey
        | ParameterTypeMismatch of ParameterKey
        

    type ValidateParametersFailure = {
        Missing: ParameterKey list
        /// <summary>
        /// Describes a parameter type mismatch. First part of the tuple is the expected type,
        /// second is the given type and third is the key
        /// </summary>
        Mismatches: (ParameterType * ParameterType * ParameterKey) list
    }
        
        
    type ValidateParametersResult =
        | ValidateSuccess
        | Failure of ValidateParametersFailure
    
    
    type TextResources = Map<Language, Map<TextKey, string>>
    
    
    let resolve (resources: TextResources) (language: Language) (text: Text) : string =
        resources
        |> Map.tryFind language
        |> Option.bind (Map.tryFind text.ResourceKey)
        |> Option.defaultValue $"<text key '%s{text.ResourceKey |> TextKey.value}' missing for language %s{language |> Language.value}>"
        
    
    /// <summary>
    /// Checks whether the `given` map contains all values defined by the `expected` map
    /// </summary>
    /// <param name="expected">Map with all parameters (and types) that are expected</param>
    /// <param name="given">Map with all actual parameters and their values</param>
    let containsAll (expected: Map<ParameterKey, ParameterType>) (given: Map<ParameterKey, Parameter>) : ValidateParametersResult =
        let result =
            expected
            |> Map.toList
            |> List.fold
                   (fun acc (key, value) ->
                    match given |> Map.tryFind key with
                    | Some actualParameter ->
                        match value, actualParameter with
                        | ParameterType.Bool, Parameter.Bool _
                        | ParameterType.Float, Parameter.Float _
                        | ParameterType.Int, Parameter.Int _
                        | ParameterType.String, Parameter.String _ -> acc
                        | _ -> { acc with Mismatches = (value, actualParameter |> Parameters.getParameterType, key) :: acc.Mismatches }
                    | None ->
                        { acc with Missing = key :: acc.Missing }
                    )
                   { Missing = []; Mismatches = [] }
        if result.Missing.IsEmpty && result.Mismatches.IsEmpty then ValidateSuccess
        else result |> Failure


    /// <summary>
    /// Sets all parameters that can be set using the given inputs. If you want to check for missing/mismatching parameters
    /// use the <see cref="validate"/> function
    /// </summary>
    /// <param name="parameters"></param>
    /// <param name="formatters">Map of formatting options for </param>
    /// <param name="resolvedString"></param>
    let withParameters (parameters: Map<ParameterKey, Parameter>) (formatters: Map<ParameterKey, Parameter -> string>) (resolvedString: string) =
        let getMapper (key: ParameterKey) =
            formatters |> Map.tryFind key |> Option.defaultValue Parameters.stringify
        parameters
        |> Map.fold (fun (acc: string) (key: ParameterKey) (param: Parameter) -> acc.Replace($"{{{key}}}", param |> (key |> getMapper))) resolvedString
            
            
    let toDisplayable resources language text parameters =
        let parameters = parameters |> Option.defaultValue Map.empty
        let formatting = text.ParameterFormatting |> Option.defaultValue Map.empty
        let parameterizedString =
            text
            |> resolve resources language
            |> withParameters parameters formatting
            
        match parameters |> containsAll (text.Parameters |> Option.defaultValue Map.empty) with
        | ValidateSuccess -> Ok { Text = parameterizedString; Intent = text.Intent |> Option.defaultValue
                                                                                          NarrativeStyle.Regular } 
        | Failure validateParametersFailure ->
            let mergedMissing =
                if validateParametersFailure.Missing.IsEmpty then String.Empty
                else " <> " + String.Join(',', validateParametersFailure.Missing |> Seq.map string)
            let mergedMismatches =
                if validateParametersFailure.Mismatches.IsEmpty then String.Empty
                else
                    " <> " + 
                    (validateParametersFailure.Mismatches
                    |> List.fold (fun acc (expected, actual, key) ->
                            let toAppend = $"%s{key |> ParameterKey.value} mismatch: expected %A{expected} got %A{actual}"
                            if acc.Length = 0 then toAppend else $"%s{acc}, %s{toAppend}") String.Empty)
            Error { Text = $"%s{parameterizedString}%s{mergedMissing}%s{mergedMismatches}"; Intent = System }

    
    let private toDomainResources (raw: Map<string, Map<string, string>>) : TextResources =
        raw 
        |> Map.toList
        |> List.map (fun (lang, keys) -> 
            let typedKeys = 
                keys 
                |> Map.toList 
                |> List.map (fun (k, v) -> (TextKey.create k, v))
                |> Map.ofList
            (Language.create lang, typedKeys))
        |> Map.ofList


    let loadResourceFromFile (filename: string) : Result<TextResources, string> =
        if not (filename |> System.IO.File.Exists) then (Error $"The given resource file '%s{filename}' does not exist")
        else
            let readFileAction filename =
                try Ok (filename |> System.IO.File.ReadAllText)
                with exn -> Error $"Could not read file '%s{filename}' because: %s{exn.Message}"

            let parseContentAction (content: string) =
                    try
                        let deserialized = content |> System.Text.Json.JsonSerializer.Deserialize<Map<string, Map<string, string>>>
                        Ok (deserialized |> toDomainResources)
                    with exn -> Error $"Could not parse content of file '%s{filename}' because: %s{exn.Message}"
                
            filename
            |> readFileAction
            |> Result.bind parseContentAction
            