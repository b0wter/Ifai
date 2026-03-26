module Ifai.Lib.Conditions

open DynamicExpresso
open Ifai.Lib.Shared


/// Transforms an attribute value into an object so that it can be passed into a
/// DynamicExpresso interpreter
let attributeAsValue (v: AttributeValue) : obj =
    match v with
    | Bool value -> value :>obj 
    | String value -> value :>obj
    | Int(value, _, _) -> value :> obj
    | UInt(value, _, _) -> value :> obj
    | Float(value, _, _) -> value :> obj


let inline private tryIdAsString (x: ^T) : string option =
    match box x with
    | :? ThingId as ThingId s -> s |> Some
    | :? RoomId as RoomId s -> s |> Some
    | :? CharacterId as CharacterId s -> s |> Some
    | :? SpellId as SpellId s -> s |> Some
    | :? SpellInstanceId as SpellInstanceId s -> s |> Some
    | :? System.String -> x.ToString() |> Some
    | _ -> None


let inline private idAsString (x: ^T) : string =
    match x |> tryIdAsString with
    | Some i -> i
    | None ->
        failwithf $"Cannot get id value from '%A{x}'"


/// <summary>
/// Builds a lambda function that can be used to evaluate a condition expression
/// </summary>
/// <returns>
/// Returns a list of all identifiers in the expression and a function that can be used to evaluate the expression
/// </returns>
(*
let buildLambda<'a, 'b when 'b: comparison> (expression: string) : (string list * (Map<'b, AttributeValue> -> 'a))  =
    // this interpreter is only created once and thus must not be used in the lambda!
    let identifyParametersInterpreter = Interpreter()
    let allIdentifiers = identifyParametersInterpreter.DetectIdentifiers(expression)
    
    allIdentifiers.Identifiers
        |> Seq.map _.Name
        |> Seq.append allIdentifiers.UnknownIdentifiers
        |> List.ofSeq
    ,
    fun (parameters: Map<'b, AttributeValue>) ->
        let interpreter = Interpreter()
        do parameters |> Map.iter (fun key value ->
            interpreter.SetVariable(
                key |> idAsString,
                value |> attributeAsValue) |> ignore)
        interpreter.Eval(expression) :?> 'a
*)


let buildLambda (typedKnownParameters: Map<AttributeId, AttributeValue>) expression =
    let knownParameters =
        typedKnownParameters
        |> Seq.map (fun kvp -> kvp.Key |> AttributeId.value, kvp.Value)
        |> Map.ofSeq
    let interpreter = Interpreter()

    let ids = interpreter.DetectIdentifiers(expression)

    let names =
        ids.Identifiers |> Seq.map _.Name
        |> Seq.append ids.UnknownIdentifiers
        |> List.ofSeq
        
    let getAttributeType (id: string) =
        knownParameters
        |> Map.tryFind id
        |> Option.map (function Bool _ -> typeof<bool> | String _ -> typeof<string> | Int _ -> typeof<int> | UInt _ -> typeof<uint> | Float _ -> typeof<float>)
        |> Option.defaultValue typeof<obj>

    let parameters =
        names
        |> List.map (fun n -> Parameter(n, n |> getAttributeType))
        |> List.toArray

    let compiled =
        interpreter.Parse(expression, parameters)

    names,
    fun (values: Map<AttributeId, AttributeValue>) ->
        let args =
            names
            |> List.map (fun name ->
                values
                |> Map.find (AttributeId.create name)
                |> attributeAsValue)
            |> List.toArray

        compiled.Invoke(args) :?> 'a