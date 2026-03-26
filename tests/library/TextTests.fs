module Ifai.Library.Tests.TextTests

open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers

open Ifai.Lib
open Ifai.Library.Tests.Helpers


// ---------------------------------------------------------------------------
// Parameters.getAllKeys
// ---------------------------------------------------------------------------

[<Fact>]
let ``getAllKeys with empty string returns empty list`` () =
    Parameters.getAllKeys "" |> should be Empty

[<Fact>]
let ``getAllKeys with no placeholders returns empty list`` () =
    Parameters.getAllKeys "plain text" |> should be Empty

[<Fact>]
let ``getAllKeys with single placeholder returns it`` () =
    Parameters.getAllKeys "{name}"
    |> should equal [ ParameterKey.create "name" ]

[<Fact>]
let ``getAllKeys with multiple placeholders returns all`` () =
    Parameters.getAllKeys "{a} and {b}"
    |> should equal [ ParameterKey.create "a"; ParameterKey.create "b" ]

[<Fact>]
let ``getAllKeys with duplicate placeholders returns duplicates`` () =
    Parameters.getAllKeys "{a} {a}"
    |> should equal [ ParameterKey.create "a"; ParameterKey.create "a" ]

[<Fact>]
let ``getAllKeys with adjacent placeholders returns both`` () =
    Parameters.getAllKeys "{a}{b}"
    |> should equal [ ParameterKey.create "a"; ParameterKey.create "b" ]


// ---------------------------------------------------------------------------
// Parameters.getMissingParameters
// ---------------------------------------------------------------------------

[<Fact>]
let ``getMissingParameters with all present returns empty list`` () =
    let params' = [ (ParameterKey.create "a", Parameter.Int 1) ] |> Map.ofList
    Parameters.getMissingParameters params' "value is {a}"
    |> should be Empty

[<Fact>]
let ``getMissingParameters with one missing returns it`` () =
    let params' = Map.empty
    Parameters.getMissingParameters params' "value is {a}"
    |> should equal [ ParameterKey.create "a" ]

[<Fact>]
let ``getMissingParameters with multiple missing returns all`` () =
    let params' = Map.empty
    Parameters.getMissingParameters params' "{a} and {b}"
    |> should equal [ ParameterKey.create "a"; ParameterKey.create "b" ]

[<Fact>]
let ``getMissingParameters with no placeholders returns empty`` () =
    let params' = [ (ParameterKey.create "a", Parameter.Int 1) ] |> Map.ofList
    Parameters.getMissingParameters params' "no placeholders"
    |> should be Empty


// ---------------------------------------------------------------------------
// Parameters.stringify
// ---------------------------------------------------------------------------

[<Fact>]
let ``stringify Bool true returns checkmark`` () =
    Parameters.stringify (Parameter.Bool true) |> should equal "✔️"

[<Fact>]
let ``stringify Bool false returns X`` () =
    Parameters.stringify (Parameter.Bool false) |> should equal "❌"

[<Fact>]
let ``stringify Int returns string representation`` () =
    Parameters.stringify (Parameter.Int 42) |> should equal "42"

[<Fact>]
let ``stringify Float returns string representation`` () =
    Parameters.stringify (Parameter.Float 3.14) |> should equal "3.14"

[<Fact>]
let ``stringify String returns identity`` () =
    Parameters.stringify (Parameter.String "hello") |> should equal "hello"


// ---------------------------------------------------------------------------
// Parameters.getParameterType
// ---------------------------------------------------------------------------

[<Fact>]
let ``getParameterType maps Bool to Bool`` () =
    Parameters.getParameterType (Parameter.Bool true) |> should equal ParameterType.Bool

[<Fact>]
let ``getParameterType maps Int to Int`` () =
    Parameters.getParameterType (Parameter.Int 0) |> should equal ParameterType.Int

[<Fact>]
let ``getParameterType maps Float to Float`` () =
    Parameters.getParameterType (Parameter.Float 0.0) |> should equal ParameterType.Float

[<Fact>]
let ``getParameterType maps String to String`` () =
    Parameters.getParameterType (Parameter.String "") |> should equal ParameterType.String


// ---------------------------------------------------------------------------
// Text.localize
// ---------------------------------------------------------------------------

let private mkResources lang entries : Text.TextResources =
    [ (Language.create lang, entries |> List.map (fun (k, v) -> (TextKey.create k, v)) |> Map.ofList) ] |> Map.ofList

[<Fact>]
let ``localize with existing key returns localized text`` () =
    let resources = mkResources "en" [ ("greet", "Hello") ]
    let text = Text.create (TextKey.create "greet")
    let result = Text.localize resources (Language.create "en") text
    result.Text |> should equal "Hello"

[<Fact>]
let ``localize with missing key returns fallback with System style`` () =
    let resources = mkResources "en" []
    let text = Text.create (TextKey.create "missing")
    let result = Text.localize resources (Language.create "en") text
    result.NarrativeStyle |> should equal NarrativeStyle.System
    result.Text |> should haveSubstring "missing"

[<Fact>]
let ``localize with missing language returns fallback`` () =
    let resources = mkResources "en" [ ("key", "Hello") ]
    let text = Text.create (TextKey.create "key")
    let result = Text.localize resources (Language.create "de") text
    result.NarrativeStyle |> should equal NarrativeStyle.System

[<Fact>]
let ``localize with no NarrativeStyle defaults to Regular`` () =
    let resources = mkResources "en" [ ("key", "text") ]
    let text = Text.create (TextKey.create "key")
    let result = Text.localize resources (Language.create "en") text
    result.NarrativeStyle |> should equal NarrativeStyle.Regular

[<Fact>]
let ``localize preserves explicit NarrativeStyle`` () =
    let resources = mkResources "en" [ ("key", "text") ]
    let text = { Text.create (TextKey.create "key") with NarrativeStyle = Some NarrativeStyle.Dialogue }
    let result = Text.localize resources (Language.create "en") text
    result.NarrativeStyle |> should equal NarrativeStyle.Dialogue

[<Fact>]
let ``localize carries through Parameters`` () =
    let resources = mkResources "en" [ ("key", "{n} items") ]
    let expectedParams = [ (ParameterKey.create "n", ParameterType.Int) ] |> Map.ofList |> Some
    let text = { Text.create (TextKey.create "key") with Parameters = expectedParams }
    let result = Text.localize resources (Language.create "en") text
    result.Parameters |> should equal expectedParams


// ---------------------------------------------------------------------------
// Text.containsAll
// ---------------------------------------------------------------------------

[<Fact>]
let ``containsAll with matching types returns ValidateSuccess`` () =
    let expected = [ (ParameterKey.create "a", ParameterType.Int) ] |> Map.ofList
    let given = [ (ParameterKey.create "a", Parameter.Int 1) ] |> Map.ofList
    Text.containsAll expected given |> should equal Text.ValidateSuccess

[<Fact>]
let ``containsAll with empty expected returns ValidateSuccess`` () =
    let given = [ (ParameterKey.create "a", Parameter.Int 1) ] |> Map.ofList
    Text.containsAll Map.empty given |> should equal Text.ValidateSuccess

[<Fact>]
let ``containsAll with missing parameter returns Failure`` () =
    let expected = [ (ParameterKey.create "a", ParameterType.Int) ] |> Map.ofList
    let result = Text.containsAll expected Map.empty
    match result with
    | Text.Failure f ->
        f.Missing |> should contain (ParameterKey.create "a")
        f.Mismatches |> should be Empty
    | _ -> failwith "Expected Failure"

[<Fact>]
let ``containsAll with type mismatch returns Failure with Mismatches`` () =
    let expected = [ (ParameterKey.create "a", ParameterType.Int) ] |> Map.ofList
    let given = [ (ParameterKey.create "a", Parameter.String "nope") ] |> Map.ofList
    let result = Text.containsAll expected given
    match result with
    | Text.Failure f ->
        f.Missing |> should be Empty
        f.Mismatches |> should not' (be Empty)
        let (expectedType, actualType, key) = f.Mismatches |> List.head
        expectedType |> should equal ParameterType.Int
        actualType |> should equal ParameterType.String
        key |> should equal (ParameterKey.create "a")
    | _ -> failwith "Expected Failure"

[<Fact>]
let ``containsAll with both missing and mismatched returns Failure with both`` () =
    let expected =
        [ (ParameterKey.create "a", ParameterType.Int)
          (ParameterKey.create "b", ParameterType.Bool) ] |> Map.ofList
    let given = [ (ParameterKey.create "a", Parameter.String "wrong") ] |> Map.ofList
    let result = Text.containsAll expected given
    match result with
    | Text.Failure f ->
        f.Missing |> should not' (be Empty)
        f.Mismatches |> should not' (be Empty)
    | _ -> failwith "Expected Failure"


// ---------------------------------------------------------------------------
// Text.mergeParameters
// ---------------------------------------------------------------------------

[<Fact>]
let ``mergeParameters with no parameters needed returns Ok`` () =
    let resources = mkResources "en" [ ("key", "plain text") ]
    let localized = Text.localize resources (Language.create "en") (Text.create (TextKey.create "key"))
    let result = Text.mergeParameters None localized
    result |> should be (ofCase <@ Result<Text.DisplayableText, Text.DisplayableText>.Ok @>)
    (result |> Result.forceOk).Text |> should equal "plain text"

[<Fact>]
let ``mergeParameters substitutes single parameter`` () =
    let resources = mkResources "en" [ ("key", "{count} apples") ]
    let localized = Text.localize resources (Language.create "en") (Text.create (TextKey.create "key"))
    let params' = [ (ParameterKey.create "count", Parameter.Int 4) ] |> Map.ofList |> Some
    let result = Text.mergeParameters params' localized
    (result |> Result.forceOk).Text |> should equal "4 apples"

[<Fact>]
let ``mergeParameters substitutes multiple parameters`` () =
    let resources = mkResources "en" [ ("key", "{who} has {count} apples") ]
    let localized = Text.localize resources (Language.create "en") (Text.create (TextKey.create "key"))
    let params' =
        [ (ParameterKey.create "who", Parameter.String "Alice")
          (ParameterKey.create "count", Parameter.Int 3) ] |> Map.ofList |> Some
    let result = Text.mergeParameters params' localized
    (result |> Result.forceOk).Text |> should equal "Alice has 3 apples"

[<Fact>]
let ``mergeParameters with missing parameter returns Error`` () =
    let resources = mkResources "en" [ ("key", "{a} and {b}") ]
    let expectedParams =
        [ (ParameterKey.create "a", ParameterType.Int)
          (ParameterKey.create "b", ParameterType.Int) ] |> Map.ofList |> Some
    let text = { Text.create (TextKey.create "key") with Parameters = expectedParams }
    let localized = Text.localize resources (Language.create "en") text
    let params' = [ (ParameterKey.create "a", Parameter.Int 1) ] |> Map.ofList |> Some
    let result = Text.mergeParameters params' localized
    result |> should be (ofCase <@ Result<Text.DisplayableText, Text.DisplayableText>.Error @>)
    (result |> Result.forceError).NarrativeStyle |> should equal NarrativeStyle.System

[<Fact>]
let ``mergeParameters with type mismatch returns Error`` () =
    let resources = mkResources "en" [ ("key", "{a}") ]
    let expectedParams = [ (ParameterKey.create "a", ParameterType.Int) ] |> Map.ofList |> Some
    let text = { Text.create (TextKey.create "key") with Parameters = expectedParams }
    let localized = Text.localize resources (Language.create "en") text
    let params' = [ (ParameterKey.create "a", Parameter.String "wrong") ] |> Map.ofList |> Some
    let result = Text.mergeParameters params' localized
    result |> should be (ofCase <@ Result<Text.DisplayableText, Text.DisplayableText>.Error @>)
    (result |> Result.forceError).Text |> should haveSubstring "mismatch"

[<Fact>]
let ``mergeParameters uses custom formatter when provided`` () =
    let resources = mkResources "en" [ ("key", "{n} items") ]
    let customFormatter = fun (p: Parameter) ->
        match p with
        | Parameter.Int i -> $"[%d{i}]"
        | _ -> Parameters.stringify p
    let formatting = [ (ParameterKey.create "n", customFormatter) ] |> Map.ofList |> Some
    let text = { Text.create (TextKey.create "key") with ParameterFormatting = formatting }
    let localized = Text.localize resources (Language.create "en") text
    let params' = [ (ParameterKey.create "n", Parameter.Int 5) ] |> Map.ofList |> Some
    let result = Text.mergeParameters params' localized
    (result |> Result.forceOk).Text |> should equal "[5] items"

[<Fact>]
let ``mergeParameters preserves NarrativeStyle on success`` () =
    let resources = mkResources "en" [ ("key", "text") ]
    let text = { Text.create (TextKey.create "key") with NarrativeStyle = Some NarrativeStyle.Hint }
    let localized = Text.localize resources (Language.create "en") text
    let result = Text.mergeParameters None localized
    (result |> Result.forceOk).NarrativeStyle |> should equal NarrativeStyle.Hint
