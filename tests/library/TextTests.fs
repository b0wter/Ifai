module Ifai.Library.Tests.TextTests

open System
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers

open Ifai.Lib

type TestUnion = First | Second of int | Third of string

[<Fact>]
let ``Transforming a non-parameterized text resource into a displayable text should return the proper string`` () =
    let text = Text.create ("key" |> TextKey.create)
    let language = "eng" |> Language.create
    let resources =
        [(
            language,
            [ ("key" |> TextKey.create, "sample text") ] |> Map.ofList
        )] |> Map.ofList

    let asDisplayable = Text.toDisplayable resources language text None

    asDisplayable |> should be (ofCase <@ Result<Text.DisplayableText, Text.DisplayableText>.Ok @>)
    (asDisplayable |> Ifai.Library.Tests.Helpers.Result.forceOk).Text |> should equal "sample text"
    (asDisplayable |> Ifai.Library.Tests.Helpers.Result.forceOk).Intent |> should equal NarrativeStyle.Regular


[<Fact>]
let ``Transforming a parameterized text resource into a displayable text using valid parameters returns proper string`` () =
    let text = Text.create ("key" |> TextKey.create)
    let language = "eng" |> Language.create
    let resources =
        [(
            language,
            [ ("key" |> TextKey.create, "{count} apples") ] |> Map.ofList
        )] |> Map.ofList

    let asDisplayable =
        Text.toDisplayable
            resources
            language
            text
            ([("count" |> ParameterKey.create, 4 |> Parameter.Int)] |> Map.ofList |> Some)

    asDisplayable |> should be (ofCase <@ Result<Text.DisplayableText, Text.DisplayableText>.Ok @>)
    (asDisplayable |> Ifai.Library.Tests.Helpers.Result.forceOk).Text |> should equal "sample text"
    (asDisplayable |> Ifai.Library.Tests.Helpers.Result.forceOk).Intent |> should equal NarrativeStyle.Regular

