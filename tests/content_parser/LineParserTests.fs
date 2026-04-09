module LineParserTests

open Ifai.ContentParser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Parse simple item line`` () =
    let line = "item:"
    let result = LineParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 0u; Key = "item"; Value = None; StartsWithDash = false }))

[<Fact>]
let ``Parse line with leading dash and spaces`` () =
    let line = "    - throw, toss:"
    let result = LineParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 4u; Key = "throw, toss"; Value = None; StartsWithDash = true }))

[<Fact>]
let ``Parse line with dash and key value`` () =
    let line = "        - target: decorations.underground_lake.lake"
    let result = LineParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 8u; Key = "target"; Value = Some "decorations.underground_lake.lake"; StartsWithDash = true }))

[<Fact>]
let ``Parse line with comment`` () =
    let line = "          destroy: this # will completely remove the pebble from the game"
    let result = LineParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 10u; Key = "destroy"; Value = Some "this"; StartsWithDash = false }))

[<Fact>]
let ``Parse string line`` () =
    let line = "    just some text"
    let result = LineParser.parseLine line
    result |> should equal (Some (ContentLine.StringLine "    just some text"))

[<Fact>]
let ``Parse whole in_front_of_house.ifa file`` () =
    let results = TestHelper.readTestData "in_front_of_house.ifa" |> LineParser.parse
    
    // Helper to find a ContentLine by Key
    let findLine key = 
        results |> List.find (function 
            | ContentLine.ComplexLine cl when cl.Key = key -> true 
            | _ -> false)
        |> function ContentLine.ComplexLine cl -> cl | _ -> failwith "Not a ContentLine"

    // Check some specific lines from the file
    // Line 1: room:
    results[0] |> should equal (ContentLine.ComplexLine { Indentation = 0u; Key = "room"; Value = None; StartsWithDash = false })
    
    // Line 2:   id: in_front_of_house
    results[1] |> should equal (ContentLine.ComplexLine { Indentation = 2u; Key = "id"; Value = Some "in_front_of_house"; StartsWithDash = false })
    
    // Line 9:     - haustür: rooms.hallway
    results[8] |> should equal (ContentLine.ComplexLine { Indentation = 4u; Key = "haustür"; Value = Some "rooms.hallway"; StartsWithDash = true })
    
    // Line 71:            target: items.door_key
    let targetLine = results |> List.find (function ContentLine.ComplexLine cl when cl.Key = "target" && cl.Value = Some "items.door_key" -> true | _ -> false)
    match targetLine with
    | ContentLine.ComplexLine cl -> cl.Indentation |> should equal 12u
    | _ -> failwith "Expected target line"

    // Line 12:    - riechen, schnuppern, schnüffeln:
    let riechenLine = findLine "riechen, schnuppern, schnüffeln"
    riechenLine |> should equal { Indentation = 4u; Key = "riechen, schnuppern, schnüffeln"; Value = None; StartsWithDash = true }

    // Check an unstructured line
    // Line 5:    Du stehst in einem adretten Garten ...
    let houseDescPart = results |> List.find (function ContentLine.StringLine s when s.Contains("Du stehst in einem adretten Garten") -> true | _ -> false)
    houseDescPart |> should equal (ContentLine.StringLine "    Du stehst in einem adretten Garten vor einem alten Haus. Der Wind weht leicht durch zwei kleine Kirschbäume und kitzelt dich an der Nase.")
