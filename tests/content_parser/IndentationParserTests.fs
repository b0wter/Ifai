module IndentationParserTests

open System
open System.IO
open Ifai.ContentParser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Parse simple item line`` () =
    let line = "item:"
    let result = IndentationParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 0u; Key = "item"; Value = None; StartsWithDash = false }))

[<Fact>]
let ``Parse line with leading dash and spaces`` () =
    let line = "    - throw, toss:"
    let result = IndentationParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 4u; Key = "throw, toss"; Value = None; StartsWithDash = true }))

[<Fact>]
let ``Parse line with dash and key value`` () =
    let line = "        - target: decorations.underground_lake.lake"
    let result = IndentationParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 8u; Key = "target"; Value = Some "decorations.underground_lake.lake"; StartsWithDash = true }))

[<Fact>]
let ``Parse line with comment`` () =
    let line = "          destroy: this # will completely remove the pebble from the game"
    let result = IndentationParser.parseLine line
    result |> should equal (Some (ContentLine.ComplexLine { Indentation = 10u; Key = "destroy"; Value = Some "this"; StartsWithDash = false }))

[<Fact>]
let ``Parse string line`` () =
    let line = "    just some text"
    let result = IndentationParser.parseLine line
    result |> should equal (Some (ContentLine.StringLine "    just some text"))

[<Fact>]
let ``Parse whole underground_lake.ifa file`` () =
    let path = Path.Combine("TestData", "underground_lake.ifa")
    let content = File.ReadAllText(path)
    let results = IndentationParser.parse content
    
    // Helper to find a ContentLine by Key
    let findLine key = 
        results |> List.find (function 
            | ContentLine.ComplexLine cl when cl.Key = key -> true 
            | _ -> false)
        |> function ContentLine.ComplexLine cl -> cl | _ -> failwith "Not a ContentLine"

    // Check some specific lines from the file
    // Line 1: room:
    results[0] |> should equal (ContentLine.ComplexLine { Indentation = 0u; Key = "room"; Value = None; StartsWithDash = false })
    
    // Line 2:   id: underground_lake
    results[1] |> should equal (ContentLine.ComplexLine { Indentation = 2u; Key = "id"; Value = Some "underground_lake"; StartsWithDash = false })
    
    // Line 5:     - north: rooms.cave_tunnel
    results[4] |> should equal (ContentLine.ComplexLine { Indentation = 4u; Key = "north"; Value = Some "rooms.cave_tunnel"; StartsWithDash = true })
    
    // Line 25:           destroy: this # will completely remove the pebble from the game
    let destroyLine = findLine "destroy"
    destroyLine |> should equal { Indentation = 10u; Key = "destroy"; Value = Some "this"; StartsWithDash = false }

    // Line 22:    - throw, toss:
    let throwLine = findLine "throw, toss"
    throwLine |> should equal { Indentation = 4u; Key = "throw, toss"; Value = None; StartsWithDash = true }

    // Check an unstructured line
    // Line 13:    You stand at the entrance to a cave. ...
    let caveDescPart = results |> List.find (function ContentLine.StringLine s when s.Contains("You stand at the entrance to a cave") -> true | _ -> false)
    caveDescPart |> should equal (ContentLine.StringLine "    You stand at the entrance to a cave. The entrance is dimly lit, small plants and brush cover the rock.")
