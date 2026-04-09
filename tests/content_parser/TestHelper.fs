module TestHelper

open System.IO

let readTestData (fileName: string) =
    File.ReadAllText(Path.Combine("TestData", fileName))

let readFragment (fileName: string) =
    File.ReadAllText(Path.Combine("TestData", "fragments", fileName))
