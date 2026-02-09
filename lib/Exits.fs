namespace Ifai.Lib

/// Represents cardinal and intermediate directions as well as vertical directions.
type Direction =
    | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    | Up | Down | Left | Right


/// In order to allow custom exits (like "behind curtain") we wrap the directions
type Exit =
    | Dir of Direction
    | Custom of Text


module Exit =
    let asText (e: Exit) =
        match e with
        | Dir Direction.North -> Text.create (TextKey.create "north")
        | Dir Direction.South -> Text.create (TextKey.create "south")
        | Dir Direction.East -> Text.create (TextKey.create "east")
        | Dir Direction.West -> Text.create (TextKey.create "west")
        | Dir Direction.NorthEast -> Text.create (TextKey.create "northeast")
        | Dir Direction.NorthWest -> Text.create (TextKey.create "northwest")
        | Dir Direction.SouthEast -> Text.create (TextKey.create "southeast")
        | Dir Direction.SouthWest -> Text.create (TextKey.create "southwest")
        | Dir Direction.Up -> Text.create (TextKey.create "up")
        | Dir Direction.Down -> Text.create (TextKey.create "down")
        | Dir Direction.Left -> Text.create (TextKey.create "left")
        | Dir Direction.Right -> Text.create (TextKey.create "right")
        | Custom e -> e
