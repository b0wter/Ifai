module Ifai.Lib.Directions

/// Represents cardinal and intermediate directions as well as vertical directions.
type Direction =
    | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    | Up | Down | Left | Right

/// In order to allow custom exits (like "behind curtain") we wrap the directions
type Exit =
    | Dir of Direction
    | Custom of string