module Ifai.Lib.Shared

type Visibility =
    | Visible
    | Hidden
    | Concealed
    
    
type AttributeValue =
    | Bool of bool
    | Int of value:int * min:int * max:int
    | UInt of value:uint * min:uint * max:uint
    | Float of value:float * min:float * max:float
    | String of string


type AttributeId = AttributeId of string
module AttributeId =
    let create (s:string) = AttributeId s
    let value (AttributeId s) = s


type AttributeOperation =
    | Set of value:AttributeValue
    | Increment of int
    | Decrement of int
    | Remove


/// <summary>
/// Describes the burning state of a room.
/// </summary>
/// <remarks>
/// Does not contain a case for not being burning, as this is the default state.
/// </remarks>
type BurningModifier =
    | Burning of intensity:uint
    | BurntOut


/// <summary>
/// Describes the flooding state of a tile.
/// </summary>
/// <remarks>
/// Does not contain a case for not being flooded, as this is the default state.
/// </remarks>
type FloodingModifier =
    | Rising of rate:uint
    | Lowering of rate:uint
    | PartlyFlooded of percent:uint
    | Flooded


type PressureState =
    /// Pressure is currently increasing
    | Pressurising of rate:uint * percent:uint
    /// Pressure is at default state
    | Pressurized
    /// Pressure is currently decreasing
    | Depressurising of rate:uint * percent:uint
    /// Room is completely depressurized
    | Depressurized
    /// Room is overpressurized
    | Overpressurized of percent:uint
    /// Room is underpressurized
    | Underpressurized of percent:uint


type FloodingState =
    | NotFlooded
    | Flooded
    | Rising
    | Lowering


/// Applies a new modifier to a Set of already existing modifiers.
let applyDeltaModifier zero add clampBottom clampTop predicate extract constructor delta modifiers =
    let matching, others =
        modifiers |> Set.partition predicate

    let newValue =
        matching
        |> Set.fold (fun acc next -> add acc (extract next)) delta
        |> clampBottom
        |> clampTop
    
    if newValue = zero then others
    else others |> Set.add (constructor newValue)