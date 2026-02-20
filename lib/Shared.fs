module Ifai.Lib.Shared

type Visibility =
    | Visible
    | Hidden
    | Concealed
    
    
type AttributeValue =
    | Bool of bool
    | Int of value:int * min:int * max:int
    | UInt of value:uint * min:uint * max:uint
    | Float of float
    | String of string


type AttributeId = AttributeId of string


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
    | Flooded


type PressureState =
    | Pressurized
    | Depressurized


type FloodingState =
    | NotFlooded
    | Flooded
    | Rising
    | Lowering


    