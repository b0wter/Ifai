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


type FireState =
    | NotOnFire
    | OnFire of intensity:uint


type PressureState =
    | Pressurized
    | Depressurized


type FloodingState =
    | NotFlooded
    | Flooded
    | Rising
    | Lowering


    