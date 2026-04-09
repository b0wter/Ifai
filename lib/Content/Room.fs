namespace Ifai.Lib.Content

open Ifai.Lib
open Ifai.Lib.Shared

type RoomEvent =
    | Nothing
    | Batch of RoomEvent list
module RoomEvent =
    let create e = Batch [e]


/// <summary>
/// Rooms are stored in their initial state and do not change over time.
/// <c>RoomModifiers</c> are used to modify the room state. They also include properties that are uncommon
/// </summary>
type RoomModifier =
    /// Describes a room that is currently burning or already fully burnt
    | Burning of BurningModifier
    /// Describes a room that is currently flooding
    | Flooding of FloodingModifier
    /// Describes a room that is currently radioactive
    | Radioactive of level:uint
    /// Describes a room that has been cursed
    | Cursed of potency:uint
    /// Describes a room that has been blessed
    | Blessed of potency:uint
    /// Allows authors to add custom attributes to rooms
    | Custom of name:Shared.AttributeId * value:AttributeValue

module RoomModifier =
    let private modifierAsString = function
        | Burning _ -> "burning"
        | Flooding _ -> "flooding"
        | Radioactive _ -> "radioactive"
        | Cursed _ -> "cursed"
        | Blessed _ -> "blessed"
        | Custom (id, _) -> id |> AttributeId.value
        
    let private mergeBurning (a: BurningModifier) (b: BurningModifier) : BurningModifier =
        match a, b with
        | BurningModifier.Burning a1, BurningModifier.Burning b1 -> BurningModifier.Burning (a1 + b1)
        | BurntOut, _
        | _, BurntOut -> BurntOut

    let private mergeFlooding (a: FloodingModifier) (b: FloodingModifier) : Result<FloodingModifier, string> =
        match a, b with
        | FloodingModifier.PartlyFlooded _, FloodingModifier.Flooded
        | FloodingModifier.Flooded, FloodingModifier.PartlyFlooded _ -> Error "Cannot merge Flooded and PartyFlooded state"
        | PartlyFlooded a1, PartlyFlooded b1 when a1 = b1 -> PartlyFlooded a1 |> Ok
        | PartlyFlooded a1, PartlyFlooded b1 -> Error "Cannot merge two partly-flooded states with different flooding percentages"
        | FloodingModifier.Flooded, FloodingModifier.Flooded -> FloodingModifier.Flooded |> Ok

    let private mergeFloodingDelta (a: FloodingDeltaModifier) (b: FloodingDeltaModifier) =
        match a, b with
        | FloodingDeltaModifier.Rising r1, FloodingDeltaModifier.Rising r2 ->
            FloodingDeltaModifier.Rising (r1 + r2)
        | FloodingDeltaModifier.Rising r1, FloodingDeltaModifier.Lowering r2 ->
            if r1 > r2 then FloodingDeltaModifier.Rising (r1 - r2) else FloodingDeltaModifier.Lowering (r2 - r1)
        | FloodingDeltaModifier.Lowering r1, FloodingDeltaModifier.Rising r2 ->
            if r1 > r2 then FloodingDeltaModifier.Lowering (r1 - r2) else FloodingDeltaModifier.Rising (r2 - r1)
        | FloodingDeltaModifier.Lowering r1, FloodingDeltaModifier.Lowering r2 ->
            FloodingDeltaModifier.Lowering (r1 + r2)

    let private mergePressure (a: PressureState) (b: PressureState) =
        match a, b with
        | Pressurising(rate, percent), Pressurising(rate1, percent1) -> failwith "todo"
        | Pressurising(rate, percent), Pressurized -> failwith "todo"
        | Pressurising(rate, percent), Depressurising(rate1, percent1) -> failwith "todo"
        | Pressurising(rate, percent), Depressurized -> failwith "todo"
        | Pressurising(rate, percent), Overpressurized percent1 -> failwith "todo"
        | Pressurising(rate, percent), Underpressurized percent1 -> failwith "todo"
        | Pressurized, Pressurising(rate, percent) -> failwith "todo"
        | Pressurized, Pressurized -> failwith "todo"
        | Pressurized, Depressurising(rate, percent) -> failwith "todo"
        | Pressurized, Depressurized -> failwith "todo"
        | Pressurized, Overpressurized percent -> failwith "todo"
        | Pressurized, Underpressurized percent -> failwith "todo"
        | Depressurising(rate, percent), Pressurising(rate1, percent1) -> failwith "todo"
        | Depressurising(rate, percent), Pressurized -> failwith "todo"
        | Depressurising(rate, percent), Depressurising(rate1, percent1) -> failwith "todo"
        | Depressurising(rate, percent), Depressurized -> failwith "todo"
        | Depressurising(rate, percent), Overpressurized percent1 -> failwith "todo"
        | Depressurising(rate, percent), Underpressurized percent1 -> failwith "todo"
        | Depressurized, Pressurising(rate, percent) -> failwith "todo"
        | Depressurized, Pressurized -> failwith "todo"
        | Depressurized, Depressurising(rate, percent) -> failwith "todo"
        | Depressurized, Depressurized -> failwith "todo"
        | Depressurized, Overpressurized percent -> failwith "todo"
        | Depressurized, Underpressurized percent -> failwith "todo"
        | Overpressurized percent, Pressurising(rate, percent1) -> failwith "todo"
        | Overpressurized percent, Pressurized -> failwith "todo"
        | Overpressurized percent, Depressurising(rate, percent1) -> failwith "todo"
        | Overpressurized percent, Depressurized -> failwith "todo"
        | Overpressurized percent, Overpressurized percent1 -> failwith "todo"
        | Overpressurized percent, Underpressurized percent1 -> failwith "todo"
        | Underpressurized percent, Pressurising(rate, percent1) -> failwith "todo"
        | Underpressurized percent, Pressurized -> failwith "todo"
        | Underpressurized percent, Depressurising(rate, percent1) -> failwith "todo"
        | Underpressurized percent, Depressurized -> failwith "todo"
        | Underpressurized percent, Overpressurized percent1 -> failwith "todo"
        | Underpressurized percent, Underpressurized percent1 -> failwith "todo"

    let merge (modifiers: RoomModifier list) : RoomModifier list =
        let grouped =
            modifiers
            |> List.groupBy modifierAsString


/// <summary>
/// The <c>RoomEnvironment</c> is the initial state of a room. It consists of common properties
/// that almost all rooms share. The room is modified by <see cref="Ifai.Lib.RoomModifier"/> which are not stored in the
/// room itself but the global state model
/// </summary>
type RoomEnvironment = {
    Temperature: int
    Pressure: PressureState
    GravityPercent: uint
    LightLevel: LightState
}
module RoomEnvironment =
    let ``default`` = {
        Temperature = 20
        Pressure = Pressurized
        GravityPercent = 100u
        LightLevel = LightState.Moderate
    }


// should room include modifiers? probably not... rooms should be static definitions of the initial state
// that can be modified by events


type Room = {
    Id: RoomId
    Name: Text.LocalizedText
    Description: Text.LocalizedText
    Connections: Map<Exit, Connection<RoomId>>
    OnEnter: RoomEvent
    OnLeaving: RoomEvent
    Environment: RoomEnvironment
}


module Room =
    let create id name desc =
        { Id = id
          Name = name
          Description = desc
          Connections = Map.empty
          Environment = RoomEnvironment.``default``
          OnEnter = RoomEvent.Nothing
          OnLeaving = RoomEvent.Nothing }
    
    let rename (r: Room) newName = { r with Name = newName }
    
    let name (r: Room) = r.Name
    let description (r: Room) = r.Description
    let connections r = r.Connections
    let environment r = r.Environment
    let onEnter r = r.OnEnter
    let onLeaving r = r.OnLeaving

    let listExits r =
        r.Connections
        |> Map.toList
