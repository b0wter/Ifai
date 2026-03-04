namespace Ifai.Lib

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
    | Burning of Shared.BurningModifier
    /// Describes a room that is currently flooding
    | Flooding of Shared.FloodingModifier
    /// Describes a room that is currently radioactive
    | Radioactive of level:uint
    /// Describes a room that has been cursed
    | Cursed of potency:uint
    /// Describes a room that has been blessed
    | Blessed of potency:uint
    /// Allows authors to add custom attributes to rooms
    | Custom of name:Shared.AttributeId * value:Shared.AttributeValue


/// <summary>
/// The <c>RoomEnvironment</c> is the initial state of a room. It consists of common properties
/// that almost all rooms share. The room is modified by <see cref="Ifai.Lib.RoomModifier"/> which are not stored in the
/// room itself but the global state model
/// </summary>
type RoomEnvironment = {
    Temperature: int
    Pressure: Shared.PressureState
    GravityPercent: uint
    LightLevel: LightState
}
module RoomEnvironment =
    let ``default`` = {
        Temperature = 20
        Pressure = Shared.Pressurized
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
    OnEnter: RoomEvent option
    OnLeaving: RoomEvent option
    Environment: RoomEnvironment
}


module Room =
    let create id name desc =
        { Id = id
          Name = name
          Description = desc
          Connections = Map.empty
          Environment = RoomEnvironment.``default``
          OnEnter = None
          OnLeaving = None }
    
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
