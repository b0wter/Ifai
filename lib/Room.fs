namespace Ifai.Lib

type RoomId = private RoomId of string
module RoomId =
    let create g = g |> RoomId 
    let value (RoomId r) = r


type RoomEvent =
    | Nothing
    | Batch of RoomEvent list


module RoomEvent =
    let create e = Batch [e]


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


type RoomModifiers =
    | Burning of intensity:uint
    | Flooded
    | Radioactive of level:uint
    | Cursed of potency:uint
    | Blessed of potency:uint


// should room include modifiers? probably not... rooms should be static definitions of the initial state
// that can be modified by events


type Room = {
    Id: RoomId
    Name: Text
    Description: Text
    Connections: Map<Exit, Connection<RoomId>>
    OnEnter: RoomEvent option
    OnLeaving: RoomEvent option
    // Modifiers: RoomModifiers list
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
    let onEnter r = r.OnEnter
    let onLeaving r = r.OnLeaving

    let listExits r =
        r.Connections
        |> Map.toList
