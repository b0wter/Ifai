namespace Ifai.Library

open System

module RoomIds =
    type RoomId = private RoomId of Guid
    let create g = g |> RoomId 
    let value (RoomId r) = r 

module RoomEvents =
    type RoomEvent =
        | Batch of RoomEvent list

module Rooms =
    type Room = {
        Id: RoomIds.RoomId
        Name: string
        Description: string
        Connections: Map<MovementDirection, RoomIds.RoomId>
        OnEnter: RoomEvents.RoomEvent option
        OnExit: RoomEvents.RoomEvent option
    }
