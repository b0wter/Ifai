namespace Ifai.Lib

type RoomId = private RoomId of string
module RoomId =
    let create g = g |> RoomId 
    let value (RoomId r) = r

type CharacterId = CharacterId of string
module CharacterId =
    let create id = CharacterId id
    let value (CharacterId id) = id

type SpellId = SpellId of string
module SpellId =
    let create id = SpellId id
    let value (SpellId id) = id

type ThingId = ThingId of string
module ThingId =
    let create id = id |> ThingId
    let value (ThingId i) = i

type SpellInstanceId = SpellInstanceId of string
module SpellInstanceId =
    let create id = id |> SpellInstanceId
    let value (SpellInstanceId i) = i
