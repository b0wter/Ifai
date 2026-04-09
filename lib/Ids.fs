namespace Ifai.Lib

open System

type RoomId = RoomId of string
module RoomId =
    let create id =
        if id |> String.IsNullOrWhiteSpace
        then failwith "Cannot create room id from empty string"
        else id |> RoomId 
    let value (RoomId r) = r

type CharacterId = CharacterId of string
module CharacterId =
    let create id =
        if id |> String.IsNullOrWhiteSpace
        then failwith "Cannot create character id from empty string"
        else id |> CharacterId
    let value (CharacterId id) = id

type SpellId = SpellId of string
module SpellId =
    let create id =
        if id |> String.IsNullOrWhiteSpace
        then failwith "Cannot create spell id from empty string"
        else id |> SpellId
    let value (SpellId id) = id

type ThingId = ThingId of string
module ThingId =
    let create id =
        if id |> String.IsNullOrWhiteSpace
        then failwith "Cannot create thing id from empty string"
        else id |> ThingId
    let value (ThingId i) = i

type SpellInstanceId = SpellInstanceId of string
module SpellInstanceId =
    let create id =
        if id |> String.IsNullOrWhiteSpace
        then failwith "Cannot create spell-instance id from empty string"
        else id |> SpellInstanceId
    let value (SpellInstanceId i) = i

type MetaId =
    | WrappedRoomId of RoomId
    | WrappedCharacterId of CharacterId
    | WrappedSpellId of SpellId
    | WrappedThingId of ThingId
    | WrappedSpellInstanceId of SpellInstanceId
module MetaId =
    let value (m: MetaId) =
        match m with
        | WrappedRoomId r -> r |> RoomId.value
        | WrappedCharacterId c -> c |> CharacterId.value
        | WrappedSpellId s -> s |> SpellId.value
        | WrappedThingId t -> t |> ThingId.value
        | WrappedSpellInstanceId i -> i |> SpellInstanceId.value