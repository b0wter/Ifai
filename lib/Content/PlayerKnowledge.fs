namespace Ifai.Lib.Content

open Ifai.Lib

type PlayerKnowledge = {
    DiscoveredThing: Set<ThingId>
    DiscoveredCharacters: Set<CharacterId>
    DiscoveredRooms: Set<RoomId>
}

module PlayerKnowledge =
    let empty = {
        DiscoveredThing = Set.empty
        DiscoveredCharacters = Set.empty
        DiscoveredRooms = Set.empty
    }
    
    let markRoomAsDiscovered (id: RoomId) (knowledge: PlayerKnowledge) : PlayerKnowledge =
        { knowledge with DiscoveredRooms = knowledge.DiscoveredRooms |> Set.add id }
    
    let markCharacterAsDiscovered (id: CharacterId) (knowledge: PlayerKnowledge) : PlayerKnowledge =
        { knowledge with DiscoveredCharacters = knowledge.DiscoveredCharacters |> Set.add id }
    
    let markThingAsDiscovered (id: ThingId) (knowledge: PlayerKnowledge) : PlayerKnowledge =
        { knowledge with DiscoveredThing = knowledge.DiscoveredThing |> Set.add id }
