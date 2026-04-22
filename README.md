# Ifai

A text adventure game engine and runtime.

## Project Structure

### Core Library (`lib/Ifai.Lib`)
The foundation of the system. Contains:
- **Game state models**: `Model`, `GameMode`, `World`, `Room`, `Character`, `Thing`, `Interaction`, `Scene`
- **Content definitions**: Room, Character, Thing, Spell, Trait, Interaction, Scene entities
- **Runtime logic**: Game modes (Exploring, Loading, Saving, Transitioning), parsing, built-in commands (save, quit)
- **Shared types**: Directions, connections, visibility states

### AI Engine (`ai/Ifai.Ai`)
AI integration layer. Provides:
- `AiClient`: Interface to AI models (currently supports Ollama)
- Chat history management
- System message configuration
- Async prompt processing

### Content Parser (`content_parser/Ifai.ContentParser`)
Parses game content files (`.ifa` format). Contains:
- **LineParser**: Parses indented game content files
- **BlockParser**: Handles structured content blocks (rooms, things, interactions)
- **DomainMapper**: Maps parsed content to domain models
- **Validation**: Validates ID uniqueness and interaction parameters

### Runtime (`runtime/Ifai.Runtime`)
Game engine execution. Written in F# but designed for easy C# interop. Provides:
- `Engine`: Main game loop and event handling (F# module)
- **InteropTypes**: C#-friendly POCO classes for seamless C# consumption:
   - `GameStateInfo`, `RoomInfo`, `CharacterInfo`, `ThingInfo`, `ConnectionInfo`, `PlayerInfo`
- **InteropMappers**: Maps internal F# game state to C#-compatible types
- `Interop.Api.run()`: Export function returning `Engine` with C#-compatible message types
   - C# code can call `engine.Input.Send()` and subscribe to `engine.Output`
   - No F# types leak into C# code
- File I/O operations for saving/loading games

### Dummies (`dummies/Ifai.Dummies`)
Sample/demo content. Contains:
- `World`: Sample world initialization
- `Rooms`: Three dummy test rooms
- `Texts`: English and German text resources

### CLI Application (`cli/Ifa.Cli`)
Command-line interface. Provides:
- Text-based game interface using Spectre.Console
- Input/output handling
- Sample game execution with dummy content

### GUI Application (`gui/Ifai.Gui`)
Desktop GUI application. Provides:
- Avalonia-based cross-platform GUI
- MainWindowViewModel for game display
- Interactive text adventure interface

### SignalR Connection (`connections/signalr_host`)
Server-side SignalR hub. Provides:
- Real-time multiplayer support
- Integration with game runtime
- C# client with F# backend

### SignalR Client (`connections/signalr_client`)
Client-side SignalR hub client. Provides:
- Real-time communication with SignalR server
- Remote game state synchronization

### Tests (`tests/ai/Ifai.Ai.Tests`)
Unit tests. Contains:
- Tests for `AiClient` AI integration
- Uses xUnit and FakeItEasy
