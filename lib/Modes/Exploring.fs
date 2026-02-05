module Ifai.Lib.Modes.Exploring

open Ifai.Lib
open System

(*
    This state is the well-known gameplay loop of exploring a room and interacting with it
*)

type ExploringBuiltIn =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
    | Up
    | Down
    | Left
    | Right
    | LookAround
    | Examine of string
    | Take of string
    | TakeAll
    
    
let exploringBuiltInPriority b =
    match b with 
    | North ->  1
    | NorthEast -> 1
    | East ->  1
    | SouthEast -> 1
    | South ->  1
    | SouthWest -> 1
    | West ->  1
    | NorthWest -> 1
    | Up ->  3
    | Down ->  2
    | Left ->  2
    | Right ->  2
    | LookAround -> 3
    | Examine _ -> 3
    | Take _ ->  3
    | TakeAll ->  3


let private english =
    [
    ["n"; "north"], fun _ -> North
    ["ne"; "northeast"], fun _ -> NorthEast
    ["e"; "east"], fun _ -> East
    ["se"; "southeast"], fun _ -> SouthEast
    ["s"; "south"], fun _ -> South
    ["sw"; "southwest"], fun _ -> SouthWest
    ["w"; "west"], fun _ -> West
    ["nw"; "northwest"], fun _ -> NorthWest
    ["u"; "up"], fun _ -> Up
    ["d"; "down"], fun _ -> Down
    ["l"; "left"], fun _ -> Left
    ["r"; "right"], fun _ -> Right
    // TODO: definition needs to be tighter because this would also match "look at foobar" which it is not meant for! That needs to match "Examine"
    // being able to explicitly allow/disallow arguments might solve this
    ["l"; "look"], fun _ -> LookAround
    // TODO: Replacing the option with an empty string is not the final solution!
    ["x"; "examine" ], fun argument -> Examine (argument |> Option.defaultValue String.Empty)
    // TODO: Replacing the option with an empty string is not the final solution!
    ["t"; "take"], fun argument -> Take (argument |> Option.defaultValue String.Empty)
    ["take all"; "takeall"; "get all"; "getall"], fun _ -> TakeAll
    ]
    
    
let private german =
    [
    ["n"; "nord"; "norden"], fun _ -> North
    ["no"; "nordost"; "nordosten"], fun _ -> NorthEast
    ["o"; "ost"; "osten"], fun _ -> East
    ["so"; "südost"; "südosten"], fun _ -> SouthEast
    ["s"; "süd"; "süden"], fun _ -> South
    ["sw"; "südwest"; "südwesten"], fun _ -> SouthWest
    ["w"; "west"; "westen"], fun _ -> West
    ["nw"; "nordwest"; "nordwesten"], fun _ -> NorthWest
    ["o"; "oben"], fun _ -> Up
    ["u"; "unten"], fun _ -> Down
    ["l"; "links"], fun _ -> Left
    ["r"; "rechts"], fun _ -> Right
    ["u"; "umgucken"; "umschauen"; "umsehen"], fun _ -> LookAround
    // TODO: Replacing the option with an empty string is not the final solution!
    ["g"; "a"; "gucken"; "angucken"; "anschauen"; "ansehen"; "x"; "untersuchen "; "unsersuche "], fun argument -> Examine (argument |> Option.defaultValue String.Empty)
    // TODO: Replacing the option with an empty string is not the final solution!
    ["n "; "nehme "; "nimm "], fun argument -> Take (argument |> Option.defaultValue String.Empty)
    ["nimm alles" ], fun _ -> TakeAll
    ]
    
    
let builtIns =
    [ Language.create "en", english
      Language.create "ger", german
    ] |> Map.ofList


type Input =
    | BuiltIn of ExploringBuiltIn list
    | Sentence of string


/// This represents the user's intent as extracted from the entered input
type ExploringIntent =
    | Move of Directions.Exit
    | Wait
    | LookAround
    | Examine of string
    | Take of string
    | TakeAll
    | Ignore of string
    | Unknown
    
    
type ExploringEvent =
    | NewRom
    | UserInput of Input
    | ResolvingRoomEvent of RoomEvent option


type ExploringState = {
    Foo: int
    // TODO: add meaningful fields
}


/// Turns raw text input into intent
let parser (language: Language) (input: string) : Input =
    let matches = input |> (Parser.tryParseBuiltIn builtIns language)
    if matches.IsEmpty then input |> Sentence
    else matches |> Input.BuiltIn


let matchInputWithIntent (input: ExploringBuiltIn) : ExploringIntent =
    match input with
    | North -> Move (Directions.Exit.Dir Directions.North)
    | NorthEast -> Move (Directions.Exit.Dir Directions.NorthEast)
    | East -> Move (Directions.Exit.Dir Directions.East)
    | SouthEast -> Move (Directions.Exit.Dir Directions.SouthEast)
    | South -> Move (Directions.Exit.Dir Directions.South)
    | SouthWest -> Move (Directions.Exit.Dir Directions.SouthWest)
    | West -> Move (Directions.Exit.Dir Directions.West)
    | NorthWest -> Move (Directions.Exit.Dir Directions.NorthWest)
    | Up -> Move (Directions.Exit.Dir Directions.Up)
    | Down -> Move (Directions.Exit.Dir Directions.Down)
    | Left -> Move (Directions.Exit.Dir Directions.Left)
    | Right -> Move (Directions.Exit.Dir Directions.Right)
    | ExploringBuiltIn.LookAround -> LookAround
    | ExploringBuiltIn.Take x -> Take x
    | ExploringBuiltIn.Examine x -> Examine x
    | ExploringBuiltIn.TakeAll -> TakeAll


let resolveUserIntent (world: World) (state: ExploringState) (input: Input) : ExploringIntent =
    match input with
    | BuiltIn builtIns ->
        // TODO: since we have multiple intents we can do a lot better than the following code
        //       we can ask the user for clarification and so on
        builtIns |> List.sortBy exploringBuiltInPriority |> List.tryHead |> Option.map matchInputWithIntent |> Option.defaultValue Unknown
    | Input.Sentence _ -> ExploringIntent.Ignore "sentence input is not yet supported"


let handleIntent (world: World) (state: ExploringState) (intent: ExploringIntent) : StepResult<ExploringState> =
    match intent with
    | Move exit ->
        match world |> World.getRoomIdForExit exit with
        | Some nextRoomId ->
            failwith "implement transition mode here :D"
        | None ->
            StepResult.init world state
            |> StepResult.withRender (RenderAction.Text (Text.create (TextKey.create "no_exit_found")))
    | LookAround ->
        StepResult.init world state
        |> StepResult.withRender (RenderAction.Fallback "Looking around in the exploring mode has not been implemented yet")
    | Examine item ->
        StepResult.init world state
        |> StepResult.withRender (RenderAction.Fallback "Examining in the exploring mode has not been implemented yet")
    | Take item ->
        StepResult.init world state
        |> StepResult.withRender (RenderAction.Fallback "Taking in the exploring mode has not been implemented yet")
    | TakeAll ->
        StepResult.init world state
        |> StepResult.withRender (RenderAction.Fallback "TakingAll in the exploring mode has not been implemented yet")
    | Ignore _ ->
        StepResult.init world state
    | Unknown ->
        StepResult.init world state
        |> StepResult.withRender (RenderAction.Text (Text.create (TextKey.create "unknown_intent")))
    | Wait ->
        StepResult.init { world with Turn = world.Turn + 1u } state
        |> StepResult.withRender (RenderAction.Text (Text.create (TextKey.create "waiting_description")))


let updateExploring (world: World) (state: ExploringState) (msg: ExploringEvent) : StepResult<ExploringState> =
    match msg with
    | UserInput input ->
        let intent = resolveUserIntent world state input
        intent |> handleIntent world state
    | _ ->
        StepResult.init world state