module Ifai.Lib.Exploring

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
    ["l"; "look"; "look around"], fun _ -> LookAround
    ["x"; "examine"; "l at "; "look at "], fun argument -> Examine argument
    ["t"; "take"], fun argument -> Take argument
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
    ["g"; "a"; "gucken"; "angucken"; "anschauen"; "ansehen"; "x"; "untersuchen "; "unsersuche "], fun argument -> Examine argument
    ["n "; "nehme "; "nimm "], fun argument -> Take argument
    ["nimm alles" ], fun _ -> TakeAll
    ]
    
    
let builtIns =
    [ Language.create "en", english
      Language.create "de", german
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


type ExploringCmd =
    | Print of string
    | Nothing


type ExploringState = {
    Foo: int
    // TODO: add meaningful fields
}


let runCmd (cmd: ExploringCmd) : ExploringEvent list =
    match cmd with
    | Print text ->
        do Console.WriteLine(text)
        []
    | Nothing -> []
    
    
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


let tryMoveToRoom (world: World) (exit: Directions.Exit) : Room option =
    world.Rooms
    |> Map.tryFind world.CurrentRoom
    |> Option.map _.Connections
    |> Option.bind (Map.tryFind exit)
    |> Option.bind (fun roomId -> world.Rooms |> Map.tryFind roomId)
    

let handleIntent (world: World) (state: ExploringState) (intent: ExploringIntent) : World * ExploringState * ExploringCmd =
    match intent with
    | Move exit ->
        match exit |> tryMoveToRoom world with
        | Some room -> { world with CurrentRoom = room.Id }, state, Nothing
        | None -> world, state, Nothing
    | LookAround -> world, state, Nothing
    | Examine item -> world, state, Nothing
    | Take item -> world, state, Nothing
    | TakeAll -> world, state, Nothing
    | Ignore _ -> world, state, Nothing
    | Unknown -> world, state, (ExploringCmd.Print "Input could not be interpreted")
    | Wait -> { world with Turn = world.Turn + 1u }, state, Nothing


let updateExploring (world: World) (state: ExploringState) (msg: ExploringEvent) : World * ExploringState * ExploringCmd =
    match msg with
    | UserInput input ->
        let intent = resolveUserIntent world state input
        intent |> handleIntent world state
    | _ -> world, state, Nothing