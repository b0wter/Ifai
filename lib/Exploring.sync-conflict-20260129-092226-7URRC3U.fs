module Ifai.Lib.Exploring

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
    | Global of BuiltIns.GlobalBuiltIn
    | BuiltIn of ExploringBuiltIn


/// This represents the user's intent as extracted from the entered input
type ExploringIntent =
    | Move of Directions.Exit
    | Wait


type ExploringMsg =
    | NewRom
    | UserInput of Input
    | ResolvingRoomEvent of RoomEvent option


type ExploringCmd =
    | Nothing


type ExploringState = {
    Foo: int
}


let runCmd (cmd: ExploringCmd) : ExploringMsg list =
    match cmd with
    | Nothing -> []
    
    
/// Turns raw text input into intent
let parser (input: BuiltIns.GlobalParserResult) : Input option =
    match input with
    | BuiltIns.GlobalParserResult.BuiltIn.Save _ -> None
    | 

    | Input.BuiltIn. -> Move (Directions.Exit.Dir Directions.North) |> Some
    | "ne" | "northeast" -> Move (Directions.Exit.Dir Directions.NorthEast) |> Some
    | "e" | "east" -> Move (Directions.Exit.Dir Directions.East) |> Some
    | "se" | "southeast" -> Move (Directions.Exit.Dir Directions.SouthEast) |> Some
    | "s" | "south" -> Move (Directions.Exit.Dir Directions.South) |> Some
    | "sw" | "southwest" -> Move (Directions.Exit.Dir Directions.SouthWest) |> Some
    | "w" | "west" -> Move (Directions.Exit.Dir Directions.West) |> Some
    | "nw" | "northwest" -> Move (Directions.Exit.Dir Directions.NorthWest) |> Some
    | _ -> None
    *)


let updateExploring (world: World) (state: ExploringState) (msg: ExploringMsg) : World * ExploringState * ExploringCmd =
    match msg with
    | _ -> world, state, Nothing