open System
open System.Text.Json.Serialization

open Ifai.Lib
open Ifai.Lib.Modes

let printDebug (result: GlobalResult) (event: Event) =
    do Console.ForegroundColor <- ConsoleColor.Yellow
    do Console.Write($"{result.Model.GameMode.Head.GetType().Name}")
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Green
    do Console.Write(event)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Blue
    do Console.Write(result.Runtime)
    do Console.ResetColor ()
    do Console.Write(" - ")
    do Console.ForegroundColor <- ConsoleColor.Magenta
    do Console.WriteLine(result.Transition)
    do Console.ResetColor ()
let createDebugOutput () =
    { new IDebugOutput with
        member this.RenderState (result: GlobalResult) (event: Event) = printDebug result event
        member this.RenderSystemMessage (message: string) =
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine(message)}


let textWriter filename allowOverwrite (content: string) =
    try
        if (filename |> IO.File.Exists) && (not allowOverwrite) then filename |> WriteFileResult.AlreadyExists
        else
            do IO.File.WriteAllText(filename, content)
            WriteFileResult.Success
    with
    | exn -> WriteFileResult.Failure exn.Message


let options =
    JsonFSharpOptions
        .Default()
        .WithAllowNullFields()
        .ToJsonSerializerOptions()

let serializer object =
    try
        Text.Json.JsonSerializer.Serialize(object, options) |> Ok
    with
    | exn -> Error exn.Message


let createFileIo () =
    { new IFileIO with
        member this.WriteFile filename allowOverwrite content = textWriter filename allowOverwrite content
        member this.Serialize obj = serializer obj }

let createRenderer () =
     let render (text: string) (style: NarrativeStyle) =
         Console.ResetColor()
         match style with
         | Regular -> ()
         | Emphasized -> Console.ForegroundColor <- ConsoleColor.DarkYellow
         | Hint -> Console.ForegroundColor <- ConsoleColor.Gray
         | Dialogue -> Console.ForegroundColor <- ConsoleColor.DarkGreen
         | System -> Console.ForegroundColor <- ConsoleColor.DarkRed
         Console.WriteLine(text)

     { new IRenderer with
         member this.Clear () = Console.Clear()
         member this.RenderGameState state = ()
         member this.RenderText (text, style) = render text style }

let createInput () =
    { new IAsyncInput with
        member this.ReadInput ct = AsyncConsoleReader.AsyncConsole.ReadLineAsync(ct).AsTask() |> Async.AwaitTask }

let model = Ifai.Resources.World.init Ifai.Resources.Rooms.dummyRooms Ifai.Resources.Rooms.dummyRoomIds[0] (Language.create "en") Ifai.Resources.Texts.textResources

let engine = Runtime.run (createFileIo()) model

let rec handleEngineMessage (message: EngineMessage) =
    do Console.ResetColor()
    match message with
    | UpdatedGameState gameStateInfo -> printfn "%A" gameStateInfo
    | NewHistoryItem(s, narrativeStyleInfo) -> printfn "%s" s
    | ClearScreen -> Console.Clear()
    | DebugOutputResult(globalResult, event) -> Console.ForegroundColor <- ConsoleColor.DarkGray; printDebug globalResult event
    | DebugOutputMessage s -> printfn "%s" s
    | RequestQuit -> engine.CancellationTokenSource.Cancel()
    | EngineMessage.Batch engineMessages -> Array.iter handleEngineMessage engineMessages

do engine.Output.Subscribe(fun x ->
    handleEngineMessage x.OriginalMessage
    )

let externalInput = createInput()

async {
    while true do
        let! input = externalInput.ReadInput(engine.CancellationTokenSource.Token)
        engine.Input.Send(input |> EngineCommand.UserInput)
} |> Async.Start

engine.CancellationTokenSource.Token.WaitHandle.WaitOne() |> ignore