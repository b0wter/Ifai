open System
open System.Text.Json.Serialization

open Ifai.Lib
open Ifai.Lib.Modes
open Ifai.Runtime

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


let textWriter (filename: string) allowOverwrite (content: string) =
    let fullPath = System.IO.Path.GetFullPath(filename)

    let dir =
        let d = System.IO.Path.GetDirectoryName(fullPath)
        if System.String.IsNullOrEmpty(d) then
            System.IO.Directory.GetCurrentDirectory()
        else d

    let tempFile = System.IO.Path.Combine(dir, $".tmp_{System.IO.Path.GetRandomFileName()}")
    let backupFile = fullPath + ".bak"

    try
        // Optional early exit
        if not allowOverwrite && System.IO.File.Exists(fullPath) then
            WriteFileResult.AlreadyExists fullPath
        else
            System.IO.Directory.CreateDirectory(dir) |> ignore

            use fs = new System.IO.FileStream(tempFile, System.IO.FileMode.CreateNew, System.IO.FileAccess.Write, System.IO.FileShare.None)
            use writer = new System.IO.StreamWriter(fs, new System.Text.UTF8Encoding(false))
            writer.Write(content)
            writer.Flush()
            fs.Flush(true)

            if System.IO.File.Exists(fullPath) then
                System.IO.File.Replace(tempFile, fullPath, backupFile, true)
            else
                System.IO.File.Move(tempFile, fullPath)

            WriteFileResult.Success

    with ex ->
        try if System.IO.File.Exists(tempFile) then System.IO.File.Delete(tempFile) with _ -> ()
        WriteFileResult.Failure ex.Message


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
         | NarrativeStyle.System -> Console.ForegroundColor <- ConsoleColor.DarkRed
         Console.WriteLine(text)

     { new IRenderer with
         member this.Clear () = Console.Clear()
         member this.RenderGameState state = ()
         member this.RenderText (text, style) = render text style }

let createInput () =
    { new IAsyncInput with
        member this.ReadInput ct = AsyncConsoleReader.AsyncConsole.ReadLineAsync(ct).AsTask() |> Async.AwaitTask }

let model = Ifai.Dummies.World.init [] Ifai.Dummies.Rooms.dummyRoomIds[0] (Language.create "en") Ifai.Dummies.Texts.textResources

let engine = Engine.run (createFileIo()) model

let rec handleEngineMessage (message: EngineMessage) =
    do Console.ResetColor()
    match message with
    | EngineMessage.UpdatedGameState gameSnapshot -> printfn "%A" gameSnapshot
    | EngineMessage.NewHistoryItem(s, _) -> printfn "%s" s
    | EngineMessage.ClearScreen -> Console.Clear()
    | EngineMessage.DebugOutputResult(globalResult, event) -> Console.ForegroundColor <- ConsoleColor.DarkGray; printDebug globalResult event
    | EngineMessage.DebugOutputMessage s -> printfn "%s" s
    | EngineMessage.RequestQuit -> engine.CancellationTokenSource.Cancel()
    | EngineMessage.Batch engineMessages -> Array.iter handleEngineMessage engineMessages

do engine.Output.Subscribe(fun x ->
    handleEngineMessage x
    ) |> ignore

let externalInput = createInput()

async {
    while true do
        let! input = externalInput.ReadInput(engine.CancellationTokenSource.Token)
        engine.Input.Send(input |> EngineCommand.UserInput)
} |> Async.Start

engine.CancellationTokenSource.Token.WaitHandle.WaitOne() |> ignore
