module Ifai.Runtime.Engine

open System
open System.Threading
open Ifai.Lib
open Ifai.Lib.Content
open Ifai.Runtime


let runAction
        (fileIo: IFileIO)
        (terminate: unit -> unit)
        (action: RuntimeAction<Event>) : Event option =
    match action with
    | Quit ->
        do terminate ()
        None
    | RuntimeAction.Nothing -> None
    | Parsing p -> p |> GameLoop.runParser |> Some
    | WriteFile (filename, allowOverwrite, content) ->
        content
        |> (fileIo.WriteFile filename allowOverwrite)
        |> (function
            | WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | WriteFileResult.Failure error -> Event.FileWriteFailed error
            | WriteFileResult.AlreadyExists filename -> Event.FileAlreadyExists filename)
        |> Some
    | ReadFile _ -> failwith "todo: not yet implemented"
    | SerializeAndWriteToFile (filename, allowOverwrite, obj) ->
        obj
        |> fileIo.Serialize
        |> Result.map (fileIo.WriteFile filename allowOverwrite)
        |> (function
            | Ok WriteFileResult.Success -> Event.FileWrittenSuccessfully
            | Ok (WriteFileResult.Failure err) -> err |> Event.FileWriteFailed
            | Ok (WriteFileResult.AlreadyExists f) -> f |> Event.FileAlreadyExists
            | Error err -> err |> Event.FileWriteFailed)
        |> Some
    | OfEvent e -> Some e
    // The SaveGame case is special since it requires a current instance of the model. That model is only known to the
    // runtime and that's why the `SaveGame` action is replaced with a `SerializeAndWriteToFile` action with the world
    // as parameter
    | SaveGame _ -> failwith "SaveGame must be resolved by the runtime and cannot be run as an action"


let run (fileIo: IFileIO) (model: Model) : RuntimeEngine =
    let cts = new CancellationTokenSource()
    let terminate = fun () -> cts.Cancel()
    let runAction = runAction fileIo terminate
    let engineMessageEvent = Event<EngineMessage>()
    let sendEngineMessage (message: EngineMessage) = message |> engineMessageEvent.Trigger

    let agent = MailboxProcessor<Ifai.Lib.Event>.Start(fun inbox ->
        let rec loop (currentModel: Model) (pendingTransition: ModeTransition option) = async {
            try
                (*
                    To transition between states and not lose any messages, the transition works as follows:
                    - we store the pending transition in a mutable variable.
                    - we "drain" the current mailbox, this should be no problem since messages are almost exclusively
                      created through user input
                    - once the mailbox is empty and we cannot dequeue any event, we apply the transition
                *)
                let! maybeEvent = inbox.TryReceive(100)
                match maybeEvent with
                | Some event ->
                    let result = GameLoop.update currentModel event

                    sendEngineMessage (EngineMessage.UpdatedGameState (RuntimeMappers.constructGameSnapshot result.Model))

                    do sendEngineMessage (EngineMessage.DebugOutputResult (result, event))

                    match result.Render |> RuntimeMappers.render result.Model.TextResources result.Model.Language with
                    | Some renderable -> renderable |> sendEngineMessage
                    | _ -> ()

                    do
                       // Since the SaveGame action requires the current `model` it can only be truly constructed in
                       // the runtime.
                       (match result.Runtime with
                        | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, result.Model)
                        | other -> other)
                        |> runAction
                        |> Option.iter inbox.Post

                    if cts.Token.IsCancellationRequested then
                        do printfn "Shutting down"
                        return ()
                    else
                        match result.Transition, pendingTransition with
                        | ModeTransition.Nothing, None ->
                            return! loop result.Model None
                        | otherTransition, None ->
                            return! loop result.Model (Some otherTransition)
                        | otherTransition, Some pending ->
                            do
                                (EngineMessage.DebugOutputMessage $"Received transition to %A{otherTransition} while having pending transition to %A{pending}, ignoring new transition and keep pending transition")
                                |> sendEngineMessage
                            return! loop result.Model pendingTransition
                | None ->
                    match pendingTransition with
                    | Some t ->
                        let newModel, runtimeAction, renderAction = currentModel |> GameLoop.runTransition t

                        do (match runtimeAction with
                            | SaveGame (filename, allowOverwrite) -> SerializeAndWriteToFile (filename, allowOverwrite, newModel)
                            | other -> other)
                            |> runAction
                            |> Option.iter inbox.Post

                        match renderAction |> RuntimeMappers.render newModel.TextResources model.Language with
                        | Some renderable -> renderable |> sendEngineMessage
                        | None -> ()

                        return! loop newModel None
                    | None ->
                        return! loop currentModel None
            with
            | exn ->
                do (EngineMessage.DebugOutputMessage $"Error while running command: %s{exn.Message}%s{Environment.NewLine}%s{exn.StackTrace}") |> sendEngineMessage
                do cts.Cancel ()
                return ()
        }
        loop model None
    )

    let inputPort =
        { new IEngineInput with
            member _.Send command =
                if not cts.IsCancellationRequested then
                    match command with
                    | EngineCommand.UserInput input -> input |> Ifai.Lib.Event.RawInput |> agent.Post }

    {
        Input = inputPort
        Output = engineMessageEvent.Publish
        CancellationTokenSource = cts
    }


let initializeModel (world: World) (language: string) texts =
    let exploringState =
        { Ifai.Lib.Modes.Exploring.ExploringState.Foo = 42 }

    { Model.Language = Language.create language
      Model.GameMode = [GameMode.Exploring exploringState]
      Model.TextResources = texts
      Model.World = world
    }
