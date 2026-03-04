using Ifai.Lib;
using Microsoft.AspNetCore.SignalR;
using Ifai.SignalR.Host.Hubs;
using System.Collections.Concurrent;

namespace Ifai.SignalR.Host;

public class GameEngineManager
{
    private readonly IHubContext<IfaiHub> _hubContext;
    private readonly ConcurrentDictionary<string, (Engine Engine, IDisposable Subscription)> _engines = new();

    public GameEngineManager(IHubContext<IfaiHub> hubContext)
    {
        _hubContext = hubContext;
    }

    public void StopAll()
    {
        foreach (var connectionId in _engines.Keys)
        {
            DisposeGame(connectionId);
        }
    }

    public void StartGame(string connectionId)
    {
        if (_engines.ContainsKey(connectionId))
        {
            return;
        }

        var fileIo = new SimpleFileIo();
        var language = LanguageModule.create("en");
        var model = Dummies.World.init(
            Dummies.Rooms.dummyRooms(language),
            Dummies.Rooms.dummyRoomIds.First(), 
            LanguageModule.create("en"), 
            Dummies.Texts.textResources);

        var engine = Runtime.run(fileIo, model);
        var subscription = engine.Output.Subscribe(new AnonymousObserver<EngineMessageInfo>(msg => OnEngineMessage(connectionId, msg)));
        
        _engines.TryAdd(connectionId, (engine, subscription));
    }

    public void DisposeGame(string connectionId)
    {
        if (_engines.TryRemove(connectionId, out var pair))
        {
            pair.Engine.CancellationTokenSource.Cancel();
            pair.Subscription.Dispose();
        }
    }

    private void OnEngineMessage(string connectionId, EngineMessageInfo messageInfo)
    {
        HandleEngineMessage(connectionId, messageInfo).Wait();
    }

    public void SendCommand(string connectionId, string input)
    {
        if (_engines.TryGetValue(connectionId, out var pair))
        {
            pair.Engine.Input.Send(EngineCommand.NewUserInput(input));
        }
    }

    private async Task HandleEngineMessage(string connectionId, EngineMessageInfo message)
    {
        switch (message)
        {
            case UpdatedGameStateMessage msg:
                await _hubContext.Clients.Client(connectionId).SendAsync("UpdatedGameState", msg.GameState);
                break;
            case NewHistoryItemMessage msg:
                await _hubContext.Clients.Client(connectionId).SendAsync("NewHistoryItem", msg.Text, msg.Style);
                break;
            case ClearScreenMessage:
                await _hubContext.Clients.Client(connectionId).SendAsync("ClearScreen");
                break;
            case DebugOutputResultMessage msg:
                // Optional: Send debug info to clients if needed
                break;
            case DebugOutputMessageMessage msg:
                await _hubContext.Clients.Client(connectionId).SendAsync("DebugMessage", msg.Message);
                break;
            case RequestQuitMessage:
                await _hubContext.Clients.Client(connectionId).SendAsync("RequestQuit");
                DisposeGame(connectionId);
                break;
            case BatchMessage msg:
                foreach (var innerMsg in msg.Messages)
                {
                    await HandleEngineMessage(connectionId, innerMsg);
                }
                break;
        }
    }

    private class AnonymousObserver<T>(Action<T> onNext) : IObserver<T>
    {
        public void OnCompleted() { }
        public void OnError(Exception error) { }
        public void OnNext(T value) => onNext(value);
    }

    private class SimpleFileIo : IFileIO
    {
        public WriteFileResult WriteFile(string filename, bool allowOverwrite, string content)
        {
            return WriteFileResult.NewFailure("Not implemented");
        }

        public Microsoft.FSharp.Core.FSharpResult<string, string> Serialize(object obj)
        {
            return Microsoft.FSharp.Core.FSharpResult<string, string>.NewError("Not implemented");
        }
    }
}
