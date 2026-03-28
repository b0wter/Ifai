using Ifai.Lib;
using Microsoft.AspNetCore.SignalR;
using Ifai.SignalR.Host.Hubs;
using System.Collections.Concurrent;

namespace Ifai.SignalR.Host;

public class GameEngineManager : IDisposable
{
    private readonly IHubContext<IfaiHub> _hubContext;
    private readonly ILogger<GameEngineManager> _logger;
    private readonly ConcurrentDictionary<string, Lazy<(Engine Engine, IDisposable Subscription)>> _engines = new();

    public GameEngineManager(IHubContext<IfaiHub> hubContext, ILogger<GameEngineManager> logger)
    {
        _hubContext = hubContext;
        _logger = logger;
    }

    public void Dispose()
    {
        StopAll();
    }

    private void StopAll()
    {
        foreach (var connectionId in _engines.Keys)
        {
            DisposeGame(connectionId);
        }
    }

    public void StartGame(string connectionId)
    {
        var lazy = _engines.GetOrAdd(connectionId, id => new Lazy<(Engine, IDisposable)>(() =>
        {
            var fileIo = new SimpleFileIo();
            var language = LanguageModule.create("en");
            var model = Dummies.World.init(
                Dummies.Rooms.dummyRooms(language),
                Dummies.Rooms.dummyRoomIds.First(),
                LanguageModule.create("en"),
                Dummies.Texts.textResources);

            var engine = Runtime.run(fileIo, model);
            var subscription = engine.Output.Subscribe(new AnonymousObserver<EngineMessageInfo>(
                onNext: msg => OnEngineMessage(id, msg),
                onError: ex => _logger.LogError(ex, "Engine observable error for connection {ConnectionId}", id)));

            return (engine, subscription);
        }));
        _ = lazy.Value;
    }

    public void DisposeGame(string connectionId)
    {
        if (_engines.TryRemove(connectionId, out var lazy) && lazy.IsValueCreated)
        {
            var pair = lazy.Value;
            try { pair.Engine.CancellationTokenSource.Cancel(); } catch (ObjectDisposedException) { }
            try { pair.Engine.CancellationTokenSource.Dispose(); } catch (ObjectDisposedException) { }
            pair.Subscription.Dispose();
        }
    }

    private void OnEngineMessage(string connectionId, EngineMessageInfo messageInfo)
    {
        _ = Task.Run(async () =>
        {
            try
            {
                await HandleEngineMessage(connectionId, messageInfo);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error handling engine message for connection {ConnectionId}", connectionId);
            }
        });
    }

    public void SendCommand(string connectionId, string input)
    {
        if (_engines.TryGetValue(connectionId, out var lazy) && lazy.IsValueCreated)
        {
            lazy.Value.Engine.Input.Send(EngineCommand.NewUserInput(input));
        }
        else
        {
            _logger.LogWarning("SendCommand called for unknown connection {ConnectionId}", connectionId);
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
                await _hubContext.Clients.Client(connectionId).SendAsync("DebugMessage", $"{msg.Event}");
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
            default:
                _logger.LogWarning("Unhandled engine message type {MessageType} for connection {ConnectionId}", message.GetType().Name, connectionId);
                break;
        }
    }

    private class AnonymousObserver<T>(Action<T> onNext, Action<Exception> onError) : IObserver<T>
    {
        public void OnCompleted() { }
        public void OnError(Exception error) => onError(error);
        public void OnNext(T value) => onNext(value);
    }

    private class SimpleFileIo : IFileIO
    {
        public WriteFileResult WriteFile(string filename, bool allowOverwrite, string content)
        {
            return WriteFileResult.NewFailure("Saving is not available over SignalR connections.");
        }

        public Microsoft.FSharp.Core.FSharpResult<string, string> Serialize(object obj)
        {
            return Microsoft.FSharp.Core.FSharpResult<string, string>.NewError("Saving is not available over SignalR connections.");
        }
    }
}
