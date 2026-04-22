using Ifai.Lib;
using Ifai.Runtime.Interop;
using Microsoft.AspNetCore.SignalR;
using Ifai.SignalR.Host.Hubs;
using System.Collections.Concurrent;
using System.Text;
using System.Text.Json.Serialization;
using Ifai.Runtime.Interop.Types;
using Engine = Ifai.Runtime.Interop.Types.Engine;

namespace Ifai.SignalR.Host;

public class GameEngineManager(IHubContext<IfaiHub> hubContext, ILogger<GameEngineManager> logger)
    : IDisposable
{
    private readonly ConcurrentDictionary<string, Lazy<(Runtime.Interop.Types.Engine Engine, IDisposable Subscription)>> _engines = new();

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

            var engine = Runtime.Interop.Engine.run(fileIo, model);
            var subscription = engine.Output.Subscribe(new AnonymousObserver<EngineMessageInfo>(
                onNext: msg => OnEngineMessage(id, msg),
                onError: ex => logger.LogError(ex, "Engine observable error for connection {ConnectionId}", id)));

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
                logger.LogError(ex, "Error handling engine message for connection {ConnectionId}", connectionId);
            }
        });
    }

    public void SendCommand(string connectionId, string input)
    {
        if (_engines.TryGetValue(connectionId, out var lazy) && lazy.IsValueCreated)
        {
            lazy.Value.Engine.Input.Send(Runtime.EngineCommand.NewUserInput(input));
        }
        else
        {
            logger.LogWarning("SendCommand called for unknown connection {ConnectionId}", connectionId);
        }
    }

    private async Task HandleEngineMessage(string connectionId, EngineMessageInfo message)
    {
        switch (message)
        {
            case UpdatedGameStateMessage msg:
                await hubContext.Clients.Client(connectionId).SendAsync("UpdatedGameState", msg.GameState);
                break;
            case NewHistoryItemMessage msg:
                await hubContext.Clients.Client(connectionId).SendAsync("NewHistoryItem", msg.Text, msg.Style);
                break;
            case ClearScreenMessage:
                await hubContext.Clients.Client(connectionId).SendAsync("ClearScreen");
                break;
            case DebugOutputResultMessage msg:
                await hubContext.Clients.Client(connectionId).SendAsync("DebugMessage", $"{msg.Event}");
                break;
            case DebugOutputMessageMessage msg:
                await hubContext.Clients.Client(connectionId).SendAsync("DebugMessage", msg.Message);
                break;
            case RequestQuitMessage:
                await hubContext.Clients.Client(connectionId).SendAsync("RequestQuit");
                DisposeGame(connectionId);
                break;
            case BatchMessage msg:
                foreach (var innerMsg in msg.Messages)
                {
                    await HandleEngineMessage(connectionId, innerMsg);
                }
                break;
            default:
                logger.LogWarning("Unhandled engine message type {MessageType} for connection {ConnectionId}", message.GetType().Name, connectionId);
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
        string? tmpFile = null;

        try
        {
            var fullPath = Path.GetFullPath(filename);

            var dir = Path.GetDirectoryName(fullPath);
            if (string.IsNullOrEmpty(dir))
                dir = Directory.GetCurrentDirectory();

            // Optional early exit (avoids unnecessary I/O)
            if (!allowOverwrite && File.Exists(fullPath))
                return WriteFileResult.NewAlreadyExists(fullPath);

            Directory.CreateDirectory(dir);

            tmpFile = Path.Combine(dir, $".tmp_{Path.GetRandomFileName()}");
            var backupFile = fullPath + ".bak";

            // Write content to temp file with explicit encoding + durability
            using (var fs = new FileStream(tmpFile, FileMode.CreateNew, FileAccess.Write, FileShare.None))
            using (var writer = new StreamWriter(fs, new UTF8Encoding(encoderShouldEmitUTF8Identifier: false)))
            {
                writer.Write(content);
                writer.Flush();
                fs.Flush(true); // flush OS buffers to disk
            }

            if (File.Exists(fullPath))
            {
                // Atomic replace (with backup)
                File.Replace(tmpFile, fullPath, backupFile, ignoreMetadataErrors: true);
            }
            else
            {
                File.Move(tmpFile, fullPath);
            }

            return WriteFileResult.Success;
        }
        catch (Exception ex)
        {
            // Best-effort cleanup of temp file only
            try
            {
                if (tmpFile != null && File.Exists(tmpFile))
                    File.Delete(tmpFile);
            }
            catch
            {
                // ignored
            }

            return WriteFileResult.NewFailure(ex.Message);
        }
    }

    public Microsoft.FSharp.Core.FSharpResult<string, string> Serialize(object obj)
    {
        try
        {
            var json = System.Text.Json.JsonSerializer.Serialize(obj, new System.Text.Json.JsonSerializerOptions
            {
                Converters = { new JsonStringEnumConverter() }
            });
            return Microsoft.FSharp.Core.FSharpResult<string, string>.NewOk(json);
        }
        catch (Exception ex)
        {
            return Microsoft.FSharp.Core.FSharpResult<string, string>.NewError(ex.Message);
        }
    }
}
}
