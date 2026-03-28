using Microsoft.AspNetCore.SignalR;

namespace Ifai.SignalR.Host.Hubs;

public class IfaiHub : Hub
{
    private readonly ILogger<IfaiHub> _logger;
    private readonly GameEngineManager _gameEngineManager;

    public IfaiHub(ILogger<IfaiHub> logger, GameEngineManager gameEngineManager)
    {
        _logger = logger;
        _gameEngineManager = gameEngineManager;
    }

    public Task SendMessage(string user, string message)
    {
        _logger.LogInformation("Received message from {User}: {Message}", user, message);
        _gameEngineManager.SendCommand(Context.ConnectionId, message);
        return Task.CompletedTask;
    }

    public override async Task OnConnectedAsync()
    {
        _logger.LogInformation("User connected: {ConnectionId}", Context.ConnectionId);
        _gameEngineManager.StartGame(Context.ConnectionId);
        await base.OnConnectedAsync();
    }

    public override async Task OnDisconnectedAsync(Exception? exception)
    {
        _logger.LogInformation("User disconnected: {ConnectionId}. Exception: {Exception}", Context.ConnectionId, exception?.Message ?? "None");
        _gameEngineManager.DisposeGame(Context.ConnectionId);
        await base.OnDisconnectedAsync(exception);
    }
}
