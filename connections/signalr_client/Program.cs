using Microsoft.AspNetCore.SignalR.Client;
using Microsoft.Extensions.DependencyInjection;
using System.Text.Json.Serialization;

Console.WriteLine("Ifai.SignalR.Client is starting...");

var connection = new HubConnectionBuilder()
    .WithUrl("http://localhost:5188/ifaiHub")
    .WithAutomaticReconnect()
    .AddJsonProtocol(options =>
    {
        options.PayloadSerializerOptions.Converters.Add(new JsonStringEnumConverter());
        options.PayloadSerializerOptions.Converters.Add(new JsonFSharpConverter());
    })
    .Build();

connection.On<string, string>("ReceiveMessage", (user, message) =>
{
    Console.WriteLine($"[{user}]: {message}");
});

connection.On<string, string>("NewHistoryItem", (text, style) =>
{
    Console.WriteLine($"{text} ({style})");
});

/*
connection.On<object>("UpdatedGameState", (gameState) =>
{
    Console.WriteLine($"[Engine]: Game state updated.");
});
*/

connection.On("ClearScreen", () =>
{
    Console.Clear();
});

connection.On<string>("DebugMessage", (message) =>
{
    Console.WriteLine($"[Debug]: {message}");
});

try
{
    await connection.StartAsync();
    Console.WriteLine("Connected to Ifai SignalR Server.");
}
catch (Exception ex)
{
    Console.WriteLine($"Failed to connect: {ex.Message}");
    return;
}

Console.WriteLine("Type a message and press Enter to send (or 'exit' to quit):");

while (true)
{
    var message = Console.ReadLine();
    if (string.IsNullOrWhiteSpace(message) || message.ToLower() == "exit")
    {
        break;
    }

    try
    {
        await connection.InvokeAsync("SendMessage", "ClientUser", message);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error sending message: {ex.Message}");
    }
}

await connection.StopAsync();
Console.WriteLine("Disconnected.");
