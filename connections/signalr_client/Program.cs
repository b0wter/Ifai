using Microsoft.AspNetCore.SignalR.Client;
using Microsoft.Extensions.DependencyInjection;
using System.Text.Json.Serialization;

Console.WriteLine("Ifai.SignalR.Client is starting...");

var exitRequested = new TaskCompletionSource();

var connection = new HubConnectionBuilder()
    .WithUrl("http://localhost:5188/ifaiHub")
    .WithAutomaticReconnect()
    .AddJsonProtocol(options =>
    {
        options.PayloadSerializerOptions.Converters.Add(new JsonStringEnumConverter());
        options.PayloadSerializerOptions.Converters.Add(new JsonFSharpConverter());
    })
    .Build();

connection.On<string, string>("NewHistoryItem", (text, style) =>
{
    var originalColor = Console.ForegroundColor;
    try
    {
        var styleColor = style switch
        {
            "System" => ConsoleColor.Red,
            "Dialogue" => ConsoleColor.Cyan,
            "Regular" => originalColor,
            "Emphasized" => ConsoleColor.DarkGreen,
            "Hint" => ConsoleColor.DarkGray,
            _ => ConsoleColor.Yellow
        };

        Console.ForegroundColor = styleColor;
        Console.WriteLine(text);
    }
    finally
    {
        Console.ForegroundColor = originalColor;
    }
});

connection.On("ClearScreen", () =>
{
    Console.Clear();
});

connection.On<string>("DebugMessage", (message) =>
{
    Console.WriteLine($"[Debug]: {message}");
});

connection.On("RequestQuit", () =>
{
    Console.WriteLine("Game has ended.");
    exitRequested.TrySetResult();
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
    var readTask = Task.Run(() => Console.ReadLine());
    var completed = await Task.WhenAny(readTask, exitRequested.Task);

    if (completed == exitRequested.Task)
    {
        break;
    }

    var message = await readTask;
    if (string.IsNullOrWhiteSpace(message) || message.Equals("exit", StringComparison.OrdinalIgnoreCase))
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
