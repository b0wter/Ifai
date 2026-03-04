using Ifai.SignalR.Host;
using Ifai.SignalR.Host.Hubs;
using System.Text.Json.Serialization;

var builder = WebApplication.CreateBuilder(args);

builder.WebHost.UseUrls("http://localhost:5188");

builder.Services.AddSignalR()
    .AddJsonProtocol(options =>
    {
        options.PayloadSerializerOptions.Converters.Add(new JsonStringEnumConverter());
        options.PayloadSerializerOptions.Converters.Add(new JsonFSharpConverter());
    });
builder.Services.AddSingleton<GameEngineManager>();

var app = builder.Build();

app.MapGet("/", () => "Ifai SignalR Service Running");
app.MapHub<IfaiHub>("/ifaiHub");

app.Run();
