using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.AI;
using OllamaSharp;

namespace Ifai.Ai;

public class AiClient
{
    private readonly IChatClient _chatClient;
    private readonly List<ChatMessage> _chatHistory = [];
    private string? _systemMessage;

    private AiClient(IChatClient chatClient)
    {
        _chatClient = chatClient;
    }

    public static AiClient ForTest(IChatClient chatClient)
    {
        return new AiClient(chatClient);
    }

    public static AiClient ForOllama(IConfiguration configuration)
    {
        var ollamaConfig = configuration.GetSection("Ollama");
        var endpoint = ollamaConfig["Endpoint"] ?? "http://localhost:11434";
        var model = ollamaConfig["Model"] ?? "qwen2.5:1.5b";

        var chatClient = new OllamaApiClient(new Uri(endpoint), model);
        return new AiClient(chatClient);
    }

    public void SetSystemMessage(string message)
    {
        _systemMessage = message;
    }

    public async Task<string> PromptAsync(string prompt, bool continueChat = true)
    {
        if (!continueChat)
        {
            _chatHistory.Clear();
        }

        _chatHistory.Add(new ChatMessage(ChatRole.User, prompt));

        var messages = new List<ChatMessage>();
        if (_systemMessage != null)
        {
            messages.Add(new ChatMessage(ChatRole.System, _systemMessage));
        }
        messages.AddRange(_chatHistory);

        var response = await _chatClient.GetResponseAsync(messages);
        
        string? responseText = null;
        if (response.Messages.Count > 0)
        {
            responseText = String.Join(Environment.NewLine, response.Messages.Select(x => x.Text));
        }

        if (responseText != null)
        {
            _chatHistory.Add(new ChatMessage(ChatRole.Assistant, responseText));
            return responseText;
        }

        throw new InvalidOperationException("Ai model did not return a response");
    }
}