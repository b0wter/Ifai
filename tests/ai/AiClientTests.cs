using Microsoft.Extensions.AI;
using Microsoft.Extensions.Configuration;
using NSubstitute;
using Xunit;

namespace Ifai.Ai.Tests;

public class AiClientTests
{
    [Fact]
    public void ForOllama_WithValidConfig_CreatesClient()
    {
        // Arrange
        var inMemorySettings = new Dictionary<string, string> {
            {"Ollama:Endpoint", "http://test-endpoint:11434"},
            {"Ollama:Model", "test-model"}
        };

        IConfiguration configuration = new ConfigurationBuilder()
            .AddInMemoryCollection(inMemorySettings!)
            .Build();

        // Act
        var client = AiClient.ForOllama(configuration);

        // Assert
        Assert.NotNull(client);
    }
    [Fact]
    public async Task PromptAsync_NewChat_ClearsHistory()
    {
        // Arrange
        var mockChatClient = Substitute.For<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        
        mockChatClient.GetResponseAsync(Arg.Any<IList<ChatMessage>>(), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>())
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("Prompt 1");
        await client.PromptAsync("Prompt 2", continueChat: false);

        // Assert
        // We expect TWO calls total, but only the SECOND one should match the "Prompt 2" alone criteria.
        // Actually, NSubstitute's Received(1) would check if exactly one call matched.
        await mockChatClient.Received(1).GetResponseAsync(Arg.Is<IList<ChatMessage>>(list => list.Count == 1 && list[0].Text == "Prompt 2"), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>());
    }

    [Fact]
    public async Task PromptAsync_ContinueChat_KeepsHistory()
    {
        // Arrange
        var mockChatClient = Substitute.For<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        
        mockChatClient.GetResponseAsync(Arg.Any<IList<ChatMessage>>(), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>())
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("Prompt 1");
        await client.PromptAsync("Prompt 2", continueChat: true);

        // Assert
        await mockChatClient.Received(1).GetResponseAsync(Arg.Is<IList<ChatMessage>>(list => list.Count == 3 && list[2].Text == "Prompt 2"), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>());
    }

    [Fact]
    public async Task PromptAsync_WithSystemMessage_IncludesItInRequest()
    {
        // Arrange
        var mockChatClient = Substitute.For<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        client.SetSystemMessage("System Message");
        
        mockChatClient.GetResponseAsync(Arg.Any<IList<ChatMessage>>(), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>())
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("User Prompt");

        // Assert
        await mockChatClient.Received(1).GetResponseAsync(Arg.Is<IList<ChatMessage>>(list => list.Count == 2 && list[0].Role == ChatRole.System && list[1].Role == ChatRole.User), Arg.Any<ChatOptions>(), Arg.Any<CancellationToken>());
    }
}