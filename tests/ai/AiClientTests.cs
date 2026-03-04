using Microsoft.Extensions.AI;
using Microsoft.Extensions.Configuration;
using FakeItEasy;
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
        var mockChatClient = A.Fake<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>._, A<ChatOptions>._, A<CancellationToken>._))
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("Prompt 1");
        await client.PromptAsync("Prompt 2", continueChat: false);

        // Assert
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>.That.Matches(list => list.Count == 1 && list[0].Text == "Prompt 2"), A<ChatOptions>._, A<CancellationToken>._))
            .MustHaveHappenedOnceExactly();
    }

    [Fact]
    public async Task PromptAsync_ContinueChat_KeepsHistory()
    {
        // Arrange
        var mockChatClient = A.Fake<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>._, A<ChatOptions>._, A<CancellationToken>._))
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("Prompt 1");
        await client.PromptAsync("Prompt 2", continueChat: true);

        // Assert
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>.That.Matches(list => list.Count == 3 && list[2].Text == "Prompt 2"), A<ChatOptions>._, A<CancellationToken>._))
            .MustHaveHappenedOnceExactly();
    }

    [Fact]
    public async Task PromptAsync_WithSystemMessage_IncludesItInRequest()
    {
        // Arrange
        var mockChatClient = A.Fake<IChatClient>();
        var client = AiClient.ForTest(mockChatClient);
        client.SetSystemMessage("System Message");
        
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>._, A<ChatOptions>._, A<CancellationToken>._))
            .Returns(new ChatResponse(new ChatMessage(ChatRole.Assistant, "Response")));

        // Act
        await client.PromptAsync("User Prompt");

        // Assert
        A.CallTo(() => mockChatClient.GetResponseAsync(A<IList<ChatMessage>>.That.Matches(list => list.Count == 2 && list[0].Role == ChatRole.System && list[1].Role == ChatRole.User), A<ChatOptions>._, A<CancellationToken>._))
            .MustHaveHappenedOnceExactly();
    }
}