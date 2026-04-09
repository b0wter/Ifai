using CommunityToolkit.Mvvm.Input;
using CommunityToolkit.Mvvm.ComponentModel;
using System;
using System.Collections.Generic;
using System.Linq;
using Ifai.Lib;

#nullable enable

namespace Ifai.Gui.ViewModels;

public partial class MainWindowViewModel : ViewModelBase
{
    private readonly Func<string, Model> _loadModel;
    private readonly Func<Model, Engine> _createEngine;
    private readonly Action _shutdown;

    [ObservableProperty] private string _inputText = string.Empty;
    [ObservableProperty] private string _roomDescription = string.Empty;
    [ObservableProperty] private string _roomName = string.Empty;
    [ObservableProperty] private List<string> _things = [];
    [ObservableProperty] private bool _isAdventureLoaded;
    [ObservableProperty] private string _errorMessage = string.Empty;

    public System.Collections.ObjectModel.ObservableCollection<NewHistoryItemMessage> History { get; } = new();
    public System.Collections.ObjectModel.ObservableCollection<string> DebugMessages { get; } = new();

    private Engine? _engine;
    private IDisposable? _outputSubscription;

    public MainWindowViewModel(Func<string, Model> loadModel, Func<Model, Engine> createEngine, Action shutdown)
    {
        _loadModel = loadModel;
        _createEngine = createEngine;
        _shutdown = shutdown;
    }

    public void LoadAdventure(string folderPath)
    {
        // Clean up any previously running engine
        _outputSubscription?.Dispose();
        _engine?.CancellationTokenSource.Cancel();
        History.Clear();
        DebugMessages.Clear();
        ErrorMessage = string.Empty;

        try
        {
            var model = _loadModel(folderPath);

            _engine = _createEngine(model);
            var observer = new SimpleObserver<EngineMessageInfo>(OnEngineMessage, OnEngineError);
            _outputSubscription = _engine.Output.Subscribe(observer);
            IsAdventureLoaded = true;
        }
        catch (Exception ex)
        {
            ErrorMessage = $"Failed to load adventure: {ex.Message}";
            IsAdventureLoaded = false;
        }
    }

    [RelayCommand]
    private void SubmitInput()
    {
        if (_engine != null && !string.IsNullOrWhiteSpace(InputText))
        {
            var command = EngineCommand.NewUserInput(InputText);
            _engine.Input.Send(command);
            InputText = string.Empty;
        }
    }

    private void OnEngineMessage(EngineMessageInfo messageInfo)
    {
        if (!Avalonia.Threading.Dispatcher.UIThread.CheckAccess())
        {
            Avalonia.Threading.Dispatcher.UIThread.Post(() => OnEngineMessage(messageInfo));
            return;
        }

        switch (messageInfo)
        {
            case UpdatedGameStateMessage msg:
                HandleUpdatedGameState(msg.GameState);
                break;
            case NewHistoryItemMessage msg:
                HandleNewHistoryItem(msg);
                break;
            case ClearScreenMessage:
                HandleClearScreen();
                break;
            case DebugOutputResultMessage msg:
                HandleDebugMessage($"{msg.Event}");
                break;
            case DebugOutputMessageMessage msg:
                HandleDebugMessage(msg.Message);
                break;
            case RequestQuitMessage:
                HandleRequestQuit();
                break;
            case BatchMessage msg:
                foreach (var m in msg.Messages)
                {
                    OnEngineMessage(m);
                }
                break;
        }
    }

    private void OnEngineError(Exception error)
    {
        if (!Avalonia.Threading.Dispatcher.UIThread.CheckAccess())
        {
            Avalonia.Threading.Dispatcher.UIThread.Post(() => OnEngineError(error));
            return;
        }

        ErrorMessage = $"Engine error: {error.Message}";
    }

    private void HandleNewHistoryItem(NewHistoryItemMessage message)
    {
        History.Add(message);
    }

    private void HandleDebugMessage(string message)
    {
        DebugMessages.Add(message);
    }

    private void HandleUpdatedGameState(GameStateInfo gameState)
    {
        RoomDescription = gameState.Room.Description.Text;
        RoomName = gameState.Room.Name.Text;
        Things = gameState.Player.Inventory.Select(t => t.Name.Text).ToList();
    }

    private void HandleClearScreen()
    {
        History.Clear();
        DebugMessages.Clear();
    }

    private void HandleRequestQuit()
    {
        Exit();
    }

    [RelayCommand]
    private void Exit()
    {
        _outputSubscription?.Dispose();
        _engine?.CancellationTokenSource.Cancel();
        _shutdown();
    }
}

internal class SimpleObserver<T> : IObserver<T>
{
    private readonly Action<T> _onNext;
    private readonly Action<Exception>? _onError;

    public SimpleObserver(Action<T> onNext, Action<Exception>? onError = null)
    {
        _onNext = onNext;
        _onError = onError;
    }

    public void OnCompleted() { }
    public void OnError(Exception error) => _onError?.Invoke(error);
    public void OnNext(T value) => _onNext(value);
}
