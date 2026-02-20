using CommunityToolkit.Mvvm.Input;
using CommunityToolkit.Mvvm.ComponentModel;
using System;
using System.Linq;
using System.Threading;
using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;
using Ifai.Gui.Helpers;
using Ifai.Lib;

#nullable enable

namespace Ifai.Gui.ViewModels;

public partial class MainWindowViewModel : ViewModelBase
{
    [ObservableProperty] private string _inputText = string.Empty;
    [ObservableProperty] private string _roomDescription = string.Empty;

    public System.Collections.ObjectModel.ObservableCollection<NewHistoryItemMessage> History { get; } = new();
    public System.Collections.ObjectModel.ObservableCollection<string> DebugMessages { get; } = new();

    private Engine? _engine;
    private IDisposable? _outputSubscription;

    public MainWindowViewModel()
    {
        Start();
    }

    [RelayCommand]
    private void Start()
    {
        var model =
            Resources.World.init(
                Resources.Rooms.dummyRooms,
                Resources.Rooms.dummyRoomIds.First(),
                LanguageModule.create("en"),
                Resources.Texts.textResources)!;

        var fileIo = new FileIo();

        // Start the runtime
        _engine = Runtime.run(fileIo, model);

        // Subscribe to the output Observable
        var observer = new SimpleObserver<EngineMessageInfo>(OnEngineMessage);
        _outputSubscription = _engine.Output.Subscribe(observer);
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
        // TODO: Update room and inventory displays
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
        if (Application.Current?.ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            desktop.Shutdown();
        }
        else
        {
            throw new NotImplementedException("Shutdown is not supported in this application lifetime");
        }
    }
}

internal class SimpleObserver<T> : IObserver<T>
{
    private readonly Action<T> _onNext;

    public SimpleObserver(Action<T> onNext)
    {
        _onNext = onNext;
    }

    public void OnCompleted() { }
    public void OnError(Exception error) { }
    public void OnNext(T value) => _onNext(value);
}
