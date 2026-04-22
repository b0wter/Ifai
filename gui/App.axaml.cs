using System;
using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core;
using Avalonia.Data.Core.Plugins;
using System.Linq;
using Avalonia.Markup.Xaml;
using Ifai.Gui.Helpers;
using Ifai.Gui.ViewModels;
using Ifai.Gui.Views;
using Ifai.Lib;
using Ifai.Lib.Content;
using Ifai.Runtime.Interop;

namespace Ifai.Gui;

public partial class App : Application
{
    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }

    private Model LoadModel(string folderPath)
    {
        var result = ContentParser.Content.createWorldFromFolder(folderPath);
        if (result.IsError)
            throw new ArgumentException($"Could not parse adventure: {result.ErrorValue}");
        // TODO: this should load actual text resources not the dummies
        return Ifai.Runtime.Interop.Engine.initializeModel(result.ResultValue, "de", Dummies.Texts.textResources);
    }

    public override void OnFrameworkInitializationCompleted()
    {
        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            // This makes sure that there is no double validation with the communiy toolkit
            DisableAvaloniaDataAnnotationValidation();

            var fileIo = new FileIo();

            Runtime.Interop.Types.Engine CreateEngine(Model model) => Engine.run(fileIo, model);

            var viewModel = new MainWindowViewModel(LoadModel, CreateEngine, () => desktop.Shutdown());

            desktop.MainWindow = new MainWindow
            {
                DataContext = viewModel,
            };
        }

        base.OnFrameworkInitializationCompleted();
    }

    private void DisableAvaloniaDataAnnotationValidation()
    {
        // Get an array of plugins to remove
        var dataValidationPluginsToRemove =
            BindingPlugins.DataValidators.OfType<DataAnnotationsValidationPlugin>().ToArray();

        // remove each entry found
        foreach (var plugin in dataValidationPluginsToRemove)
        {
            BindingPlugins.DataValidators.Remove(plugin);
        }
    }
}