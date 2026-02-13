using Spectre.Console;

var layout = new Layout("Root")
    .SplitColumns(
        new Layout("Left").Ratio(60).SplitRows(
            new Layout("TopLeft"),
            new Layout("BottomLeft").Size(3)),
        new Layout("Right").Ratio(40).SplitRows(
            new Layout("TopRight"),
            new Layout("BottomRight")
        )
    );

var roomsPanel = new Panel("[red]Rooms[/]").Expand().Header("Rooms");
var inventoryPanel = new Panel("[blue]Inventory[/]").Expand().Header("Inventory");

layout["TopLeft"].Update(
    new Panel(
        Align.Center(
            new Markup("[blue]Top Left Panel (60%)[/]"),
            VerticalAlignment.Middle))
        .Expand()
        .Header("Main Area"));

layout["BottomLeft"].Update(
    new Panel(String.Empty)
        .Expand()
        .Header("Input"));

layout["TopRight"].Update(roomsPanel);
layout["BottomRight"].Update(inventoryPanel);

/*
layout["TopRight"].Update(
    new Panel(
        Align.Center(
            new Markup("[green]Top Right Panel (20%)[/]"),
            VerticalAlignment.Middle))
        .Expand()
        .Header("Upper Status"));

layout["BottomRight"].Update(
    new Panel(
        Align.Center(
            new Markup("[yellow]Bottom Right Panel (20%)[/]"),
            VerticalAlignment.Middle))
        .Expand()
        .Header("Inventory"));
*/

while (true)
{
    AnsiConsole.Clear();
    AnsiConsole.Write(layout);

    AnsiConsole.Cursor.SetPosition(2, Console.WindowHeight - 1);
    layout["BottomLeft"].Update(new Panel(String.Empty).Expand().Header("Input"));
    var input = AnsiConsole.Ask<string>("[green]>[/]");
    if (input == "exit")
    {
        break;
    }

    /*
    layout["BottomLeft"].Update(
        new Panel(
            new Text("> " + input))
            .Expand()
            .Header("Input"));
            */
}