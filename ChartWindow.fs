module ChartWindow

open System.Drawing
open System.IO
open System.Windows
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Windows.Forms.Integration
open System.Windows.Markup

open MSDN.FSharp.Charting

open FsWpf

type private ChartWindow(title, chart:ChartTypes.GenericChart) as this =

    inherit FsUiObject<Window>(Path.Combine(__SOURCE_DIRECTORY__, "ChartWindow.xaml"))
 
    [<DefaultValue>]
    [<UiElement>]
    val mutable chartHost : WindowsFormsHost

    do
        this.UiObject.Title <- title
        let chart = chart |> FSharpChart.WithArea.AxisX(MajorGrid = Grid(LineColor = Color.LightGray))
                          |> FSharpChart.WithArea.AxisY(MajorGrid = Grid(LineColor = Color.LightGray))
                          |> FSharpChart.WithMargin(5.0f, 5.0f, 5.0f, 5.0f)
        this.chartHost.Child <- new ChartControl(chart, Dock = DockStyle.Fill)

let show title chart = 
    let chartWindow = new ChartWindow(title, chart)
    chartWindow.UiObject.Show()

let run title chart = 
    let chartWindow = new ChartWindow(title, chart)
    chartWindow.UiObject.ShowDialog() |> ignore