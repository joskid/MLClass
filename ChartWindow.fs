module ChartWindow

open System.Drawing
open System.IO
open System.Windows
open System.Windows.Forms.DataVisualization.Charting
open System.Windows.Forms.Integration
open MSDN.FSharp.Charting
open FsWpf

type ChartWindow(title, chart: ChartTypes.GenericChart) as this =

    inherit FsUiObject<Window>(Path.Combine(__SOURCE_DIRECTORY__, "ChartWindow.xaml"))

    [<DefaultValue>]
    [<UiElement>]
    val mutable chartHost : WindowsFormsHost

    let mutable chart = chart |> FSharpChart.WithArea.AxisX(MajorGrid = Grid(LineColor = Color.LightGray))
                              |> FSharpChart.WithArea.AxisY(MajorGrid = Grid(LineColor = Color.LightGray))
                              |> FSharpChart.WithMargin(5.0f, 5.0f, 5.0f, 5.0f)

    let setChart() =
        let chartControl = new ChartControl(chart)
        this.chartHost.Child <- chartControl
        chartControl

    
    let mutable chartControl = setChart()

    do
        this.UiObject.Title <- title
        setChart |> ignore

    member x.Reset(newChart) =
        chart <- newChart
        chartControl.Dispose()
        chartControl <- setChart()
        x.UiObject.Activate() |> ignore

    member x.Combine(newChart) =
        chart <- FSharpChart.Combine [chart; newChart]
        chartControl.Dispose()
        chartControl <- setChart()
        x.UiObject.Activate() |> ignore

let show title chart = 
    let chartWindow = new ChartWindow(title, chart)
    chartWindow.UiObject.Show()
    chartWindow.UiObject.Activate() |> ignore
    chartWindow

let run title chart = 
    let chartWindow = new ChartWindow(title, chart)
    chartWindow.UiObject.ShowDialog() |> ignore