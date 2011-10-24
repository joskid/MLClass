[<AutoOpen>]
module ChartExtensions

open System.Drawing
open System.Windows.Forms.DataVisualization.Charting
open MSDN.FSharp.Charting
open MSDN.FSharp.Charting.ChartTypes

let simplifyLine maxPoints points =
    let n = Array.length points
    if points.Length <= maxPoints then
        points
    else
        seq {
            for i in 0 .. (max 1 (n / maxPoints)) .. n-1 do
                yield points.[i]
        } |> Array.ofSeq

let plotIterations title functionTitle values =
    values
    |> Seq.mapi (fun i x -> (i, x))
    |> Array.ofSeq
    |> simplifyLine 200
    |> FSharpChart.Line
    |> FSharpChart.WithArea.AxisX(Minimum = 0.0, Title = "Number of iterations")
    |> FSharpChart.WithArea.AxisY(Title = functionTitle)
    |> ChartWindow.show title

let withRedCrossMarkerStyle c = c |> FSharpChart.WithSeries.Marker(MarkerStyle.Cross, Color.Red, 10)

let linspace min max (n:int) =
    [| min .. (max - min) / (float n) .. max |]

open System.Windows
open Microsoft.Research.DynamicDataDisplay
open Microsoft.Research.DynamicDataDisplay.Common.Auxiliary
open Microsoft.Research.DynamicDataDisplay.Charts
open Microsoft.Research.DynamicDataDisplay.Charts.Navigation
open Microsoft.Research.DynamicDataDisplay.DataSources.MultiDimensional

let plotContour title (xLabel, yLabel) (xMin, yMin) (xMax, yMax) f = 

    let xValues = linspace -10.0 10.0 100
    let yValues = linspace -1.0 4.0 100
    let points = Array2D.init 101 101 (fun i j -> new Point(xValues.[i], yValues.[j]))
    let fValues = Array2D.init 101 101 (fun i j -> f (xValues.[i], yValues.[j]))
    let dataSource = new WarpedDataSource2D<_>(fValues, points)

    let plotter = new ChartPlotter()
    plotter.Children.Add(new HorizontalAxisTitle(Content = xLabel))
    plotter.Children.Add(new VerticalAxisTitle(Content = yLabel))

    let isolineGraph = new IsolineGraph()
    plotter.Children.Add(isolineGraph)    

    let trackingGraph = new IsolineTrackingGraph()
    plotter.Children.Add(trackingGraph)

    plotter.Children.Add(new CursorCoordinateGraph())

    isolineGraph.DataSource <- dataSource
    trackingGraph.DataSource <- dataSource

    let visible = dataSource.GetGridBounds()
    plotter.Viewport.Visible <- visible

    let window = new Window(Width = 600.0, Height = 600.0, Content = plotter, Title = title)
    window.Show() |> ignore
